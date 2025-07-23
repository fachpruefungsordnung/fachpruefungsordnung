module FPO.Components.Preview where

import Prelude

import Affjax (printError)
import Affjax.StatusCode (StatusCode(StatusCode))
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import FPO.Components.Button (Output(Clicked), button) as Button
import FPO.Data.Request (getIgnore, getString)
import Halogen as H
import Halogen.HTML (div, div_, pre, slot, text) as HH
import Halogen.HTML.Elements (embed)
import Halogen.HTML.Properties (classes, src) as HP
import Halogen.Subscription (create, notify) as HS
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))

type Output = Unit

data Action
  = HandleButton Button.Output
  | Initialize
  | Increment
  | LoadPdf PdfState
  | ShowOrHideWarning
  | Receive Input

type Slots = (button :: forall query. H.Slot query Button.Output Int)

_button = Proxy :: Proxy "button"

type State =
  { count :: Int
  , dummyUser :: Maybe String
  , editorContent :: Maybe (Array String)
  , pdf :: PdfState
  , showWarning :: Boolean
  , pdfURL :: Maybe String
  }

type Input =
  { editorContent :: Maybe (Array String) }

data Query a
  = TellLoadPdf a
  | GotEditorQuery (Maybe (Array String)) a
  | TellShowOrHideWarning a
  | TellClickedHttpRequest a

data PdfState
  = Empty
  | AskedButError String -- to load a default PDF
  | PdfAvailable

preview :: forall m. MonadAff m => H.Component Query Input Output m
preview = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: State
  initialState =
    { count: 0
    , dummyUser: Nothing
    , editorContent: Nothing
    , pdf: Empty
    , showWarning: false
    , pdfURL: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render { count, dummyUser, editorContent, pdf, showWarning, pdfURL } =
    case pdf of
      Empty -> HH.div
        [ HP.classes
            [ HB.dFlex
            , HB.flexColumn
            , HB.flexGrow1
            , HB.textCenter
            , HB.bgInfoSubtle
            , HB.overflowHidden
            ]
        ]
        [ HH.div_ [ HH.text "Preview:" ]
        , HH.div_
            [ HH.text $
                if dummyUser == Nothing then
                  "Press the HTTP request button to load a dummy user"
                else "Wow, we have successfully loaded a dummy user with name: " <>
                  fromMaybe "err" dummyUser
            ]
        , HH.slot _button 0 Button.button { label: show count } HandleButton
        , HH.div
            [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.overflowHidden ]
            ]
            [ case editorContent of
                Nothing ->
                  HH.text "The editor has no content!"
                Just content ->
                  HH.div
                    [ HP.classes
                        [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.overflowHidden ]
                    ]
                    [ HH.text "Editor Content:"
                    , HH.div
                        [ HP.classes
                            [ HB.dFlex
                            , HB.flexColumn
                            , HB.flexGrow1
                            , H.ClassName "mt-1"
                            , HB.overflowAuto
                            ]
                        ]
                        [ HH.pre
                            [ HP.classes
                                [ HB.flexGrow1
                                , H.ClassName "border rounded p-1 bg-light"
                                , HB.h100
                                ]
                            ]
                            ( content <#> \line ->
                                HH.div_ [ HH.text $ preserveEmptyLine line ]
                            )
                        ]
                    ]
            ]
        ]
      AskedButError reason -> HH.div
        [ HP.classes
            [ HB.dFlex
            , HB.flexColumn
            , HB.flexGrow1
            , HB.textCenter
            , HB.bgInfoSubtle
            , HB.overflowHidden
            ]
        ]
        if showWarning then [ HH.text reason ]
        else [ embed [ HP.src "/api/document", HP.classes [ HB.w100, HB.h100 ] ] [] ]
      PdfAvailable -> HH.div
        [ HP.classes
            [ HB.dFlex
            , HB.flexColumn
            , HB.flexGrow1
            , HB.textCenter
            , HB.bgInfoSubtle
            , HB.overflowHidden
            ]
        ]
        [ case pdfURL of
            Nothing -> embed
              [ HP.src "/api/document", HP.classes [ HB.w100, HB.h100 ] ]
              []
            Just url -> embed [ HP.src url, HP.classes [ HB.w100, HB.h100 ] ] []
        ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots Unit m Unit
  handleAction = case _ of
    HandleButton output -> case output of
      Button.Clicked -> H.modify_ \state -> state { count = 0 }

    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter
      void
        $ H.liftAff
        $ Aff.forkAff
        $ forever do
            Aff.delay $ Milliseconds 1000.0
            H.liftEffect $ HS.notify listener Increment

    Increment -> H.modify_ \state -> state { count = state.count + 1 }

    LoadPdf pdfState -> H.modify_ \state -> state { pdf = pdfState }

    ShowOrHideWarning -> H.modify_ \state -> state
      { showWarning = not state.showWarning }

    Receive { editorContent } -> H.modify_ _
      { editorContent = editorContent }

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    TellLoadPdf a -> do
      response <- H.liftAff $ getIgnore "/document"
      case response of
        Right { status } -> do
          if status == (StatusCode 201) then H.modify_ _ { pdf = PdfAvailable }
          else H.modify_ _
            { pdf = AskedButError
                ( "Could not load pdf properly. Here should be a detailed warning message"
                    <>
                      "in the future that returned from the compiled pdf. This is just a dummy pdf for now"
                )
            }
        Left err -> do
          H.liftEffect $ log $ printError err
          H.modify_ _ { pdf = AskedButError "Error loading PDF." }
      pure (Just a)

    GotEditorQuery mEditorContent a -> do
      H.modify_ \state ->
        state { editorContent = mEditorContent }
      pure (Just a)

    TellShowOrHideWarning a -> do
      H.modify_ \state -> state { showWarning = not state.showWarning }
      pure (Just a)

    TellClickedHttpRequest a -> do
      response <- H.liftAff $ getString "/users"
      case response of
        Right { body } ->
          H.modify_ _ { dummyUser = Just body }
        Left _ -> do
          H.modify_ _ { dummyUser = Nothing }
      pure (Just a)

  -- Forces the string to be at least one character long.
  preserveEmptyLine :: String -> String
  preserveEmptyLine str = if str == "" then " " else str
