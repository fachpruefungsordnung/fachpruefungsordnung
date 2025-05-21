module Components.Preview where

import Prelude

import Affjax (printError)
import Affjax.StatusCode (StatusCode(StatusCode))
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import FPO.Data.Request (getIgnore, getString)
import Type.Proxy (Proxy(Proxy))
import Effect.Aff (delay, forkAff) as Aff
import FPO.Components.Button (Output(Clicked), button) as Button
import Effect.Aff.Class (liftAff) as H
import Effect.Class (liftEffect) as H
import Halogen as H
import Halogen.Component (Component, defaultEval, mkComponent, mkEval) as H
import Halogen.Themes.Bootstrap5 (bgInfoSubtle, col6, dFlex, flexColumn, flexGrow1, h100, overflowAuto, overflowHidden, textCenter, w100) as HB
import Halogen.HTML (div, div_, pre, slot, text) as HH
import Halogen.HTML.Elements (embed) as HH
import Web.HTML.Common (ClassName(ClassName)) as HH
import Halogen.HTML.Properties (classes, src) as HP
import Halogen.Subscription (create, notify) as HS

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
  }

type Input =
  { editorContent :: Maybe (Array String) }

data Query a
  = TellLoadPdf a
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
  initialState = { count: 0, dummyUser: Nothing, editorContent: Nothing, pdf: Empty, showWarning: false }

  render :: State -> H.ComponentHTML Action Slots m
  render { count, dummyUser, editorContent, pdf, showWarning } =
    case pdf of
      Empty -> HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.col6, HB.textCenter, HB.bgInfoSubtle, HB.overflowHidden ] ]
        [ HH.div_ [ HH.text "Hier sollte die Vorschau sein." ]
        , HH.div_
            [ HH.text $
                if dummyUser == Nothing then "Hier kommt nach dem Knopfdruck ein Text"
                else "Wow, nun haben wir einen dummy User geladen mit dem Namen: " <> fromMaybe "err" dummyUser
            ]
        , HH.slot _button 0 Button.button { label: show count } HandleButton
        , HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.overflowHidden ] ]
            [ case editorContent of
                Nothing ->
                  HH.text "Der Editor ist leer!"
                Just content ->
                  HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.overflowHidden ] ]
                    [ HH.text "Editorinhalt:"
                    , HH.div
                        [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HH.ClassName "mt-1", HB.overflowAuto ] ]
                        [ HH.pre
                            [ HP.classes [ HB.flexGrow1, HH.ClassName "border rounded p-1 bg-light", HB.h100 ] ]
                            ( content <#> \line ->
                                HH.div_ [ HH.text $ preserveEmptyLine line ]
                            )
                        ]
                    ]
            ]
        ]
      AskedButError reason -> HH.div [ HP.classes [ HB.col6, HB.textCenter, HB.bgInfoSubtle ] ]
        if showWarning then [ HH.text reason ]
        else [ HH.embed [ HP.src "/api/document", HP.classes [ HB.w100, HB.h100 ] ] [] ]
      PdfAvailable -> HH.div [ HP.classes [ HB.col6, HB.textCenter, HB.bgInfoSubtle ] ]
        [ HH.embed [ HP.src "/api/document", HP.classes [ HB.w100, HB.h100 ] ] [] ]

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

    ShowOrHideWarning -> H.modify_ \state -> state { showWarning = not state.showWarning }

    Receive { editorContent } -> H.modify_ \state -> state { editorContent = editorContent }

  handleQuery :: forall a m. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    TellLoadPdf a -> do
      response <- H.liftAff $ getIgnore "/document"
      case response of
        Right { body, headers, status, statusText } -> do
          if status == (StatusCode 201) then H.modify_ _ { pdf = PdfAvailable }
          else H.modify_ _
            { pdf = AskedButError
                ( "Could not load pdf properly. Here should be a detailed warning message"
                    <> "in the future that returned from the compiled pdf. This is just a dummy pdf for now"
                )
            }
        Left err -> do
          H.liftEffect $ log $ printError err
          H.modify_ _ { pdf = AskedButError "Error loading PDF." }
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
