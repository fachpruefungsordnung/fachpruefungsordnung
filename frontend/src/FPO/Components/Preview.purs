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
import FPO.Data.Request (getIgnore)
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
  | Receive Input

type Slots = (button :: forall query. H.Slot query Button.Output Int)

_button = Proxy :: Proxy "button"

type State =
  { count :: Int
  , editorContent :: Maybe (Array String)
  }

type Input =
  { editorContent :: Maybe (Array String) }

data Query a
  = GotEditorQuery (Maybe (Array String)) a

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
    , editorContent: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render { count, editorContent } =
    HH.div
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

    Receive { editorContent } -> H.modify_ \state -> state
      { editorContent = editorContent }

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of

    GotEditorQuery mEditorContent a -> do
      H.modify_ \state ->
        state { editorContent = mEditorContent }
      pure (Just a)

  -- Forces the string to be at least one character long.
  preserveEmptyLine :: String -> String
  preserveEmptyLine str = if str == "" then " " else str
