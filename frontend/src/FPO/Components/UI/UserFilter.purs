-- | Simple card / form box for filtering users.

module FPO.Components.UI.UserFilter
  ( Output(..)
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addButton, addCard, addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (InputType(..), classes) as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

type State = FPOState (username :: String, email :: String)

data Action
  = SetUsername String
  | SetEmail String
  | HandleFilter
  | Receive (Connected FPOTranslator Unit)

data Output = Filter { username :: String, email :: String }

component
  :: forall m q
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component q Unit Output m
component = connect selectTranslator $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { username: ""
    , email: ""
    , translator: fromFpoTranslator context
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    addCard (translate (label :: _ "common_filterBy") state.translator)
      [ HP.classes [ HB.col12, HB.colMd3, HB.colLg3, HB.mb3 ] ] $ HH.div
      [ HP.classes [ HB.row ] ]
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.username
              (translate (label :: _ "common_userName") state.translator)
              (translate (label :: _ "common_userName") state.translator)
              "bi-person"
              false
              HP.InputText
              SetUsername
          , addColumn
              state.email
              (translate (label :: _ "common_email") state.translator)
              (translate (label :: _ "common_email") state.translator)
              "bi-envelope-fill"
              false
              HP.InputEmail
              SetEmail
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  true
                  "Filter"
                  (Just "bi-funnel")
                  (const HandleFilter)
              ]
          ]
      ]

  handleAction
    :: forall slots
     . Action
    -> H.HalogenM State Action slots Output m Unit
  handleAction action = case action of
    SetUsername u -> do
      H.modify_ _ { username = u }
    SetEmail e -> do
      H.modify_ _ { email = e }
    HandleFilter -> do
      state <- H.get
      H.raise (Filter { username: state.username, email: state.email })
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
