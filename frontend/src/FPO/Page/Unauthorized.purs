-- | Unauthorized (403) page.
-- |
-- | Displayed when a user tries to access a page they do not have permission
-- | to view.  Offers a button to return to the home page.

module FPO.Page.Unauthorized (component) where

import Prelude

import Data.Maybe (Maybe(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.Css as HB
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Simple.I18n.Translator (label, translate)

data Action
  = GoHome
  | Receive (Connected FPOTranslator Unit)

type State = FPOState ()

component
  :: forall query output m
   . Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component query Unit output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } = { translator: fromFpoTranslator context }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ HH.div [ HP.classes [ HB.col, HB.textCenter ] ]
          [ HH.h1 [] [ HH.text "403" ]
          , HH.h3 [ HP.classes [ HB.mb3 ] ]
              [ HH.text $ translate (label :: _ "unauthorized_title") state.translator
              ]
          , HH.p [ HP.classes [ HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "unauthorized_message")
                  state.translator
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnPrimary ]
              , HE.onClick (const GoHome)
              ]
              [ HH.text "Home" ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    GoHome -> do
      navigate Home
      pure unit
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
