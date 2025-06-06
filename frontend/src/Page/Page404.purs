-- | Simple admin panel page. Right now, this is just a placeholder.
-- |
-- | This page is only accessible to users with admin privileges.

module FPO.Page.Page404 (component) where

import Prelude

import Data.Maybe (Maybe(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

data Action
  = GoHome -- ^ Navigate to the home page.
  | Receive (Connected FPOTranslator Unit)

type State = FPOState ()

-- | 404 page component.
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
          [ HH.h1 [] [ HH.text "404" ]
          , HH.p []
              [ HH.text $ translate (label :: _ "p404_notFound") state.translator
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
