-- | Simple admin panel page. Right now, this is just a placeholder.
-- |
-- | This page is only accessible to users with admin privileges.
-- |
-- | TODO: Implement the actual admin panel functionality, see mockups.
-- |       For a start, it should be enough to connect with the backend
-- |       and check if we're even allowed to access this page, then
-- |       handle the response accordingly.

module FPO.Page.AdminPanel (component) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)

type State = FPOState (error :: Maybe String)

-- | Admin panel page component.
component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Unit output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } = { translator: fromFpoTranslator context, error: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ renderAdminPanel state
      , HH.div [ HP.classes [ HB.textCenter ] ]
          [ case state.error of
              Just err -> HH.div [ HP.classes [ HB.alert, HB.alertDanger ] ]
                [ HH.text err ]
              Nothing -> HH.text ""
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      -- TODO: Usually, we would fetch some data here (and handle
      --       the error of missing credentials), but for now,
      --       we just check if the user is an admin and redirect
      --       to a 404 page if not.
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $
        navigate Page404
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

renderAdminPanel :: forall w. State -> HH.HTML w Action
renderAdminPanel state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
            [ HH.text $ translate (label :: _ "ap_adminPanel") state.translator ]
        , HH.div [ HP.style "text-align: justify;" ]
            [ HH.text $
                "You should see an admin panel now, but it hasn't been implemented yet. "
                  <>
                    "Also, notice that this page will only be accessible to users with admin privileges. "
                  <>
                    "If you are not an admin, should have been redirected to some other page (404 or home, for example)."
            ]
        ]
    ]
