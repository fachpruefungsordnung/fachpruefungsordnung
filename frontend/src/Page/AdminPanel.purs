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

import Data.Maybe (Maybe(..))
import FPO.Data.Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB

data Action = Initialize

type State =
  { error :: Maybe String
  }

-- | Admin panel page component.
component
  :: forall query input output m
   . MonadStore Store.Action Store.Store m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState :: input -> State
  initialState _ =
    { error: Nothing
    }

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
      pure unit

renderAdminPanel :: forall w. State -> HH.HTML w Action
renderAdminPanel _ =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
            [ HH.text "Admin Panel" ]
        , HH.div [ HP.style "text-align: justify;" ]
            [ HH.text $
                "You should see an admin panel now, but it hasn't been implemented yet. "
                  <>
                    "Also, notice that this page will only be accessible to users with admin privileges. "
                  <>
                    "If you are not an admin, you will be redirected to some other page (404 or home, for example)."
            ]
        ]
    ]
