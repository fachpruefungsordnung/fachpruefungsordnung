-- | Simple admin panel page. Right now, this is just a placeholder.
-- |
-- | This page is only accessible to users with admin privileges.

module FPO.Page.Page404 (component) where

import Prelude

import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

data Action = GoHome -- ^ Navigate to the home page.

-- | 404 page component.
component
  :: forall query input output m
   . Navigate m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  render :: Unit -> H.ComponentHTML Action () m
  render _ =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ HH.div [ HP.classes [ HB.col, HB.textCenter ] ]
          [ HH.h1 [] [ HH.text "404" ]
          , HH.p []
              [ HH.text
                  "Diese Seite existiert nicht, wurde verschoben, oder erfordert Privilegien, die Sie nicht besitzen."
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnPrimary ]
              , HE.onClick (const GoHome)
              ]
              [ HH.text "Startseite" ]
          ]
      ]

  handleAction :: Action -> H.HalogenM Unit Action () output m Unit
  handleAction = case _ of
    GoHome -> do
      navigate Home
      pure unit
