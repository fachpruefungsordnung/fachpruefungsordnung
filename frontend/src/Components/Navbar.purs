-- | Navbar component for the application.
-- | It contains links to different pages and a brand name.
-- | 
-- | This also serves as a guide for how to implement navigation in this application
-- | using the Navigate type class. Refer to the implementations of `render` and `handleAction`
-- | for more details on how to use the `Navigate` class.

module FPO.Components.Navbar where

import Prelude

import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes) as HP
import Halogen.HTML.Properties (style)
import Halogen.Themes.Bootstrap5
  ( bgBodyTertiary
  , btn
  , btnLink
  , containerFluid
  , meAuto
  , navItem
  , navLink
  , navbar
  , navbarBrand
  , navbarCollapse
  , navbarExpandSm
  , navbarNav
  ) as HB

data Action = Navigate Route

navbar :: forall query input output m. Navigate m => H.Component query input output m
navbar = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }
  where
  render :: Unit -> H.ComponentHTML Action () m
  render _ = HH.nav [ HP.classes [ HB.navbar, HB.navbarExpandSm, HB.bgBodyTertiary ] ]
    [ HH.div [ HP.classes [ HB.containerFluid ] ]
        [ HH.a [ HP.classes [ HB.navbarBrand ], HE.onClick (const $ Navigate Home), style "cursor: pointer" ] [ HH.text "FPO-Editor" ]
        , HH.div [ HP.classes [ HB.navbarCollapse ] ]
            [ HH.ul [ HP.classes [ HB.navbarNav, HB.meAuto ] ]
                [ HH.li [ HP.classes [ HB.navItem ] ]
                    [ navButton "Home" Home ]
                , HH.li [ HP.classes [ HB.navItem ] ]
                    [ navButton "Login" Login ]
                ]
            ]
        ]
    ]

  -- Handles the navigation action
  handleAction :: forall s. Action -> H.HalogenM Unit Action s output m Unit
  handleAction (Navigate route) = do
    navigate route
    pure unit

  -- Creates a navigation button
  navButton :: String -> Route -> H.ComponentHTML Action () m
  navButton label route =
    HH.button
      [ HP.classes [ HB.navLink, HB.btn, HB.btnLink ]
      , HE.onClick (const $ Navigate route)
      ]
      [ HH.text label ]