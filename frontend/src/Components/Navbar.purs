-- | Navbar component for the application.
-- | It contains links to different pages and a brand name.
-- | 
-- | This also serves as a guide for how to implement navigation in this application
-- | using the Navigate type class. Refer to the implementations of `render` and `handleAction`
-- | for more details on how to use the `Navigate` class.

module FPO.Components.Navbar where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import FPO.Data.Store (User)
import FPO.Data.Store as Store
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes) as HP
import Halogen.HTML.Properties (style)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 (bgBodyTertiary, btn, btnLink, containerFluid, dropdown, dropdownItem, dropdownMenu, dropdownToggle, meAuto, msAuto, navItem, navLink, navbar, navbarBrand, navbarCollapse, navbarExpandSm, navbarNav) as HB

type State = { user :: Maybe User, dropdownOpen :: Boolean }

data Action
  = Navigate Route
  | Receive (Connected Store.Store Unit) -- Receive store updates
  | ToggleDropdown
  | Logout

-- | The navbar component that renders the navigation bar.
-- |
-- | It subscribes to store changes to update the user information.
navbar
  :: forall query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component query Unit output m
navbar = connect (selectEq identity) $ H.mkComponent
  { initialState: \{ context: store } -> { user: store.user, dropdownOpen: false }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render state = HH.nav [ HP.classes [ HB.navbar, HB.navbarExpandSm, HB.bgBodyTertiary ] ]
    [ HH.div [ HP.classes [ HB.containerFluid ] ]
        [ HH.a [ HP.classes [ HB.navbarBrand ], HE.onClick (const $ Navigate Home), style "cursor: pointer" ] [ HH.text "FPO-Editor" ]
        , HH.div [ HP.classes [ HB.navbarCollapse ] ]
            [ HH.ul [ HP.classes [ HB.navbarNav, HB.meAuto ] ]
                [ HH.li [ HP.classes [ HB.navItem ] ]
                    [ navButton "Home" Home ]
                ]
            -- Right side of the navbar
            , HH.ul [ HP.classes [ HB.navbarNav, HB.msAuto ] ]
                [ HH.li [ HP.classes [ HB.navItem ] ]
                    [ case state.user of
                        Nothing -> navButton "Login" Login
                        Just user -> userDropdown user.userName state.dropdownOpen
                    ]
                ]
            ]
        ]
    ]

  handleAction
    :: forall slots
     . Action
    -> H.HalogenM State Action slots output m Unit
  handleAction (Navigate route) = do
    navigate route
  handleAction (Receive { context: store }) = do
    H.modify_ _ { user = store.user }
  handleAction ToggleDropdown = do
    H.modify_ \s -> s { dropdownOpen = not s.dropdownOpen }
  handleAction Logout = do
    H.modify_ _ { dropdownOpen = false }
    -- TODO: Perform logout logic, e.g., clear user session, redirect to login.
    -- Notice how we do not have to change this component's user state here, because
    -- updating the store will trigger a re-evaluation of the navbar component.
    updateStore (Store.SetUser Nothing)

  -- Creates a navigation button
  navButton :: String -> Route -> H.ComponentHTML Action () m
  navButton label route =
    HH.button
      [ HP.classes [ HB.navLink, HB.btn, HB.btnLink ]
      , HE.onClick (const $ Navigate route)
      ]
      [ HH.text label ]

  -- Creates a user dropdown with user icon and logout option
  userDropdown :: String -> Boolean -> H.ComponentHTML Action () m
  userDropdown username dropdownOpen =
    HH.div [ HP.classes [ HB.dropdown ] ]
      [ HH.button
          [ HP.classes [ HB.btn, HB.btnLink, HB.navLink, HB.dropdownToggle ]
          , HE.onClick (const ToggleDropdown)
          ]
          [ HH.i [ HP.classes [ ClassName "bi", ClassName "bi-person" ] ] []
          , HH.text username
          ]
      , HH.ul
          [ HP.classes [ HB.dropdownMenu ]
          , style $ if dropdownOpen then "display: block;" else "display: none;"
          ]
          [ HH.li_
              [ HH.span
                  [ HP.classes [ HB.dropdownItem ]
                  , style "cursor: default;"
                  , HE.onClick (const Logout)
                  ]
                  [ HH.text "Logout" ]
              ]
          ]
      ]