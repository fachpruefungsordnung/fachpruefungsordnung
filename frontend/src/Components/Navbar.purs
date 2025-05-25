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
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (attr, classes, style) as HP
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB

type State = { user :: Maybe User }

data Action
  = Navigate Route
  | Receive (Connected Store.Store Unit) -- Receive store updates
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
  { initialState: \{ context: store } -> { user: store.user }
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
        [ HH.a
            [ HP.classes [ HB.navbarBrand ]
            , HE.onClick (const $ Navigate Home)
            , HP.style "cursor: pointer"
            ]
            [ HH.text "FPO-Editor" ]
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
                        Just user -> userDropdown user.userName
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
  handleAction Logout = do
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
  userDropdown :: String -> H.ComponentHTML Action () m
  userDropdown username =
    HH.li
      [ HP.classes [ HB.navItem, HB.dropdown ] ]
      [ HH.a
          [ HP.classes [ HB.navLink, HB.dropdownToggle ]
          , role "button"
          , HP.attr (AttrName "data-bs-toggle") "dropdown"
          , HP.attr (AttrName "aria-expanded") "false"
          ]
          [ HH.i [ HP.classes [ ClassName "bi-person", HB.me1 ] ] []
          , HH.text username
          ]
      , HH.ul
          [ HP.classes [ HB.dropdownMenu ]
          , HP.attr (AttrName "aria-labelledby") "navbarDarkDropdownMenuLink"
          ]
          [ HH.li_
              [ HH.span
                  [ HP.classes [ HB.dropdownItem ]
                  , HP.style "cursor: default;"
                  , HE.onClick (const Logout)
                  ]
                  [ HH.text "Logout" ]
              ]
          ]
      ]