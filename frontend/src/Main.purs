-- | This module defines the main entry point of the application and manages
-- | the high-level structure of the app.
-- |
-- | It implements `Main.component`, the root Halogen component responsible for
-- | rendering the appropriate page based on the current route. Additionally,
-- | it includes global components that persist across multiple pages, such as the navbar.

module Main where

import Data.Maybe

import Data.Either (hush)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import FPO.AppM (runAppM)
import FPO.Components.Navbar as Navbar
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..), routeCodec, routeToString)
import FPO.Data.Store as Store
import FPO.Page.AdminPanel as AdminPanel
import FPO.Page.EditorPage as EditorPage
import FPO.Page.Home as Home
import FPO.Page.Login as Login
import FPO.Page.Page404 as Page404
import FPO.Page.Profile as Profile
import FPO.Page.ResetPassword as PasswordReset
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Themes.Bootstrap5 as HB
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, Void, bind, const, discard, pure, unit, void, when, ($), (/=), (<$>), (<<<), (<>), (||))
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Router and Main Page

type State = { route :: Maybe Route }

data Query a = NavigateQ Route a -- ^ Query to navigate to a new route.

data Action = Initialize -- ^ Action to initialize the main component.

_navbar = Proxy :: Proxy "navbar"
_home = Proxy :: Proxy "home"
_editor = Proxy :: Proxy "editor"
_login = Proxy :: Proxy "login"
_resetPassword = Proxy :: Proxy "resetPassword"
_adminPanel = Proxy :: Proxy "adminPanel"
_page404 = Proxy :: Proxy "page404"
_profile = Proxy :: Proxy "profile"

type Slots =
  ( home :: forall q. H.Slot q Void Unit
  , editor :: forall q. H.Slot q Unit Unit
  , login :: forall q. H.Slot q Void Unit
  , navbar :: forall q. H.Slot q Void Unit
  , resetPassword :: forall q. H.Slot q Void Unit
  , adminPanel :: forall q. H.Slot q Void Unit
  , page404 :: forall q. H.Slot q Void Unit
  , profile :: forall q. H.Slot q Void Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component Query Unit Void m
component =
  H.mkComponent
    { initialState: const { route: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , handleQuery = handleQuery
        }
    }
  where
  render :: State -> H.ComponentHTML Action Slots m
  render state = HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.vh100, HB.p0, HB.overflowHidden ] ]
    [ HH.slot_ _navbar unit Navbar.navbar unit
    , case state.route of
        Nothing -> HH.slot_ _page404 unit Page404.component unit
        Just p -> case p of
          Home -> HH.slot_ _home unit Home.component unit
          Editor -> HH.slot_ _editor unit EditorPage.component unit
          Login -> HH.slot_ _login unit Login.component unit
          PasswordReset -> HH.slot_ _resetPassword unit PasswordReset.component unit
          AdminPanel -> HH.slot_ _adminPanel unit AdminPanel.component unit
          Profile { loginSuccessful } -> HH.slot_ _profile unit Profile.component { loginSuccessfulBanner: loginSuccessful }
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Void m (Maybe a)
  handleQuery = case _ of
    NavigateQ dest a -> do
      -- Before navigating, check if the user is allowed to access the destination.
      -- If the user is not an admin, restrict access to the AdminPanel.
      -- This might not be a scalable solution for larger applications, and we might
      -- want to implement a more robust permission system in the future.
      --
      -- We could also allow the user to access any page they want, but each page would
      -- check if the user is allowed to access it and display an error message if not.
      admin <- maybe false _.isAdmin <$> _.user <$> getStore
      let allowed = admin || dest /= AdminPanel

      H.modify_ _ { route = if allowed then Just dest else Nothing }
      pure $ Just a

--------------------------------------------------------------------------------
-- Main

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let initialStore = { inputMail: "", user: Nothing } :: Store.Store
  rootComponent <- runAppM initialStore component
  halogenIO <- runUI rootComponent unit body

  -- Set up a listener for hash changes and update the route accordingly.
  void $ liftEffect $ matchesWith (RD.parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      _response <- halogenIO.query $ H.mkTell $ NavigateQ new
      log $ "Navigated to: " <> routeToString new
      pure unit
