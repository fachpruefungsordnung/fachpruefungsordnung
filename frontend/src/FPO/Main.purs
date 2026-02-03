-- | This module defines the main entry point of the application and manages
-- | the high-level structure of the app.
-- |
-- | It implements `Main.component`, the root Halogen component responsible for
-- | rendering the appropriate page based on the current route. Additionally,
-- | it includes global components that persist across multiple pages, such as the navbar.

module Main where

import Data.Either (hush)
import Data.Function (const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import FPO.AppM (runAppM)
import FPO.Components.AppToasts as AppToasts
import FPO.Components.Navbar as Navbar
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..), routeCodec)
import FPO.Data.Store (loadLanguage)
import FPO.Data.Store as Store
import FPO.Page.Admin.Administration as Administration
import FPO.Page.Admin.CreateGroup as CreateGroup
import FPO.Page.Admin.CreateUser as CreateUser
import FPO.Page.Admin.Group.AddMembers as GroupAddMembers
import FPO.Page.Admin.Group.DocOverview as ViewGroupDocuments
import FPO.Page.Admin.Group.MemberOverview as ViewGroupMembers
import FPO.Page.EditorPage as EditorPage
import FPO.Page.Home as Home
import FPO.Page.Login as Login
import FPO.Page.Page404 as Page404
import FPO.Page.Profile as Profile
import FPO.Page.ResetPassword as PasswordReset
import FPO.Translations.Translator
  ( FPOTranslator(..)
  , detectBrowserLanguage
  , getTranslatorForLanguage
  )
import FPO.UI.Style as Style
import FPO.UI.Truncated (setupTruncationListener)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Halogen.VDom.Driver (runUI)
import Prelude
  ( Unit
  , Void
  , bind
  , discard
  , pure
  , unit
  , void
  , when
  , ($)
  , (/=)
  , (<$>)
  , (<<<)
  , (==)
  )
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Router and Main Page

type State = { route :: Maybe Route }

data Query a = NavigateQ Route a -- ^ Query to navigate to a new route.

data Action
  = Initialize -- ^ Action to initialize the main component.
  | HandleProfile Profile.Output

_navbar = Proxy :: Proxy "navbar"
_home = Proxy :: Proxy "home"
_editor = Proxy :: Proxy "editor"
_login = Proxy :: Proxy "login"
_resetPassword = Proxy :: Proxy "resetPassword"
_administration = Proxy :: Proxy "administration"
_createUser = Proxy :: Proxy "createUser"
_createGroup = Proxy :: Proxy "createGroup"
_viewGroupDocuments = Proxy :: Proxy "viewGroupDocuments"
_viewGroupMembers = Proxy :: Proxy "viewGroupMembers"
_groupAddMembers = Proxy :: Proxy "groupAddMembers"
_page404 = Proxy :: Proxy "page404"
_profile = Proxy :: Proxy "profile"
_appToasts = Proxy :: Proxy "appToasts"

type Slots =
  ( home :: forall q. H.Slot q Void Unit
  , editor :: forall q. H.Slot q Unit Unit
  , login :: forall q. H.Slot q Void Unit
  , navbar :: H.Slot Navbar.Query Void Unit
  , resetPassword :: forall q. H.Slot q Void Unit
  , administration :: forall q. H.Slot q Void Unit
  , createUser :: forall q. H.Slot q Void Unit
  , createGroup :: forall q. H.Slot q Void Unit
  , viewGroupDocuments :: forall q. H.Slot q Void Unit
  , viewGroupMembers :: forall q. H.Slot q Void Unit
  , groupAddMembers :: forall q. H.Slot q Void Unit
  , page404 :: forall q. H.Slot q Void Unit
  , profile :: forall q. H.Slot q Profile.Output Unit
  , appToasts :: forall q. H.Slot q Void Unit
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
  render state = HH.div
    [ HP.classes
        [ HB.dFlex
        , HB.flexColumn
        , HB.vh100
        , HB.p0
        , HB.overflowYAuto
        , HB.overflowXHidden
        , Style.disableScrollbar
        ]
    ]
    [ HH.slot_ _navbar unit Navbar.navbar unit
    , HH.slot_ _appToasts unit AppToasts.component unit
    , case state.route of
        Nothing -> HH.slot_ _page404 unit Page404.component unit
        Just p -> case p of
          Home -> HH.slot_ _home unit Home.component unit
          Editor { docID } -> HH.slot_ _editor unit EditorPage.component docID
          Login -> HH.slot_ _login unit Login.component unit
          PasswordReset { token } -> HH.slot_ _resetPassword unit
            PasswordReset.component
            { token }
          Administration { tab } -> HH.slot_ _administration unit
            Administration.component
            { tab }
          CreateUser -> HH.slot_ _createUser unit CreateUser.component unit
          CreateGroup -> HH.slot_ _createGroup unit CreateGroup.component unit
          ViewGroupDocuments { groupID } -> HH.slot_ _viewGroupDocuments unit
            ViewGroupDocuments.component
            groupID
          ViewGroupMembers { groupID } -> HH.slot_ _viewGroupMembers unit
            ViewGroupMembers.component
            groupID
          GroupAddMembers { groupID } -> HH.slot_ _groupAddMembers unit
            GroupAddMembers.component
            groupID
          Page404 -> HH.slot_ _page404 unit Page404.component unit
          Profile { loginSuccessful, userId } -> HH.slot _profile unit
            Profile.component
            { loginSuccessfulBanner: loginSuccessful, userId: userId }
            HandleProfile
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute
    HandleProfile profileOutput -> case profileOutput of
      Profile.ChangedUsername -> do
        -- If the username was changed, we want to reload the user data
        -- to ensure that the navbar displays the correct user information.
        H.tell _navbar unit Navbar.RequestReloadUser

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Void m (Maybe a)
  handleQuery = case _ of
    NavigateQ dest a -> do
      -- Here, we do not check for user credentials before navigating.
      -- Each page (e.g., profile or admin panel) should handle its own access control.
      -- If the user is not logged in, they will be redirected to the login page or
      -- some other sensible page (e.g., home or 404 page).
      -- This is reasonable because each specific backend API call will check
      -- whether the user is logged in or not (and has sufficient access rights), and
      -- the frontend should then handle the response accordingly - for each possible
      -- page and action.

      route <- H.gets _.route
      -- If the user came from the login page, we want to reload the user data
      -- to ensure that the navbar displays the correct user information.
      when (route == Just Login) $ do
        H.tell _navbar unit Navbar.RequestReloadUser

      H.modify_ _ { route = Just dest }
      updateStore $ Store.SetCurrentRoute (Just dest)

      pure $ Just a

--------------------------------------------------------------------------------
-- Main

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  savedLang <- H.liftEffect loadLanguage
  browserLang <- H.liftEffect detectBrowserLanguage
  let defaultLang = fromMaybe browserLang savedLang :: String
  let translator = getTranslatorForLanguage defaultLang
  let
    initialStore =
      { inputMail: ""
      , loginRedirect: Nothing
      , currentRoute: Nothing
      , translator: FPOTranslator translator
      , language: defaultLang
      , toasts: []
      , errorCooldowns: Map.empty
      , totalToasts: 0
      , handleRequestError: true
      } :: Store.Store
  rootComponent <- runAppM initialStore component
  halogenIO <- runUI rootComponent unit body

  -- Set up a listener for hash changes and update the route accordingly.
  void $ liftEffect $ matchesWith (RD.parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      _response <- halogenIO.query $ H.mkTell $ NavigateQ new
      pure unit

  void $ liftEffect setupTruncationListener
