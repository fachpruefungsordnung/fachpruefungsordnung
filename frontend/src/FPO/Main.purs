-- | This module defines the main entry point of the application and manages
-- | the high-level structure of the app.
-- |
-- | It implements `Main.component`, the root Halogen component responsible for
-- | rendering the appropriate page based on the current route. Additionally,
-- | it includes global components that persist across multiple pages, such as the navbar.

module Main where

import Data.Function (const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import FPO.AppM (runAppM)
import FPO.Components.AppToasts as AppToasts
import FPO.Components.Navbar as Navbar
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Route
  ( GroupSubRoute(..)
  , Route(..)
  , currentPath
  , isLoginRoute
  , parseRoute
  )
import FPO.Data.Store (loadLanguage)
import FPO.Data.Store as Store
import FPO.Page.Admin.Administration as Administration
import FPO.Page.Admin.CreateGroup as CreateGroup
import FPO.Page.Admin.CreateUser as CreateUser
import FPO.Page.Admin.GroupOverview as GroupOverview
import FPO.Page.EditorPage as EditorPage
import FPO.Page.Home as Home
import FPO.Page.Login as Login
import FPO.Page.Page404 as Page404
import FPO.Page.Profile as Profile
import FPO.Page.ResetPassword as PasswordReset
import FPO.Page.Unauthorized as Unauthorized
import FPO.Translations.Translator
  ( FPOTranslator(..)
  , detectBrowserLanguage
  , getTranslatorForLanguage
  )
import FPO.UI.Css as HB
import FPO.UI.Style as Style
import FPO.UI.Truncated (setupTruncationListener)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
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
  , (<<<)
  , (==)
  )
import Routing.PushState as PushState
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
_groupOverview = Proxy :: Proxy "groupOverview"
_page404 = Proxy :: Proxy "page404"
_unauthorized = Proxy :: Proxy "unauthorized"
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
  , groupOverview :: forall q. H.Slot q Void Unit
  , page404 :: forall q. H.Slot q Void Unit
  , unauthorized :: forall q. H.Slot q Void Unit
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
    [ HH.div [ HP.classes [ H.ClassName "fpo-global-bg" ] ]
        [ HH.div [ HP.classes [ H.ClassName "fpo-global-bg__noise" ] ] []
        , HH.div
            [ HP.classes
                [ H.ClassName "fpo-global-bg__orb"
                , H.ClassName "fpo-global-bg__orb--1"
                ]
            ]
            []
        , HH.div
            [ HP.classes
                [ H.ClassName "fpo-global-bg__orb"
                , H.ClassName "fpo-global-bg__orb--2"
                ]
            ]
            []
        , HH.div
            [ HP.classes
                [ H.ClassName "fpo-global-bg__orb"
                , H.ClassName "fpo-global-bg__orb--3"
                ]
            ]
            []
        , HH.div
            [ HP.classes
                [ H.ClassName "fpo-global-bg__orb"
                , H.ClassName "fpo-global-bg__orb--4"
                ]
            ]
            []
        ]
    , HH.slot_ _navbar unit Navbar.navbar unit
    , HH.slot_ _appToasts unit AppToasts.component unit
    , case state.route of
        Nothing -> HH.div_ [] -- Loading: route not yet resolved
        Just p -> case p of
          Home -> HH.slot_ _home unit Home.component unit
          Editor docID params -> HH.slot_ _editor unit EditorPage.component
            { docID, params }
          Login { redirect } -> HH.slot_ _login unit Login.component
            { redirect }
          PasswordReset { token } -> HH.slot_ _resetPassword unit
            PasswordReset.component
            { token }
          AdminUsers -> HH.slot_ _administration unit
            Administration.component
            { tab: Nothing }
          CreateUser -> HH.slot_ _createUser unit CreateUser.component unit
          AdminGroups -> HH.slot_ _administration unit
            Administration.component
            { tab: Just "groups" }
          CreateGroup -> HH.slot_ _createGroup unit CreateGroup.component unit
          GroupRoute groupID GroupDocuments -> HH.slot_ _groupOverview unit
            GroupOverview.component
            { groupID, tab: Nothing }
          GroupRoute groupID GroupMembers -> HH.slot_ _groupOverview unit
            GroupOverview.component
            { groupID, tab: Just "members" }
          GroupRoute groupID GroupSettings -> HH.slot_ _groupOverview unit
            GroupOverview.component
            { groupID, tab: Just "settings" }
          Page404 -> HH.slot_ _page404 unit Page404.component unit
          Unauthorized -> HH.slot_ _unauthorized unit Unauthorized.component unit
          Profile -> HH.slot _profile unit
            Profile.component
            { userId: Nothing }
            HandleProfile
          UserProfile userId -> HH.slot _profile unit
            Profile.component
            { userId: Just userId }
            HandleProfile
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Void m Unit
  handleAction = case _ of
    Initialize -> do
      path <- liftEffect currentPath
      let
        parsedRoute = parseRoute path
      -- Set route and currentRoute immediately so that handleAppError (called
      -- from child-component API failures) can see the correct "from" route.
      -- We intentionally do NOT call `navigate` here — the browser URL is
      -- already at `parsedRoute`, so pushing the same URL would only create
      -- a redundant history entry and, more importantly, trigger the
      -- `matchesWith` listener which would queue a `NavigateQ` that races
      -- with any auth-error redirect produced by child components during
      -- initialisation.
      H.modify_ _ { route = Just parsedRoute }
      updateStore $ Store.SetCurrentRoute (Just parsedRoute)
    HandleProfile profileOutput -> case profileOutput of
      Profile.ChangedUsername -> do
        -- If the username was changed, we want to reload the user data
        -- to ensure that the navbar displays the correct user information.
        H.tell _navbar unit Navbar.RequestReloadUser

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Void m (Maybe a)
  handleQuery = case _ of
    NavigateQ dest a -> do
      -- Validate against the live browser URL.  The `matchesWith` listener
      -- can deliver NavigateQ messages out of order (e.g. an initial-load
      -- NavigateQ for the protected page arriving *after* handleAppError
      -- already pushed /login?redirect=…).  By checking the browser URL we
      -- discard any stale NavigateQ whose route no longer matches reality,
      -- keeping the view and address bar in sync at all times.
      browserPath <- liftEffect currentPath
      let browserRoute = parseRoute browserPath
      when (dest == browserRoute) $ do
        route <- H.gets _.route
        -- If the user came from the login page, we want to reload the user data
        -- to ensure that the navbar displays the correct user information.
        when (maybe false isLoginRoute route) $ do
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
  pushStateInterface <- H.liftEffect PushState.makeInterface
  let
    initialStore =
      { inputMail: ""
      , currentRoute: Nothing
      , translator: FPOTranslator translator
      , language: defaultLang
      , toasts: []
      , errorCooldowns: Map.empty
      , totalToasts: 0
      , pushStateInterface: pushStateInterface
      } :: Store.Store
  rootComponent <- runAppM initialStore component
  halogenIO <- runUI rootComponent unit body

  -- Set up a listener for path changes and update the route accordingly.
  -- The initial callback (old == Nothing) is allowed through so that the
  -- matchesWith state is primed, but handleQuery validates every
  -- NavigateQ against the live browser URL, so a stale initial
  -- NavigateQ that arrives after an auth redirect is harmlessly
  -- discarded.
  -- Use `Just <<< parseRoute` so unknown paths produce `Page404` instead of
  -- being silently dropped.  `RD.parse routeCodec` returns `Either`, and
  -- `Foldable (Either e)` only folds `Right` values, so a parse failure
  -- would never invoke the callback — meaning unknown URLs left the page
  -- unchanged.  Wrapping in `Maybe` (always `Just`) guarantees the
  -- callback fires for every path change.
  void $ liftEffect $ PushState.matchesWith (Just <<< parseRoute)
    ( \old new ->
        when (old /= Just new) $ launchAff_ do
          _response <- halogenIO.query $ H.mkTell $ NavigateQ new
          pure unit
    )
    pushStateInterface

  void $ liftEffect setupTruncationListener
