-- | Unified Administration page for managing users and groups.
-- |
-- | This page combines user and group management into a single tabbed interface.

module FPO.Page.Admin.Administration
  ( component
  ) where

import Prelude

import Data.Array (filter, length, null, replicate, slice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, toLower)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( LoadState(..)
  , deleteIgnore
  , getUser
  , getUserGroups
  , getUsers
  )
import FPO.Data.Route
  ( GroupSubRoute(..)
  , Route(..)
  )
import FPO.Data.Store as Store
import FPO.Dto.GroupDto
  ( GroupID
  , GroupOverview(..)
  , getGroupOverviewID
  , getGroupOverviewName
  )
import FPO.Dto.UserDto (UserID, getUserID, isAdmin)
import FPO.Dto.UserOverviewDto (UserOverviewDto)
import FPO.Dto.UserOverviewDto as UOD
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.Css as HB
import FPO.UI.HTML (loadingSpinner)
import FPO.UI.Modals.DeleteModal (deleteConfirmationModal)
import FPO.UI.Style as Style
import FPO.Util as Util
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

_userPagination = Proxy :: Proxy "userPagination"
_groupPagination = Proxy :: Proxy "groupPagination"

type Slots =
  ( userPagination :: H.Slot P.Query P.Output Unit
  , groupPagination :: H.Slot P.Query P.Output Unit
  )

data Tab = UsersTab | GroupsTab

derive instance eqTab :: Eq Tab

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  -- Tab actions
  | SwitchTab Tab
  -- User actions
  | SetUserPage P.Output
  | ChangeUserFilter String
  | RequestDeleteUser UserOverviewDto MouseEvent
  | ConfirmDeleteUser String
  | CancelDeleteUser
  | NavigateToProfile String
  | NavigateToCreateUser
  -- Group actions
  | SetGroupPage P.Output
  | ChangeGroupFilter String
  | RequestDeleteGroup GroupOverview MouseEvent
  | ConfirmDeleteGroup GroupID
  | CancelDeleteGroup
  | NavigateToGroupDocuments GroupID
  | NavigateToCreateGroup

type Input = { tab :: Maybe String }

type State = FPOState
  ( currentTab :: Tab
  , currentUserID :: Maybe UserID
  -- User state
  , users :: LoadState (Array UserOverviewDto)
  , filteredUsers :: Array UserOverviewDto
  , userFilter :: String
  , userPage :: Int
  , requestDeleteUser :: Maybe UserOverviewDto
  -- Group state
  , groups :: LoadState (Array GroupOverview)
  , filteredGroups :: Array GroupOverview
  , groupFilter :: String
  , groupPage :: Int
  , requestDeleteGroup :: Maybe GroupOverview
  -- Waiting state
  , waiting :: Boolean
  )

component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Input output m
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
  initialState :: Connected FPOTranslator Input -> State
  initialState { context, input } =
    { translator: fromFpoTranslator context
    , currentTab: tabFromString input.tab
    , currentUserID: Nothing
    -- User state
    , users: Loading
    , filteredUsers: []
    , userFilter: ""
    , userPage: 0
    , requestDeleteUser: Nothing
    -- Group state
    , groups: Loading
    , filteredGroups: []
    , groupFilter: ""
    , groupPage: 0
    , requestDeleteGroup: Nothing
    -- Waiting state
    , waiting: false
    }

  tabFromString :: Maybe String -> Tab
  tabFromString = case _ of
    Just "groups" -> GroupsTab
    _ -> UsersTab

  modalDeleteUserRef :: H.RefLabel
  modalDeleteUserRef = H.RefLabel "modal-delete-user"

  modalDeleteGroupRef :: H.RefLabel
  modalDeleteGroupRef = H.RefLabel "modal-delete-group"

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      [ renderDeleteUserModal state
      , renderDeleteGroupModal state
      , HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $ translate (label :: _ "admin_administration") state.translator
          ]
      , renderTabs state
      , case state.currentTab of
          UsersTab -> renderUsersTab state
          GroupsTab -> renderGroupsTab state
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      userResult <- getUser
      case userResult of
        -- Don't navigate to Page404 here – handleAppError already redirects
        -- to the login page for auth errors (401) and to Page404 for
        -- not-found errors (404).  Navigating to Page404 unconditionally
        -- would race with the auth redirect and overwrite the ?redirect=
        -- query parameter, causing the user to land on the 404 page after
        -- logging back in.  We also short-circuit to avoid firing further
        -- API calls (loadUsers / loadGroups) that would cascade-fail.
        Left _ -> pure unit
        Right user -> do
          if not (isAdmin user) then
            navigate Unauthorized
          else do
            H.modify_ _ { currentUserID = Just $ getUserID user }

            -- Load both users and groups
            loadUsers
            loadGroups

    Receive { context, input } -> do
      H.modify_ _
        { translator = fromFpoTranslator context
        , currentTab = tabFromString input.tab
        }

    DoNothing -> pure unit

    -- Tab actions
    SwitchTab tab -> do
      navigate $ case tab of
        UsersTab -> AdminUsers
        GroupsTab -> AdminGroups

    -- User actions
    SetUserPage (P.Clicked p) -> H.modify_ _ { userPage = p }

    ChangeUserFilter f -> do
      H.modify_ _ { userFilter = f }
      filterUsers

    RequestDeleteUser userDto event -> do
      H.liftEffect $ stopPropagation (MouseEvent.toEvent event)
      H.modify_ _ { requestDeleteUser = Just userDto }
      Util.focusRef modalDeleteUserRef

    ConfirmDeleteUser userId -> do
      H.modify_ _ { requestDeleteUser = Nothing }
      response <- deleteIgnore ("/users/" <> userId)
      case response of
        Left _ -> pure unit
        Right _ -> loadUsers

    CancelDeleteUser -> H.modify_ _ { requestDeleteUser = Nothing }

    NavigateToProfile userId -> do
      state <- H.get
      navigate $
        if state.currentUserID == Just userId then Profile
        else UserProfile userId

    NavigateToCreateUser -> navigate CreateUser

    -- Group actions
    SetGroupPage (P.Clicked p) -> H.modify_ _ { groupPage = p }

    ChangeGroupFilter f -> do
      H.modify_ _ { groupFilter = f }
      filterGroups

    RequestDeleteGroup groupOverview event -> do
      H.liftEffect $ stopPropagation (MouseEvent.toEvent event)
      H.modify_ _ { requestDeleteGroup = Just groupOverview }
      Util.focusRef modalDeleteGroupRef

    ConfirmDeleteGroup groupId -> do
      H.modify_ _ { requestDeleteGroup = Nothing, waiting = true }
      response <- deleteIgnore $ "/groups/" <> show groupId
      case response of
        Left _ -> pure unit
        Right _ -> loadGroups
      H.modify_ _ { waiting = false }

    CancelDeleteGroup -> H.modify_ _ { requestDeleteGroup = Nothing }

    NavigateToGroupDocuments gID -> navigate $ GroupRoute gID GroupDocuments

    NavigateToCreateGroup -> navigate CreateGroup

  loadUsers :: H.HalogenM State Action Slots output m Unit
  loadUsers = do
    H.modify_ _ { users = Loading }
    maybeUsers <- getUsers
    case maybeUsers of
      Left _ -> H.modify_ _ { users = Loading }
      Right users -> do
        H.modify_ _ { users = Loaded users, filteredUsers = users }
        filterUsers

  loadGroups :: H.HalogenM State Action Slots output m Unit
  loadGroups = do
    H.modify_ _ { groups = Loading }
    groups <- getUserGroups
    case groups of
      Left _ -> pure unit
      Right g -> do
        H.modify_ _ { groups = Loaded g, filteredGroups = g }
        filterGroups

  filterUsers :: H.HalogenM State Action Slots output m Unit
  filterUsers = do
    state <- H.get
    case state.users of
      Loaded users -> do
        let
          filtered =
            if String.null state.userFilter then users
            else filter
              ( \user ->
                  contains (Pattern $ toLower state.userFilter)
                    (toLower $ UOD.getName user)
                    || contains (Pattern $ toLower state.userFilter)
                      (toLower $ UOD.getEmail user)
              )
              users
        H.modify_ _ { filteredUsers = filtered }
      Loading -> pure unit

  filterGroups :: H.HalogenM State Action Slots output m Unit
  filterGroups = do
    state <- H.get
    case state.groups of
      Loaded groups -> do
        let
          filtered =
            if String.null state.groupFilter then groups
            else filter
              ( contains (Pattern $ toLower state.groupFilter) <<< toLower <<<
                  getGroupOverviewName
              )
              groups
        H.modify_ _ { filteredGroups = filtered }
      Loading -> pure unit

  -- Render tabs
  renderTabs :: State -> H.ComponentHTML Action Slots m
  renderTabs state =
    HH.ul [ HP.classes [ HB.nav, HB.navTabs, HB.mb4 ] ]
      [ HH.li [ HP.classes [ HB.navItem ] ]
          [ HH.button
              [ HP.classes $ [ HB.navLink ] <>
                  (if state.currentTab == UsersTab then [ HB.active ] else [])
              , HE.onClick $ const $ SwitchTab UsersTab
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-person-fill", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "admin_users") state.translator
              ]
          ]
      , HH.li [ HP.classes [ HB.navItem ] ]
          [ HH.button
              [ HP.classes $ [ HB.navLink ] <>
                  (if state.currentTab == GroupsTab then [ HB.active ] else [])
              , HE.onClick $ const $ SwitchTab GroupsTab
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-people-fill", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "admin_groups") state.translator
              ]
          ]
      ]

  -- Users Tab
  renderUsersTab :: State -> H.ComponentHTML Action Slots m
  renderUsersTab state =
    case state.users of
      Loading -> loadingSpinner
      Loaded _ -> renderUsersList state

  renderUsersList :: State -> H.ComponentHTML Action Slots m
  renderUsersList state =
    HH.div [ HP.classes [ H.ClassName "fpo-data-list" ] ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__header" ] ]
          [ HH.h5 [ HP.classes [ H.ClassName "fpo-data-list__title" ] ]
              [ HH.text $ translate (label :: _ "admin_users_listOfUsers")
                  state.translator
              ]
          , HH.div [ HP.classes [ H.ClassName "fpo-data-list__header-actions" ] ]
              [ HH.button
                  [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                  , HE.onClick $ const NavigateToCreateUser
                  , Style.popover $ translate (label :: _ "admin_users_createNewUser")
                      state.translator
                  ]
                  [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                  , HH.text $ translate (label :: _ "common_add") state.translator
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__search" ] ]
          [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__search-wrapper" ] ]
              [ HH.i
                  [ HP.classes
                      [ H.ClassName "bi-search"
                      , H.ClassName "fpo-data-list__search-icon"
                      ]
                  ]
                  []
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ H.ClassName "fpo-data-list__search-input" ]
                  , HP.placeholder $ translate (label :: _ "admin_users_searchUsers")
                      state.translator
                  , HP.value state.userFilter
                  , HE.onValueInput ChangeUserFilter
                  ]
              ]
          ]
      , HH.div
          [ HP.classes [ H.ClassName "fpo-data-list__body" ] ]
          ( if null usrs then
              [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty" ] ]
                  [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-icon" ] ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-people" ] ] [] ]
                  , HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-text" ] ]
                      [ HH.text $ translate (label :: _ "admin_users_noUsersFound")
                          state.translator
                      ]
                  ]
              ]
            else
              map (renderUserEntry state) usrs
                <> placeholderRows usersPerPage (length usrs) userPageCount
          )
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__footer" ] ]
          [ HH.slot _userPagination unit P.component userPaginationProps SetUserPage
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__entry-count" ] ]
              [ HH.text $
                  if length state.filteredUsers == 0 then ""
                  else show (state.userPage * usersPerPage + 1) <> "–"
                    <> show
                      ( min ((state.userPage + 1) * usersPerPage)
                          (length state.filteredUsers)
                      )
                    <> " / "
                    <> show (length state.filteredUsers)
              ]
          ]
      ]
    where
    usrs = slice (state.userPage * usersPerPage) ((state.userPage + 1) * usersPerPage)
      state.filteredUsers
    usersPerPage = 10
    userPageCount = P.calculatePageCount (length state.filteredUsers) usersPerPage
    userPaginationProps =
      { pages: userPageCount
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  renderUserEntry :: State -> UserOverviewDto -> H.ComponentHTML Action Slots m
  renderUserEntry state userDto =
    HH.div
      [ HP.classes
          [ H.ClassName "fpo-data-list__row"
          , H.ClassName "fpo-data-list__row--clickable"
          ]
      , HE.onClick $ const $ NavigateToProfile $ UOD.getID userDto
      ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-info" ] ]
          [ HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-primary" ] ]
              [ HH.text $ UOD.getName userDto ]
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-secondary" ] ]
              [ HH.text $ UOD.getEmail userDto ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-actions" ] ]
          [ HH.button
              [ HP.classes
                  [ H.ClassName "fpo-data-list__action-btn"
                  , H.ClassName "fpo-data-list__action-btn--danger"
                  ]
              , HE.onClick $ \e -> RequestDeleteUser userDto e
              , HP.disabled $ state.currentUserID == Just (UOD.getID userDto)
              , Style.popover $ translate (label :: _ "admin_users_deleteUser")
                  state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
          ]
      ]

  -- Groups Tab
  renderGroupsTab :: State -> H.ComponentHTML Action Slots m
  renderGroupsTab state =
    case state.groups of
      Loading -> loadingSpinner
      Loaded _ -> renderGroupsList state

  renderGroupsList :: State -> H.ComponentHTML Action Slots m
  renderGroupsList state =
    HH.div [ HP.classes [ H.ClassName "fpo-data-list" ] ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__header" ] ]
          [ HH.h5 [ HP.classes [ H.ClassName "fpo-data-list__title" ] ]
              [ HH.text $ translate (label :: _ "admin_groups_listOfGroups")
                  state.translator
              ]
          , HH.div [ HP.classes [ H.ClassName "fpo-data-list__header-actions" ] ]
              [ HH.button
                  [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                  , HE.onClick $ const NavigateToCreateGroup
                  , Style.popover $ translate
                      (label :: _ "admin_groups_createNewGroup")
                      state.translator
                  ]
                  [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                  , HH.text $ translate (label :: _ "common_add") state.translator
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__search" ] ]
          [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__search-wrapper" ] ]
              [ HH.i
                  [ HP.classes
                      [ H.ClassName "bi-search"
                      , H.ClassName "fpo-data-list__search-icon"
                      ]
                  ]
                  []
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ H.ClassName "fpo-data-list__search-input" ]
                  , HP.placeholder $ translate
                      (label :: _ "admin_groups_searchForGroups")
                      state.translator
                  , HP.value state.groupFilter
                  , HE.onValueInput ChangeGroupFilter
                  ]
              ]
          ]
      , HH.div
          [ HP.classes [ H.ClassName "fpo-data-list__body" ] ]
          ( if null grps then
              [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty" ] ]
                  [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-icon" ] ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-collection" ] ] [] ]
                  , HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-text" ] ]
                      [ HH.text $ translate (label :: _ "admin_groups_noGroupsFound")
                          state.translator
                      ]
                  ]
              ]
            else
              map (renderGroupEntry state) grps
                <> placeholderRows groupsPerPage (length grps) groupPageCount
          )
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__footer" ] ]
          [ HH.slot _groupPagination unit P.component groupPaginationProps
              SetGroupPage
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__entry-count" ] ]
              [ HH.text $
                  if length state.filteredGroups == 0 then ""
                  else show (state.groupPage * groupsPerPage + 1) <> "-"
                    <> show
                      ( min ((state.groupPage + 1) * groupsPerPage)
                          (length state.filteredGroups)
                      )
                    <> " / "
                    <> show (length state.filteredGroups)
              ]
          ]
      ]
    where
    grps = slice (state.groupPage * groupsPerPage)
      ((state.groupPage + 1) * groupsPerPage)
      state.filteredGroups
    groupsPerPage = 10
    groupPageCount = P.calculatePageCount (length state.filteredGroups) groupsPerPage
    groupPaginationProps =
      { pages: groupPageCount
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  renderGroupEntry :: State -> GroupOverview -> H.ComponentHTML Action Slots m
  renderGroupEntry state groupOverview@(GroupOverview g) =
    HH.div
      [ HP.classes
          [ H.ClassName "fpo-data-list__row"
          , H.ClassName "fpo-data-list__row--clickable"
          ]
      , HE.onClick $ const $ NavigateToGroupDocuments g.groupOverviewID
      ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-info" ] ]
          [ HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-primary" ] ]
              [ HH.text g.groupOverviewName ]
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-secondary" ] ]
              [ HH.text g.groupOverviewDescription ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-actions" ] ]
          [ HH.button
              [ HP.classes
                  [ H.ClassName "fpo-data-list__action-btn"
                  , H.ClassName "fpo-data-list__action-btn--danger"
                  ]
              , HE.onClick $ \e -> RequestDeleteGroup groupOverview e
              , HP.disabled state.waiting
              , Style.popover $ translate (label :: _ "admin_groups_deleteGroup")
                  state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
          ]
      ]

  placeholderRows :: forall w i. Int -> Int -> Int -> Array (HH.HTML w i)
  placeholderRows perPage currentCount pages =
    let
      needed = perPage - currentCount
    in
      if pages <= 1 || needed <= 0 then []
      else replicate needed $
        HH.div
          [ HP.classes [ H.ClassName "fpo-data-list__row" ]
          , HP.style "visibility: hidden; pointer-events: none;"
          ]
          [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-info" ] ]
              [ HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-primary" ] ]
                  [ HH.text "\x00a0" ]
              , HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-secondary" ] ]
                  [ HH.text "\x00a0" ]
              ]
          ]

  -- Delete modals
  renderDeleteUserModal :: State -> H.ComponentHTML Action Slots m
  renderDeleteUserModal state =
    case state.requestDeleteUser of
      Nothing -> HH.div [ HP.classes [ HB.dNone ] ] []
      Just userDto ->
        deleteConfirmationModal
          state.translator
          userDto
          UOD.getName
          CancelDeleteUser
          (ConfirmDeleteUser <<< UOD.getID)
          DoNothing
          (Just modalDeleteUserRef)
          (translate (label :: _ "admin_users_theUser") state.translator)

  renderDeleteGroupModal :: State -> H.ComponentHTML Action Slots m
  renderDeleteGroupModal state =
    case state.requestDeleteGroup of
      Nothing -> HH.div [ HP.classes [ HB.dNone ] ] []
      Just groupOverview ->
        deleteConfirmationModal
          state.translator
          groupOverview
          getGroupOverviewName
          CancelDeleteGroup
          (ConfirmDeleteGroup <<< getGroupOverviewID)
          DoNothing
          (Just modalDeleteGroupRef)
          (translate (label :: _ "common_theGroup") state.translator)
