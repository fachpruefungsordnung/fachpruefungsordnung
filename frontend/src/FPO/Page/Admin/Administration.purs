-- | Unified Administration page for managing users and groups.
-- |
-- | This page combines user and group management into a single tabbed interface.

module FPO.Page.Admin.Administration
  ( component
  ) where

import Prelude

import Data.Array (filter, length, replicate, slice)
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
import FPO.Dto.UserDto (UserID, getUserID, isAdmin, isUserSuperadmin)
import FPO.Dto.UserOverviewDto (UserOverviewDto)
import FPO.Dto.UserOverviewDto as UOD
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (emptyEntryGen)
import FPO.UI.Modals.DeleteModal (deleteConfirmationModal)
import FPO.UI.Style as Style
import FPO.Util as Util
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

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
  | RequestDeleteUser UserOverviewDto
  | ConfirmDeleteUser String
  | CancelDeleteUser
  | NavigateToProfile String
  | NavigateToCreateUser
  -- Group actions
  | SetGroupPage P.Output
  | ChangeGroupFilter String
  | RequestDeleteGroup GroupOverview
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
        Left _ -> navigate Page404
        Right user -> do
          when (not $ isAdmin user && not (isUserSuperadmin user)) $ pure unit
          when (not $ isAdmin user) $ navigate Page404
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

    RequestDeleteUser userDto -> do
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

    RequestDeleteGroup groupOverview -> do
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
      Loading -> renderLoading
      Loaded _ -> renderUsersList state

  renderUsersList :: State -> H.ComponentHTML Action Slots m
  renderUsersList state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.col12, HB.colLg10 ] ]
          [ HH.div [ HP.classes [ HB.card ] ]
              [ HH.div
                  [ HP.classes
                      [ HB.cardHeader
                      , HB.dFlex
                      , HB.justifyContentBetween
                      , HB.alignItemsCenter
                      ]
                  ]
                  [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                      [ HH.text $ translate (label :: _ "admin_users_listOfUsers")
                          state.translator
                      ]
                  , HH.button
                      [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                      , HE.onClick $ const NavigateToCreateUser
                      , Style.popover $ translate
                          (label :: _ "admin_users_createNewUser")
                          state.translator
                      ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                      , HH.text $ translate (label :: _ "common_add") state.translator
                      ]
                  ]
              , HH.div [ HP.classes [ HB.cardBody ] ]
                  [ renderFilterInput
                      state.userFilter
                      ( translate (label :: _ "admin_users_searchUsers")
                          state.translator
                      )
                      ChangeUserFilter
                  , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                      $ map (renderUserEntry state) usrs
                          <> replicate (usersPerPage - length usrs)
                            (emptyEntryGen [ emptyUserButtons ])
                  , HH.slot _userPagination unit P.component userPaginationProps
                      SetUserPage
                  , renderEntryCount state.userPage usersPerPage
                      (length state.filteredUsers)
                  ]
              ]
          ]
      ]
    where
    usrs = slice (state.userPage * usersPerPage) ((state.userPage + 1) * usersPerPage)
      state.filteredUsers
    userPaginationProps =
      { pages: P.calculatePageCount (length state.filteredUsers) usersPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }
    usersPerPage = 10

  renderUserEntry :: State -> UserOverviewDto -> H.ComponentHTML Action Slots m
  renderUserEntry state userDto =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
          [ HH.span [ HP.classes [ HB.fwBold ] ]
              [ HH.text $ UOD.getName userDto ]
          , HH.small [ HP.classes [ HB.textMuted ] ]
              [ HH.text $ UOD.getEmail userDto ]
          ]
      , HH.div [ HP.classes [ HB.dFlex, HB.gap2 ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick $ const $ NavigateToProfile $ UOD.getID userDto
              , Style.popover $ translate (label :: _ "admin_users_goToProfilePage")
                  state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-pencil-fill" ] ] [] ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
              , HE.onClick $ const $ RequestDeleteUser userDto
              , HP.disabled $ state.currentUserID == Just (UOD.getID userDto)
              , Style.popover $ translate (label :: _ "admin_users_deleteUser")
                  state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
          ]
      ]

  emptyUserButtons :: forall w. HH.HTML w Action
  emptyUserButtons =
    HH.div [ HP.classes [ HB.dFlex, HB.gap2 ] ]
      [ HH.button
          [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-pencil-fill" ] ] [] ]
      , HH.button
          [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
      ]

  -- Groups Tab
  renderGroupsTab :: State -> H.ComponentHTML Action Slots m
  renderGroupsTab state =
    case state.groups of
      Loading -> renderLoading
      Loaded _ -> renderGroupsList state

  renderGroupsList :: State -> H.ComponentHTML Action Slots m
  renderGroupsList state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.col12, HB.colLg10 ] ]
          [ HH.div [ HP.classes [ HB.card ] ]
              [ HH.div
                  [ HP.classes
                      [ HB.cardHeader
                      , HB.dFlex
                      , HB.justifyContentBetween
                      , HB.alignItemsCenter
                      ]
                  ]
                  [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                      [ HH.text $ translate (label :: _ "admin_groups_listOfGroups")
                          state.translator
                      ]
                  , HH.button
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
              , HH.div [ HP.classes [ HB.cardBody ] ]
                  [ renderFilterInput
                      state.groupFilter
                      ( translate (label :: _ "admin_groups_searchForGroups")
                          state.translator
                      )
                      ChangeGroupFilter
                  , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                      $ map (renderGroupEntry state) grps
                          <> replicate (groupsPerPage - length grps)
                            (emptyEntryGen [ emptyGroupButtons ])
                  , HH.slot _groupPagination unit P.component groupPaginationProps
                      SetGroupPage
                  , renderEntryCount state.groupPage groupsPerPage
                      (length state.filteredGroups)
                  ]
              ]
          ]
      ]
    where
    grps = slice (state.groupPage * groupsPerPage)
      ((state.groupPage + 1) * groupsPerPage)
      state.filteredGroups
    groupPaginationProps =
      { pages: P.calculatePageCount (length state.filteredGroups) groupsPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }
    groupsPerPage = 10

  renderGroupEntry :: State -> GroupOverview -> H.ComponentHTML Action Slots m
  renderGroupEntry state groupOverview@(GroupOverview g) =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
          [ HH.span [ HP.classes [ HB.fwBold ] ]
              [ HH.text g.groupOverviewName ]
          , HH.small [ HP.classes [ HB.textMuted ] ]
              [ HH.text g.groupOverviewDescription ]
          ]
      , HH.div [ HP.classes [ HB.dFlex, HB.gap2 ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick $ const $ NavigateToGroupDocuments g.groupOverviewID
              , Style.popover $ translate
                  (label :: _ "admin_groups_viewDocumentsPage")
                  state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-pencil-fill" ] ] [] ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
              , HE.onClick $ const $ RequestDeleteGroup groupOverview
              , HP.disabled state.waiting
              , Style.popover $ translate (label :: _ "admin_groups_deleteGroup")
                  state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
          ]
      ]

  emptyGroupButtons :: forall w. HH.HTML w Action
  emptyGroupButtons =
    HH.div [ HP.classes [ HB.dFlex, HB.gap2 ] ]
      [ HH.button
          [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-pencil-fill" ] ] [] ]
      , HH.button
          [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
      ]

  -- Common rendering helpers
  renderLoading :: H.ComponentHTML Action Slots m
  renderLoading =
    HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
      [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]

  -- | Renders a subtle "Showing X–Y of Z" indicator below the pagination.
  renderEntryCount
    :: forall w. Int -> Int -> Int -> HH.HTML w Action
  renderEntryCount currentPage perPage totalItems =
    if totalItems == 0 then HH.text ""
    else
      let
        startItem = currentPage * perPage + 1
        endItem = min ((currentPage + 1) * perPage) totalItems
      in
        HH.div [ HP.classes [ HB.textStart, HB.textMuted, HB.small ] ]
          [ HH.text $
              show startItem <> "–" <> show endItem <> " / " <> show totalItems
          ]

  renderFilterInput
    :: forall w
     . String
    -> String
    -> (String -> Action)
    -> HH.HTML w Action
  renderFilterInput value placeholder action =
    HH.div [ HP.classes [ HB.inputGroup, HB.mb3 ] ]
      [ HH.span [ HP.classes [ HB.inputGroupText ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-search" ] ] [] ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.classes [ HB.formControl ]
          , HP.placeholder placeholder
          , HP.value value
          , HE.onValueInput action
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
