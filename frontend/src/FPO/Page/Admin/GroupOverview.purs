-- | Unified Group Overview page showing documents and members in a tabbed interface.
-- |
-- | This page combines the former DocOverview and MemberOverview pages into a single
-- | view under /administration/groups/:groupID

module FPO.Page.Admin.GroupOverview
  ( component
  ) where

import Prelude

import Data.Array (elem, filter, head, length, notElem, null, slice, (:))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), contains, toLower)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import FPO.Components.Pagination as P
import FPO.Data.AppError (AppError(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( changeRole
  , createNewDocument
  , deleteIgnore
  , getAuthorizedUser
  , getDocumentsQueryFromURL
  , getGroup
  , getUser
  , getUsers
  , patchGroup
  )
import FPO.Data.Route (GroupSubRoute(..), Route(..), editorRoute)
import FPO.Data.Store as Store
import FPO.Data.Time (formatRelativeTime)
import FPO.Dto.CreateDocumentDto (NewDocumentCreateDto(..))
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.FullDocument as FD
import FPO.Dto.DocumentDto.Query as DQ
import FPO.Dto.GroupDto
  ( GroupDto
  , GroupID
  , GroupMemberDto
  , GroupPatch(..)
  , getGroupDescription
  , getGroupMembers
  , getGroupName
  , getUserInfoID
  , getUserInfoName
  , getUserInfoRole
  , lookupUser
  )
import FPO.Dto.UserDto (UserID, isAdminOf, isUserSuperadmin)
import FPO.Dto.UserOverviewDto (UserOverviewDto)
import FPO.Dto.UserOverviewDto as UOD
import FPO.Dto.UserRoleDto (Role(..))
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addModal, loadingSpinner)
import FPO.UI.Modals.DeleteModal (deleteConfirmationModal)
import FPO.UI.Style as Style
import FPO.Util (focusRef, handleKeyDown, singletonIf)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import FPO.UI.Css as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Type.Proxy (Proxy(..))

_docPagination = Proxy :: Proxy "docPagination"
_memberPagination = Proxy :: Proxy "memberPagination"

type Slots =
  ( docPagination :: H.Slot P.Query P.Output Unit
  , memberPagination :: H.Slot P.Query P.Output Unit
  )

data Tab = DocumentsTab | MembersTab | SettingsTab

derive instance eqTab :: Eq Tab

type Input = { groupID :: GroupID, tab :: Maybe String }

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  -- Tab actions
  | SwitchTab Tab
  -- Document actions
  | SetDocPage P.Output
  | FilterDocuments String
  | ViewDocument DH.DocumentID
  | RequestDeleteDocument Int MouseEvent
  | ConfirmDeleteDocument Int
  | RequestCreateDocument
  | ConfirmCreateDocument
  | ChangeCreateDocumentName String
  -- Member actions
  | SetMemberPage P.Output
  | FilterMembers String
  | RequestRemoveMember UserID
  | ConfirmRemoveMember UserID
  | SetUserRole GroupMemberDto Role
  | NavigateToUserProfile UserID
  -- Modal actions
  | CancelModal
  -- Add member popover actions
  | OpenAddMemberPopover
  | CloseAddMemberPopover
  | ForceCloseAddMemberPopover
  | FilterAvailableUsers String
  | ToggleUserSelection UserID
  | ConfirmAddMembers
  | CancelAddMembers
  -- Settings actions
  | ChangeGroupName String
  | ChangeGroupDescription String
  | SaveGroupSettings

data ModalState
  = NoModal
  | DeleteDocumentModal Int
  | CreateDocumentModal { waiting :: Boolean }
  | RemoveMemberModal UserID

type State = FPOState
  ( currentTab :: Tab
  , groupID :: GroupID
  , group :: Maybe GroupDto
  , isGroupAdmin :: Boolean
  , isSuperAdmin :: Boolean
  -- Document state
  , documents :: Array DH.DocumentHeader
  , filteredDocuments :: Array DH.DocumentHeader
  , documentFilter :: String
  , docPage :: Int
  , currentTime :: Maybe DateTime
  , newDocumentName :: String
  -- Member state
  , filteredMembers :: Array GroupMemberDto
  , memberFilter :: String
  , memberPage :: Int
  -- Modal state
  , modalState :: ModalState
  -- Add member popover state
  , showAddMemberPopover :: Boolean
  , loadingUsers :: Boolean
  , availableUsers :: Array UserOverviewDto
  , filteredAvailableUsers :: Array UserOverviewDto
  , selectedUsersToAdd :: Array UserID
  , userSearchFilter :: String
  , addingMembers :: Boolean
  -- Settings state
  , editedGroupName :: String
  , editedGroupDescription :: String
  , settingsSaving :: Boolean
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
    , groupID: input.groupID
    , group: Nothing
    , isGroupAdmin: false
    , isSuperAdmin: false
    -- Document state
    , documents: []
    , filteredDocuments: []
    , documentFilter: ""
    , docPage: 0
    , currentTime: Nothing
    , newDocumentName: ""
    -- Member state
    , filteredMembers: []
    , memberFilter: ""
    , memberPage: 0
    -- Modal state
    , modalState: NoModal
    -- Add member popover state
    , showAddMemberPopover: false
    , loadingUsers: false
    , availableUsers: []
    , filteredAvailableUsers: []
    , selectedUsersToAdd: []
    , userSearchFilter: ""
    , addingMembers: false
    -- Settings state
    , editedGroupName: ""
    , editedGroupDescription: ""
    , settingsSaving: false
    }

  tabFromString :: Maybe String -> Tab
  tabFromString = case _ of
    Just "members" -> MembersTab
    Just "settings" -> SettingsTab
    _ -> DocumentsTab

  modalInputRef :: H.RefLabel
  modalInputRef = H.RefLabel "modal-input"

  modalDeleteDocRef :: H.RefLabel
  modalDeleteDocRef = H.RefLabel "modal-delete-doc"

  modalRemoveMemberRef :: H.RefLabel
  modalRemoveMemberRef = H.RefLabel "modal-remove-member"

  itemsPerPage :: Int
  itemsPerPage = 10

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      [ renderModals state
      , -- Transparent overlay to dismiss the Add Member popover on outside click
        if state.showAddMemberPopover then
          HH.div
            [ HP.style
                "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 1040; background: transparent;"
            , HE.onClick $ const ForceCloseAddMemberPopover
            ]
            []
        else HH.text ""
      , renderHeader state
      , renderTabs state
      , case state.currentTab of
          DocumentsTab -> renderDocumentsTab state
          MembersTab -> renderMembersTab state
          SettingsTab -> renderSettingsTab state
      ]

  renderModals :: State -> H.ComponentHTML Action Slots m
  renderModals state = case state.modalState of
    DeleteDocumentModal documentID ->
      deleteConfirmationModal state.translator documentID
        (docNameFromID state)
        CancelModal
        ConfirmDeleteDocument
        DoNothing
        (Just modalDeleteDocRef)
        (translate (label :: _ "common_project") state.translator)
    CreateDocumentModal ms ->
      createDocumentModal ms state
    RemoveMemberModal userID ->
      fromMaybe (HH.div_ []) do
        grp <- state.group
        member <- lookupUser grp userID
        pure $ deleteConfirmationModal state.translator userID
          (const $ getUserInfoName member)
          CancelModal
          ConfirmRemoveMember
          DoNothing
          (Just modalRemoveMemberRef)
          (translate (label :: _ "common_member") state.translator)
    NoModal -> HH.div_ []

  renderHeader :: State -> H.ComponentHTML Action Slots m
  renderHeader state =
    HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
      [ HH.text $ fromMaybe
          (translate (label :: _ "common_group") state.translator)
          (getGroupName <$> state.group)
      ]

  renderTabs :: State -> H.ComponentHTML Action Slots m
  renderTabs state =
    HH.ul [ HP.classes [ HB.nav, HB.navTabs, HB.mb4 ] ]
      [ HH.li [ HP.classes [ HB.navItem ] ]
          [ HH.button
              [ HP.classes $ [ HB.navLink ] <>
                  (if state.currentTab == DocumentsTab then [ HB.active ] else [])
              , HE.onClick $ const $ SwitchTab DocumentsTab
              ]
              [ HH.i
                  [ HP.classes [ H.ClassName "bi-file-earmark-text-fill", HB.me2 ] ]
                  []
              , HH.text $ translate (label :: _ "common_projects") state.translator
              ]
          ]
      , HH.li [ HP.classes [ HB.navItem ] ]
          [ HH.button
              [ HP.classes $ [ HB.navLink ] <>
                  (if state.currentTab == MembersTab then [ HB.active ] else [])
              , HE.onClick $ const $ SwitchTab MembersTab
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-people-fill", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "common_members") state.translator
              ]
          ]
      , HH.li [ HP.classes [ HB.navItem ] ]
          [ HH.button
              [ HP.classes $ [ HB.navLink ] <>
                  (if state.currentTab == SettingsTab then [ HB.active ] else [])
              , HE.onClick $ const $ SwitchTab SettingsTab
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-gear-fill", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "gs_settings") state.translator
              ]
          ]
      ]

  renderDocumentsTab :: State -> H.ComponentHTML Action Slots m
  renderDocumentsTab state =
    case state.group of
      Nothing -> loadingSpinner
      Just _ -> renderDocumentsList state

  renderDocumentsList :: State -> H.ComponentHTML Action Slots m
  renderDocumentsList state =
    HH.div [ HP.classes [ H.ClassName "fpo-data-list" ] ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__header" ] ]
          [ HH.h5 [ HP.classes [ H.ClassName "fpo-data-list__title" ] ]
              [ HH.text $ translate (label :: _ "gp_groupProjects") state.translator ]
          , HH.div [ HP.classes [ H.ClassName "fpo-data-list__header-actions" ] ]
              [ HH.button
                  [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                  , HE.onClick $ const RequestCreateDocument
                  , Style.popover $ translate (label :: _ "gp_createNewProject") state.translator
                  ]
                  [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                  , HH.text $ translate (label :: _ "common_add") state.translator
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__search" ] ]
          [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__search-wrapper" ] ]
              [ HH.i [ HP.classes [ H.ClassName "bi-search", H.ClassName "fpo-data-list__search-icon" ] ] []
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ H.ClassName "fpo-data-list__search-input" ]
                  , HP.placeholder $ translate (label :: _ "gp_searchProjects") state.translator
                  , HP.value state.documentFilter
                  , HE.onValueInput FilterDocuments
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__body" ] ]
          ( if null docs then
              [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty" ] ]
                  [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-icon" ] ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-folder2-open" ] ] [] ]
                  , HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-text" ] ]
                      [ HH.text $ translate (label :: _ "gp_noDocumentsFound") state.translator ]
                  ]
              ]
            else
              map (renderDocumentEntry state) docs
          )
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__footer" ] ]
          [ HH.slot _docPagination unit P.component docPaginationProps SetDocPage
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__entry-count" ] ]
              [ HH.text $ if length state.filteredDocuments == 0 then "" else show (state.docPage * itemsPerPage + 1) <> "\x2013" <> show (min ((state.docPage + 1) * itemsPerPage) (length state.filteredDocuments)) <> " / " <> show (length state.filteredDocuments) ]
          ]
      ]
    where
    docs = slice (state.docPage * itemsPerPage) ((state.docPage + 1) * itemsPerPage)
      state.filteredDocuments
    docPaginationProps =
      { pages: P.calculatePageCount (length state.filteredDocuments) itemsPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  renderDocumentEntry :: State -> DH.DocumentHeader -> H.ComponentHTML Action Slots m
  renderDocumentEntry state doc =
    HH.div
      [ HP.classes [ H.ClassName "fpo-data-list__row", H.ClassName "fpo-data-list__row--clickable" ]
      , HE.onClick $ const $ ViewDocument (DH.getID doc)
      ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-info" ] ]
          [ HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-primary" ] ]
              [ HH.text $ DH.getName doc ]
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-secondary" ] ]
              [ HH.text $ formatRelativeTime state.currentTime
                  (DD.docDateToDateTime (DH.getLastEdited doc))
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-actions" ] ]
          [ HH.button
              [ HP.classes [ H.ClassName "fpo-data-list__action-btn", H.ClassName "fpo-data-list__action-btn--danger" ]
              , HE.onClick $ \e -> RequestDeleteDocument (DH.getID doc) e
              , Style.popover $ translate (label :: _ "gp_removeProject") state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
          ]
      ]


  renderMembersTab :: State -> H.ComponentHTML Action Slots m
  renderMembersTab state =
    case state.group of
      Nothing -> loadingSpinner
      Just _ -> renderMembersList state

  renderMembersList :: State -> H.ComponentHTML Action Slots m
  renderMembersList state =
    HH.div [ HP.classes [ H.ClassName "fpo-data-list" ] ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__header" ] ]
          [ HH.h5 [ HP.classes [ H.ClassName "fpo-data-list__title" ] ]
              [ HH.text $ translate (label :: _ "common_members") state.translator ]
          , HH.div [ HP.classes [ H.ClassName "fpo-data-list__header-actions" ] ]
              [ HH.div [ HP.style "position: relative;" ]
                  [ HH.button
                      [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                      , HE.onClick $ const OpenAddMemberPopover
                      , Style.popover $ translate (label :: _ "gm_addMember") state.translator
                      ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                      , HH.text $ translate (label :: _ "common_add") state.translator
                      ]
                  , if state.showAddMemberPopover then renderAddMemberPopover state
                    else HH.text ""
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__search" ] ]
          [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__search-wrapper" ] ]
              [ HH.i [ HP.classes [ H.ClassName "bi-search", H.ClassName "fpo-data-list__search-icon" ] ] []
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ H.ClassName "fpo-data-list__search-input" ]
                  , HP.placeholder $ translate (label :: _ "gm_searchMembers") state.translator
                  , HP.value state.memberFilter
                  , HE.onValueInput FilterMembers
                  ]
              ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__body" ] ]
          ( if null members then
              [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty" ] ]
                  [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-icon" ] ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-people" ] ] [] ]
                  , HH.div [ HP.classes [ H.ClassName "fpo-data-list__empty-text" ] ]
                      [ HH.text $ translate (label :: _ "gm_noMembersFound") state.translator ]
                  ]
              ]
            else
              map (renderMemberEntry state) members
          )
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__footer" ] ]
          [ HH.slot _memberPagination unit P.component memberPaginationProps SetMemberPage
          , HH.span [ HP.classes [ H.ClassName "fpo-data-list__entry-count" ] ]
              [ HH.text $ if length state.filteredMembers == 0 then "" else show (state.memberPage * itemsPerPage + 1) <> "\x2013" <> show (min ((state.memberPage + 1) * itemsPerPage) (length state.filteredMembers)) <> " / " <> show (length state.filteredMembers) ]
          ]
      ]
    where
    members = slice (state.memberPage * itemsPerPage)
      ((state.memberPage + 1) * itemsPerPage)
      state.filteredMembers
    memberPaginationProps =
      { pages: P.calculatePageCount (length state.filteredMembers) itemsPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  renderMemberEntry :: State -> GroupMemberDto -> H.ComponentHTML Action Slots m
  renderMemberEntry state member =
    HH.div [ HP.classes [ H.ClassName "fpo-data-list__row" ] ]
      [ HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-info" ] ]
          [ HH.span [ HP.classes [ H.ClassName "fpo-data-list__row-primary" ] ]
              [ HH.text $ getUserInfoName member ]
          ]
      , HH.div [ HP.classes [ H.ClassName "fpo-data-list__row-actions" ] ]
          [ if state.isGroupAdmin then
              renderRoleToggle state member
            else
              HH.span [ HP.classes [ HB.badge, HB.bgSecondary ] ]
                [ HH.text $ roleToString state (getUserInfoRole member) ]
          , if state.isSuperAdmin then
              HH.button
                [ HP.classes [ H.ClassName "fpo-data-list__action-btn", H.ClassName "fpo-data-list__action-btn--accent" ]
                , HE.onClick $ const $ NavigateToUserProfile (getUserInfoID member)
                , Style.popover $ translate (label :: _ "admin_users_goToProfilePage") state.translator
                ]
                [ HH.i [ HP.classes [ H.ClassName "bi-person-fill" ] ] [] ]
            else
              HH.text ""
          , HH.button
              [ HP.classes [ H.ClassName "fpo-data-list__action-btn", H.ClassName "fpo-data-list__action-btn--danger" ]
              , HE.onClick $ const $ RequestRemoveMember (getUserInfoID member)
              , Style.popover $ translate (label :: _ "gm_removeMember") state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-box-arrow-right" ] ] [] ]
          ]
      ]

  renderRoleToggle :: State -> GroupMemberDto -> H.ComponentHTML Action Slots m
  renderRoleToggle state member =
    HH.div [ HP.classes [ HB.btnGroup, HB.btnGroupSm ] ]
      [ HH.button
          [ HP.classes $ [ HB.btn ] <>
              if currentRole == Admin then [ HB.btnPrimary ]
              else [ HB.btnOutlineSecondary ]
          , HE.onClick $
              if currentRole /= Admin then const $ SetUserRole member Admin
              else const DoNothing
          ]
          [ HH.text "Admin" ]
      , HH.button
          [ HP.classes $ [ HB.btn ] <>
              if currentRole == Member then [ HB.btnPrimary ]
              else [ HB.btnOutlineSecondary ]
          , HE.onClick $
              if currentRole /= Member then const $ SetUserRole member Member
              else const DoNothing
          ]
          [ HH.text $ translate (label :: _ "common_member") state.translator ]
      ]
    where
    currentRole = getUserInfoRole member


  roleToString :: State -> Role -> String
  roleToString state = case _ of
    Admin -> "Admin"
    Member -> translate (label :: _ "common_member") state.translator

  renderAddMemberPopover :: State -> H.ComponentHTML Action Slots m
  renderAddMemberPopover state =
    let
      hasSelection = not $ null state.selectedUsersToAdd
    in
      HH.div
        [ HP.classes [ HB.card, HB.shadow ]
        , HP.style
            "position: absolute; top: 100%; right: 0; z-index: 1050; width: 350px; margin-top: 0.25rem;"
        ]
        [ -- Header
          HH.div
            [ HP.classes
                [ HB.cardHeader
                , HB.dFlex
                , HB.justifyContentBetween
                , HB.alignItemsCenter
                , HB.py2
                ]
            ]
            [ HH.span [ HP.classes [ HB.fwBold ] ]
                [ HH.i [ HP.classes [ H.ClassName "bi-person-plus-fill", HB.me2 ] ] []
                , HH.text $ translate (label :: _ "gm_addMember") state.translator
                ]
            , if hasSelection then HH.span [ HP.classes [ HB.badge, HB.bgPrimary ] ]
                [ HH.text $ show (length state.selectedUsersToAdd) ]
              else HH.text ""
            ]
        , -- Body
          HH.div [ HP.classes [ HB.cardBody, HB.p2 ] ]
            [ -- Search input
              HH.div [ HP.classes [ HB.inputGroup, HB.inputGroupSm, HB.mb2 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.classes [ H.ClassName "bi-search" ] ] [] ]
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder $ translate
                        (label :: _ "admin_users_searchUsers")
                        state.translator
                    , HP.value state.userSearchFilter
                    , HE.onValueInput FilterAvailableUsers
                    ]
                ]
            , -- User list or loading
              if state.loadingUsers then HH.div
                [ HP.classes [ HB.textCenter, HB.py3 ] ]
                [ HH.div
                    [ HP.classes
                        [ HB.spinnerBorder, HB.spinnerBorderSm, HB.textPrimary ]
                    ]
                    []
                ]
              else if null state.filteredAvailableUsers then HH.div
                [ HP.classes [ HB.textCenter, HB.py3, HB.textMuted, HB.small ] ]
                [ HH.text $ translate (label :: _ "gm_noUsersFound") state.translator
                ]
              else
                HH.div
                  [ HP.classes [ HB.listGroup, HB.listGroupFlush ]
                  , HP.style "max-height: 200px; overflow-y: auto;"
                  ]
                  $ map (renderUserCheckboxEntry state) state.filteredAvailableUsers
            ]
        , -- Footer
          HH.div
            [ HP.classes
                [ HB.cardFooter, HB.dFlex, HB.justifyContentEnd, HB.gap2, HB.py2 ]
            ]
            [ HH.button
                [ HP.classes [ HB.btn, HB.btnOutlineSecondary, HB.btnSm ]
                , HE.onClick $ const CancelAddMembers
                , HP.disabled state.addingMembers
                ]
                [ HH.text $ translate (label :: _ "common_cancel") state.translator ]
            , HH.button
                [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                , HE.onClick $ const ConfirmAddMembers
                , HP.disabled $ not hasSelection || state.addingMembers
                ]
                [ if state.addingMembers then HH.span
                    [ HP.classes [ HB.spinnerBorderSm, HB.me1 ] ]
                    []
                  else HH.i [ HP.classes [ H.ClassName "bi-person-plus", HB.me1 ] ] []
                , HH.text $ translate (label :: _ "common_add") state.translator
                ]
            ]
        ]

  renderUserCheckboxEntry
    :: State -> UserOverviewDto -> H.ComponentHTML Action Slots m
  renderUserCheckboxEntry state user =
    let
      userId = UOD.getID user
      isSelected = userId `elem` state.selectedUsersToAdd
    in
      HH.label
        [ HP.classes [ HB.listGroupItem, HB.dFlex, HB.alignItemsCenter, HB.gap3 ]
        , HP.style "cursor: pointer;"
        ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.classes [ HB.formCheckInput, HB.mt0 ]
            , HP.checked isSelected
            , HE.onChecked $ const $ ToggleUserSelection userId
            ]
        , HH.div [ HP.classes [ HB.dFlex, HB.flexColumn ] ]
            [ HH.span [ HP.classes [ HB.fwBold ] ]
                [ HH.text $ UOD.getName user ]
            , HH.small [ HP.classes [ HB.textMuted ] ]
                [ HH.text $ UOD.getEmail user ]
            ]
        ]

  createDocumentModal
    :: { waiting :: Boolean } -> State -> H.ComponentHTML Action Slots m
  createDocumentModal ms state =
    addModal (translate (label :: _ "gp_createNewProject") state.translator)
      CancelModal
      DoNothing $
      [ HH.div
          [ HP.classes [ HB.modalBody ]
          , HE.onKeyDown $ handleKeyDown
              CancelModal
              ConfirmCreateDocument
              DoNothing
          , HP.tabIndex 0
          ]
          [ HH.div [ HP.classes [ HB.mb3 ] ]
              [ HH.label
                  [ HP.for "docName"
                  , HP.classes [ HH.ClassName "form-label" ]
                  ]
                  [ HH.text $ translate (label :: _ "gp_documentName")
                      state.translator
                  ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ HH.ClassName "form-control" ]
                  , HP.id "docName"
                  , HP.ref modalInputRef
                  , HP.placeholder $ translate (label :: _ "gp_enterDocumentName")
                      state.translator
                  , HP.required true
                  , HE.onValueInput ChangeCreateDocumentName
                  ]
              ]
          ]
      , HH.div [ HP.classes [ HB.modalFooter ] ]
          ( singletonIf ms.waiting
              (HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary, HB.me5 ] ] [])
              <>
                [ HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnSecondary ]
                    , HE.onClick (const CancelModal)
                    , HP.disabled ms.waiting
                    ]
                    [ HH.text $ translate (label :: _ "common_cancel")
                        state.translator
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnPrimary ]
                    , HE.onClick (const ConfirmCreateDocument)
                    , HP.disabled (state.newDocumentName == "" || ms.waiting)
                    ]
                    [ HH.text $ translate (label :: _ "common_create")
                        state.translator
                    ]
                ]
          )
      ]

  renderSettingsTab :: State -> H.ComponentHTML Action Slots m
  renderSettingsTab state =
    case state.group of
      Nothing -> loadingSpinner
      Just _ -> renderSettingsForm state

  renderSettingsForm :: State -> H.ComponentHTML Action Slots m
  renderSettingsForm state =
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
                      [ HH.text $ translate (label :: _ "gs_settings")
                          state.translator
                      ]
                  ]
              , HH.div [ HP.classes [ HB.cardBody ] ]
                  [ -- Group Name field
                    HH.div [ HP.classes [ HB.mb3 ] ]
                      [ HH.label
                          [ HP.for "groupName"
                          , HP.classes [ HB.formLabel ]
                          ]
                          [ HH.text $ translate (label :: _ "gs_groupName")
                              state.translator
                          ]
                      , HH.input
                          [ HP.type_ HP.InputText
                          , HP.classes [ HB.formControl ]
                          , HP.id "groupName"
                          , HP.placeholder $ translate
                              (label :: _ "gs_groupNamePlaceholder")
                              state.translator
                          , HP.value state.editedGroupName
                          , HP.required true
                          , HE.onValueInput ChangeGroupName
                          , HP.disabled state.settingsSaving
                          ]
                      ]
                  , -- Group Description field
                    HH.div [ HP.classes [ HB.mb3 ] ]
                      [ HH.label
                          [ HP.for "groupDescription"
                          , HP.classes [ HB.formLabel ]
                          ]
                          [ HH.text $ translate (label :: _ "gs_description")
                              state.translator
                          ]
                      , HH.textarea
                          [ HP.classes [ HB.formControl ]
                          , HP.id "groupDescription"
                          , HP.placeholder $ translate
                              (label :: _ "gs_descriptionPlaceholder")
                              state.translator
                          , HP.value state.editedGroupDescription
                          , HP.rows 3
                          , HE.onValueInput ChangeGroupDescription
                          , HP.disabled state.settingsSaving
                          ]
                      ]
                  , -- Save button
                    HH.div [ HP.classes [ HB.dFlex, HB.justifyContentEnd ] ]
                      [ HH.button
                          [ HP.classes [ HB.btn, HB.btnPrimary ]
                          , HE.onClick $ const SaveGroupSettings
                          , HP.disabled
                              ( state.settingsSaving
                                  || state.editedGroupName == ""
                              )
                          ]
                          [ if state.settingsSaving then
                              HH.span
                                [ HP.classes
                                    [ HB.spinnerBorder
                                    , HB.spinnerBorderSm
                                    , HB.me2
                                    ]
                                ]
                                []
                            else
                              HH.i
                                [ HP.classes [ H.ClassName "bi-check-lg", HB.me2 ]
                                ]
                                []
                          , HH.text $ translate (label :: _ "gs_saveSettings")
                              state.translator
                          ]
                      ]
                  ]
              ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get

      userAuth <- getAuthorizedUser state.groupID
      case userAuth of
        -- handleAppError already redirects to login for auth errors (401)
        -- and to Page404 for not-found errors (404).  Short-circuit here to
        -- avoid firing further API calls that would cascade-fail and race
        -- with the auth redirect, corrupting the ?redirect= query parameter.
        Left _ -> pure unit
        Right maybeUser ->
          if isNothing maybeUser then
            navigate Unauthorized
          else do
            userResult <- getUser
            case userResult of
              Left _ -> H.modify_ _ { isGroupAdmin = false, isSuperAdmin = false }
              Right user -> do
                let isSuperAdmin = isUserSuperadmin user
                let isGroupAdmin = user `isAdminOf` state.groupID || isSuperAdmin
                H.modify_ _
                  { isGroupAdmin = isGroupAdmin, isSuperAdmin = isSuperAdmin }

            -- Load group data
            loadGroupData

    Receive { context, input } -> do
      H.modify_ _
        { translator = fromFpoTranslator context
        , currentTab = tabFromString input.tab
        }

      state <- H.get
      when (state.groupID /= input.groupID) $ do
        H.modify_ _ { groupID = input.groupID }
        loadGroupData

    DoNothing -> pure unit

    -- Tab actions
    SwitchTab tab -> do
      state <- H.get
      navigate $ GroupRoute state.groupID $ case tab of
        DocumentsTab -> GroupDocuments
        MembersTab -> GroupMembers
        SettingsTab -> GroupSettings

    -- Document actions
    SetDocPage (P.Clicked p) -> H.modify_ _ { docPage = p }

    FilterDocuments f -> do
      H.modify_ _ { documentFilter = f }
      applyDocumentFilter

    ViewDocument docID -> do
      state <- H.get
      case state.modalState of
        NoModal -> navigate (editorRoute docID)
        _ -> pure unit

    RequestDeleteDocument docID event -> do
      H.liftEffect $ stopPropagation (MouseEvent.toEvent event)
      H.modify_ _ { modalState = DeleteDocumentModal docID }
      focusRef modalDeleteDocRef

    ConfirmDeleteDocument docID -> do
      state <- H.get
      deleteResponse <- deleteIgnore ("/documents/" <> show docID)
      case deleteResponse of
        Left _ -> pure unit
        Right _ -> do
          -- Reload documents
          documents <- getDocumentsQueryFromURL ("/docs?group=" <> show state.groupID)
          case documents of
            Right docs -> do
              H.modify_ _ { documents = DQ.getDocuments docs, modalState = NoModal }
            Left _ -> pure unit
      applyDocumentFilter

    RequestCreateDocument -> do
      H.modify_ _
        { newDocumentName = ""
        , modalState = CreateDocumentModal { waiting: false }
        }
      focusRef modalInputRef

    ConfirmCreateDocument -> do
      state <- H.get
      let newDocName = state.newDocumentName
      if newDocName == "" then
        updateStore $ Store.AddWarning
          (translate (label :: _ "gp_docNameNotEmpty") state.translator)
      else do
        setModalWaiting true
        let
          dto = NewDocumentCreateDto
            { groupID: state.groupID
            , title: newDocName
            }
        createResponse <- createNewDocument dto
        case createResponse of
          Left err -> do
            Store.addError $ DataError $ "Failed to create document: " <> show err
          Right h -> do
            H.modify_ _ { modalState = NoModal, newDocumentName = "" }
            now <- H.liftEffect nowDateTime
            let header = FD.getHeader h
            H.modify_ \s -> s
              { documents = header : s.documents
              , filteredDocuments = header : s.filteredDocuments
              , currentTime = Just now
              }
            updateStore $ Store.AddSuccess
              ( translate (label :: _ "gp_successfullyCreatedDocument")
                  state.translator
              )
            H.modify_ _ { documentFilter = "" }
            H.tell _docPagination unit $ P.SetPageQ 0
        setModalWaiting false

    ChangeCreateDocumentName name -> do
      H.modify_ _ { newDocumentName = name }

    -- Member actions
    SetMemberPage (P.Clicked p) -> H.modify_ _ { memberPage = p }

    FilterMembers f -> do
      H.modify_ _ { memberFilter = f }
      applyMemberFilter

    RequestRemoveMember userID -> do
      H.modify_ _ { modalState = RemoveMemberModal userID }
      focusRef modalRemoveMemberRef

    ConfirmRemoveMember userID -> do
      state <- H.get
      deleteResponse <- deleteIgnore
        ("/roles/" <> show state.groupID <> "/" <> userID)
      case deleteResponse of
        Left _ -> H.modify_ _ { modalState = NoModal }
        Right _ -> H.modify_ _ { modalState = NoModal }
      reloadGroupMembers

    SetUserRole member role -> do
      state <- H.get
      let userID = getUserInfoID member
      response <- changeRole state.groupID userID role
      case response of
        Left _ -> pure unit
        Right _ -> reloadGroupMembers

    NavigateToUserProfile userID -> do
      navigate $ UserProfile userID

    CancelModal -> do
      H.modify_ _ { modalState = NoModal }

    OpenAddMemberPopover -> do
      state <- H.get
      if state.showAddMemberPopover then when (null state.selectedUsersToAdd) $
        H.modify_ _ { showAddMemberPopover = false }
      else do
        H.modify_ _
          { showAddMemberPopover = true
          , loadingUsers = true
          , availableUsers = []
          , filteredAvailableUsers = []
          , selectedUsersToAdd = []
          , userSearchFilter = ""
          , addingMembers = false
          }

        usersResult <- getUsers
        case usersResult of
          Left _ -> H.modify_ _ { loadingUsers = false }
          Right users -> do
            s <- H.get
            let
              memberIds = fromMaybe [] $ map getUserInfoID <<< getGroupMembers <$>
                s.group
              available = filter (\u -> UOD.getID u `notElem` memberIds) users
            H.modify_ _
              { availableUsers = available
              , filteredAvailableUsers = available
              , loadingUsers = false
              }

    CloseAddMemberPopover -> do
      state <- H.get
      -- Only close if no users selected
      when (null state.selectedUsersToAdd) $
        H.modify_ _ { showAddMemberPopover = false }

    ForceCloseAddMemberPopover -> do
      H.modify_ _
        { showAddMemberPopover = false
        , selectedUsersToAdd = []
        , userSearchFilter = ""
        }

    FilterAvailableUsers searchText -> do
      H.modify_ _ { userSearchFilter = searchText }
      state <- H.get
      let
        filtered =
          if searchText == "" then state.availableUsers
          else filter
            ( \u ->
                contains (Pattern $ toLower searchText) (toLower $ UOD.getName u)
                  || contains (Pattern $ toLower searchText)
                    (toLower $ UOD.getEmail u)
            )
            state.availableUsers
      H.modify_ _ { filteredAvailableUsers = filtered }

    ToggleUserSelection userId -> do
      state <- H.get
      let
        newSelection =
          if userId `elem` state.selectedUsersToAdd then filter (_ /= userId)
            state.selectedUsersToAdd
          else userId : state.selectedUsersToAdd
      H.modify_ _ { selectedUsersToAdd = newSelection }

    ConfirmAddMembers -> do
      state <- H.get
      when (not $ null state.selectedUsersToAdd) $ do
        H.modify_ _ { addingMembers = true }
        -- Add each selected user as a Member
        _ <- traverse (\userId -> changeRole state.groupID userId Member)
          state.selectedUsersToAdd
        H.modify_ _
          { showAddMemberPopover = false
          , selectedUsersToAdd = []
          , addingMembers = false
          }

        reloadGroupMembers
        updateStore $ Store.AddSuccess
          (translate (label :: _ "gm_membersAddedSuccessfully") state.translator)

    CancelAddMembers -> do
      H.modify_ _
        { showAddMemberPopover = false
        , selectedUsersToAdd = []
        , userSearchFilter = ""
        }

    -- Settings actions
    ChangeGroupName name -> do
      H.modify_ _ { editedGroupName = name }

    ChangeGroupDescription desc -> do
      H.modify_ _ { editedGroupDescription = desc }

    SaveGroupSettings -> do
      state <- H.get
      when (state.editedGroupName /= "") $ do
        H.modify_ _ { settingsSaving = true }
        let
          -- Determine if description changed (compare to current group description)
          currentDesc = fromMaybe "" $ state.group >>= \g -> pure
            (getGroupDescription g)
          newDesc = state.editedGroupDescription
          -- Only include description in patch if it changed
          descPatch =
            if newDesc /= currentDesc then Just
              (if newDesc == "" then Nothing else Just newDesc)
            else Nothing
          -- Only include name in patch if it changed
          currentName = fromMaybe "" $ getGroupName <$> state.group
          namePatch =
            if state.editedGroupName /= currentName then Just state.editedGroupName
            else Nothing
          patch = GroupPatch
            { patchName: namePatch
            , patchDescription: descPatch
            }
        -- Only call API if something changed
        if namePatch == Nothing && descPatch == Nothing then
          H.modify_ _ { settingsSaving = false }
        else do
          result <- patchGroup state.groupID patch
          case result of
            Left _ -> H.modify_ _ { settingsSaving = false }
            Right _ -> do
              H.modify_ _ { settingsSaving = false }
              -- Reload group data to get updated info
              reloadGroupData
              updateStore $ Store.AddSuccess
                (translate (label :: _ "gs_settingsUpdated") state.translator)

  loadGroupData :: H.HalogenM State Action Slots output m Unit
  loadGroupData = do
    state <- H.get

    groupResult <- getGroup state.groupID
    case groupResult of
      Left _ -> pure unit
      Right grp -> do
        H.modify_ _
          { group = Just grp
          , filteredMembers = getGroupMembers grp
          , editedGroupName = getGroupName grp
          , editedGroupDescription = getGroupDescription grp
          }

    documents <- getDocumentsQueryFromURL ("/docs?group=" <> show state.groupID)
    case documents of
      Right docs -> do
        now <- H.liftEffect nowDateTime
        H.modify_ _
          { documents = DQ.getDocuments docs
          , filteredDocuments = DQ.getDocuments docs
          , currentTime = Just now
          }
      Left _ -> pure unit

  applyDocumentFilter :: H.HalogenM State Action Slots output m Unit
  applyDocumentFilter = do
    state <- H.get
    let
      filtered = filter
        ( \d -> contains (Pattern $ toLower state.documentFilter)
            (toLower $ DH.getName d)
        )
        state.documents
    H.modify_ _ { filteredDocuments = filtered }

  applyMemberFilter :: H.HalogenM State Action Slots output m Unit
  applyMemberFilter = do
    state <- H.get
    let allMembers = fromMaybe [] $ getGroupMembers <$> state.group
    let
      filtered = filter
        ( contains (Pattern $ toLower state.memberFilter) <<< toLower <<<
            getUserInfoName
        )
        allMembers
    H.modify_ _ { filteredMembers = filtered, memberPage = 0 }
    H.tell _memberPagination unit $ P.SetPageQ 0

  reloadGroupData :: H.HalogenM State Action Slots output m Unit
  reloadGroupData = do
    state <- H.get
    groupResult <- getGroup state.groupID
    case groupResult of
      Left _ -> pure unit
      Right grp -> do
        H.modify_ _
          { group = Just grp
          , filteredMembers = getGroupMembers grp
          , editedGroupName = getGroupName grp
          , editedGroupDescription = getGroupDescription grp
          }

  reloadGroupMembers :: H.HalogenM State Action Slots output m Unit
  reloadGroupMembers = do
    state <- H.get
    groupResult <- getGroup state.groupID
    case groupResult of
      Left _ -> pure unit
      Right grp -> do
        let members = getGroupMembers grp
        let
          newPage = min state.memberPage
            (P.calculatePageCount (length members) itemsPerPage - 1)
        H.tell _memberPagination unit $ P.SetPageQ newPage
        H.modify_ _
          { group = Just grp
          , filteredMembers = members
          , memberPage = max 0 newPage
          }
        applyMemberFilter

  setModalWaiting :: Boolean -> H.HalogenM State Action Slots output m Unit
  setModalWaiting w = do
    H.modify_ \s -> s
      { modalState = case s.modalState of
          CreateDocumentModal ms -> CreateDocumentModal ms { waiting = w }
          _ -> s.modalState
      }

  docNameFromID :: State -> Int -> String
  docNameFromID state docId =
    case head (filter (\dh -> DH.getID dh == docId) state.documents) of
      Just doc -> DH.getName doc
      Nothing -> "Unknown Document"
