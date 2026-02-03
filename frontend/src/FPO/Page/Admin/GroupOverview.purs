-- | Unified Group Overview page showing documents and members in a tabbed interface.
-- |
-- | This page combines the former DocOverview and MemberOverview pages into a single
-- | view under /administration/groups/:groupID

module FPO.Page.Admin.GroupOverview
  ( component
  ) where

import Prelude

import Data.Array (filter, head, length, replicate, slice, (:))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), contains, toLower)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import FPO.Components.Pagination as P
import FPO.Data.AppError (AppError(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (changeRole, createNewDocument, deleteIgnore, getAuthorizedUser, getDocumentsQueryFromURL, getGroup, getUser)
import FPO.Data.Route (Route(..), groupOverview, addGroupMember)
import FPO.Data.Store as Store
import FPO.Data.Time (formatRelativeTime)
import FPO.Dto.CreateDocumentDto (NewDocumentCreateDto(..))
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.FullDocument as FD
import FPO.Dto.DocumentDto.Query as DQ
import FPO.Dto.GroupDto (GroupDto, GroupID, GroupMemberDto, getGroupMembers, getGroupName, getUserInfoID, getUserInfoName, getUserInfoRole, lookupUser)
import FPO.Dto.UserDto (UserID, isAdminOf, isUserSuperadmin)
import FPO.Dto.UserRoleDto (Role(..))
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addModal, emptyEntryGen)
import FPO.UI.Modals.DeleteModal (deleteConfirmationModal)
import FPO.UI.Style as Style
import FPO.Util (focusRef, handleKeyDown, singletonIf)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_docPagination = Proxy :: Proxy "docPagination"
_memberPagination = Proxy :: Proxy "memberPagination"

type Slots =
  ( docPagination :: H.Slot P.Query P.Output Unit
  , memberPagination :: H.Slot P.Query P.Output Unit
  )

data Tab = DocumentsTab | MembersTab

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
  | RequestDeleteDocument Int
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
  | NavigateToAddMember
  | NavigateToUserProfile UserID
  -- Modal actions
  | CancelModal

-- | Simple "state machine" for the modal system.
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
    }

  tabFromString :: Maybe String -> Tab
  tabFromString = case _ of
    Just "members" -> MembersTab
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
      , renderHeader state
      , renderTabs state
      , case state.currentTab of
          DocumentsTab -> renderDocumentsTab state
          MembersTab -> renderMembersTab state
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
              [ HH.i [ HP.classes [ H.ClassName "bi-file-earmark-text-fill", HB.me2 ] ] []
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
      ]

  -- ============== DOCUMENTS TAB ==============

  renderDocumentsTab :: State -> H.ComponentHTML Action Slots m
  renderDocumentsTab state =
    case state.group of
      Nothing -> renderLoading
      Just _ -> renderDocumentsList state

  renderDocumentsList :: State -> H.ComponentHTML Action Slots m
  renderDocumentsList state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.col12, HB.colLg10 ] ]
          [ HH.div [ HP.classes [ HB.card ] ]
              [ HH.div [ HP.classes [ HB.cardHeader, HB.dFlex, HB.justifyContentBetween, HB.alignItemsCenter ] ]
                  [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                      [ HH.text $ translate (label :: _ "gp_groupProjects") state.translator ]
                  , HH.button
                      [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                      , HE.onClick $ const RequestCreateDocument
                      , Style.popover $ translate (label :: _ "gp_createNewProject") state.translator
                      ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                      , HH.text $ translate (label :: _ "common_add") state.translator
                      ]
                  ]
              , HH.div [ HP.classes [ HB.cardBody ] ]
                  [ renderFilterInput
                      state.documentFilter
                      (translate (label :: _ "gp_searchProjects") state.translator)
                      FilterDocuments
                  , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                      $ map (renderDocumentEntry state) docs
                          <> replicate (itemsPerPage - length docs)
                            (emptyEntryGen [ emptyDocButtons ])
                  , HH.slot _docPagination unit P.component docPaginationProps SetDocPage
                  ]
              ]
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
    HH.li [ HP.classes [ HB.listGroupItem, HB.dFlex, HB.justifyContentBetween, HB.alignItemsCenter ] ]
      [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
          [ HH.span [ HP.classes [ HB.fwBold ] ]
              [ HH.text $ DH.getName doc ]
          , HH.small [ HP.classes [ HB.textMuted ] ]
              [ HH.text $ formatRelativeTime state.currentTime
                  (DD.docDateToDateTime (DH.getLastEdited doc))
              ]
          ]
      , HH.div [ HP.classes [ HB.dFlex, HB.gap2 ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick $ const $ ViewDocument (DH.getID doc)
              , Style.popover $ translate (label :: _ "home_editing") state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-pencil-fill" ] ] [] ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
              , HE.onClick $ const $ RequestDeleteDocument (DH.getID doc)
              , Style.popover $ translate (label :: _ "gp_removeProject") state.translator
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
          ]
      ]

  emptyDocButtons :: forall w. HH.HTML w Action
  emptyDocButtons =
    HH.div [ HP.classes [ HB.dFlex, HB.gap2 ] ]
      [ HH.button [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-pencil-fill" ] ] [] ]
      , HH.button [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-trash-fill" ] ] [] ]
      ]

  -- ============== MEMBERS TAB ==============

  renderMembersTab :: State -> H.ComponentHTML Action Slots m
  renderMembersTab state =
    case state.group of
      Nothing -> renderLoading
      Just _ -> renderMembersList state

  renderMembersList :: State -> H.ComponentHTML Action Slots m
  renderMembersList state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.col12, HB.colLg10 ] ]
          [ HH.div [ HP.classes [ HB.card ] ]
              [ HH.div [ HP.classes [ HB.cardHeader, HB.dFlex, HB.justifyContentBetween, HB.alignItemsCenter ] ]
                  [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                      [ HH.text $ translate (label :: _ "common_members") state.translator ]
                  , HH.button
                      [ HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                      , HE.onClick $ const NavigateToAddMember
                      , Style.popover $ translate (label :: _ "gm_addMember") state.translator
                      ]
                      [ HH.i [ HP.classes [ H.ClassName "bi-plus-lg", HB.me1 ] ] []
                      , HH.text $ translate (label :: _ "common_add") state.translator
                      ]
                  ]
              , HH.div [ HP.classes [ HB.cardBody ] ]
                  [ renderFilterInput
                      state.memberFilter
                      (translate (label :: _ "gm_searchMembers") state.translator)
                      FilterMembers
                  , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                      $ map (renderMemberEntry state) members
                          <> replicate (itemsPerPage - length members)
                            (emptyEntryGen [ emptyMemberButtons state ])
                  , HH.slot _memberPagination unit P.component memberPaginationProps SetMemberPage
                  ]
              ]
          ]
      ]
    where
    members = slice (state.memberPage * itemsPerPage) ((state.memberPage + 1) * itemsPerPage)
      state.filteredMembers
    memberPaginationProps =
      { pages: P.calculatePageCount (length state.filteredMembers) itemsPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  renderMemberEntry :: State -> GroupMemberDto -> H.ComponentHTML Action Slots m
  renderMemberEntry state member =
    HH.li [ HP.classes [ HB.listGroupItem, HB.dFlex, HB.justifyContentBetween, HB.alignItemsCenter ] ]
      [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
          [ HH.span [ HP.classes [ HB.fwBold ] ]
              [ HH.text $ getUserInfoName member ]
          ]
      , HH.div [ HP.classes [ HB.dFlex, HB.gap2, HB.alignItemsCenter ] ]
          [ -- Role toggle button group (only for group admins)
            if state.isGroupAdmin then
              renderRoleToggle state member
            else
              HH.span [ HP.classes [ HB.badge, HB.bgSecondary ] ]
                [ HH.text $ roleToString state (getUserInfoRole member) ]
          , -- Edit user button (only for superadmins)
            if state.isSuperAdmin then
              HH.button
                [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
                , HE.onClick $ const $ NavigateToUserProfile (getUserInfoID member)
                , Style.popover $ translate (label :: _ "admin_users_goToProfilePage") state.translator
                ]
                [ HH.i [ HP.classes [ H.ClassName "bi-person-fill" ] ] [] ]
            else
              HH.text ""
          , -- Leave/Remove button
            HH.button
              [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
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
              if currentRole == Admin then [ HB.btnPrimary ] else [ HB.btnOutlineSecondary ]
          , HE.onClick $ if currentRole /= Admin then const $ SetUserRole member Admin else const DoNothing
          ]
          [ HH.text "Admin" ]
      , HH.button
          [ HP.classes $ [ HB.btn ] <>
              if currentRole == Member then [ HB.btnPrimary ] else [ HB.btnOutlineSecondary ]
              , HE.onClick $ if currentRole /= Member then const $ SetUserRole member Member else const DoNothing
          ]
          [ HH.text $ translate (label :: _ "common_member") state.translator ]
      ]
    where
    currentRole = getUserInfoRole member

  emptyMemberButtons :: State -> forall w. HH.HTML w Action
  emptyMemberButtons state =
    HH.div [ HP.classes [ HB.dFlex, HB.gap2, HB.alignItemsCenter ] ]
      [ HH.div [ HP.classes [ HB.btnGroup, HB.btnGroupSm, HB.invisible ] ]
          [ HH.button [ HP.classes [ HB.btn, HB.btnOutlineSecondary ] ] [ HH.text "Admin" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnPrimary ] ]
              [ HH.text $ translate (label :: _ "common_member") state.translator ]
          ]
      , HH.button [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-person-fill" ] ] [] ]
      , HH.button [ HP.classes [ HB.btn, HB.btnOutlineWarning, HB.btnSm, HB.invisible ] ]
          [ HH.i [ HP.classes [ H.ClassName "bi-box-arrow-right" ] ] [] ]
      ]

  roleToString :: State -> Role -> String
  roleToString state = case _ of
    Admin -> "Admin"
    Member -> translate (label :: _ "common_member") state.translator

  -- ============== COMMON RENDERING ==============

  renderLoading :: H.ComponentHTML Action Slots m
  renderLoading =
    HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
      [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]

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

  createDocumentModal :: { waiting :: Boolean } -> State -> H.ComponentHTML Action Slots m
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
                  [ HH.text $ translate (label :: _ "gp_documentName") state.translator ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ HH.ClassName "form-control" ]
                  , HP.id "docName"
                  , HP.ref modalInputRef
                  , HP.placeholder $ translate (label :: _ "gp_enterDocumentName") state.translator
                  , HP.required true
                  , HE.onValueInput ChangeCreateDocumentName
                  ]
              ]
          ]
      , HH.div [ HP.classes [ HB.modalFooter ] ]
          ( singletonIf ms.waiting
              ( HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary, HB.me5 ] ] [] )
              <>
                [ HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnSecondary ]
                    , HE.onClick (const CancelModal)
                    , HP.disabled ms.waiting
                    ]
                    [ HH.text $ translate (label :: _ "common_cancel") state.translator ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnPrimary ]
                    , HE.onClick (const ConfirmCreateDocument)
                    , HP.disabled (state.newDocumentName == "" || ms.waiting)
                    ]
                    [ HH.text $ translate (label :: _ "common_create") state.translator ]
                ]
          )
      ]

  -- ============== ACTION HANDLING ==============

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get

      -- Check authorization
      userAuth <- getAuthorizedUser state.groupID
      case userAuth of
        Left _ -> pure unit
        Right maybeUser ->
          when (isNothing maybeUser) $ navigate Page404

      -- Get current user info for permission checks
      userResult <- getUser
      case userResult of
        Left _ -> H.modify_ _ { isGroupAdmin = false, isSuperAdmin = false }
        Right user -> do
          let isSuperAdmin = isUserSuperadmin user
          let isGroupAdmin = user `isAdminOf` state.groupID || isSuperAdmin
          H.modify_ _ { isGroupAdmin = isGroupAdmin, isSuperAdmin = isSuperAdmin }

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
      let tabStr = case tab of
            DocumentsTab -> Nothing
            MembersTab -> Just "members"
      navigate $ groupOverview state.groupID tabStr

    -- Document actions
    SetDocPage (P.Clicked p) -> H.modify_ _ { docPage = p }

    FilterDocuments f -> do
      H.modify_ _ { documentFilter = f }
      applyDocumentFilter

    ViewDocument docID -> do
      state <- H.get
      case state.modalState of
        NoModal -> navigate (Editor { docID })
        _ -> pure unit

    RequestDeleteDocument docID -> do
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
        updateStore $ Store.AddWarning "Document name cannot be empty."
      else do
        setModalWaiting true
        let dto = NewDocumentCreateDto
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
            updateStore $ Store.AddSuccess "Successfully created document"
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
      deleteResponse <- deleteIgnore ("/roles/" <> show state.groupID <> "/" <> userID)
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

    NavigateToAddMember -> do
      state <- H.get
      navigate $ addGroupMember state.groupID

    NavigateToUserProfile userID -> do
      navigate $ Profile { loginSuccessful: Nothing, userId: Just userID }

    CancelModal -> do
      H.modify_ _ { modalState = NoModal }

  -- ============== HELPER FUNCTIONS ==============

  loadGroupData :: H.HalogenM State Action Slots output m Unit
  loadGroupData = do
    state <- H.get

    -- Load group
    groupResult <- getGroup state.groupID
    case groupResult of
      Left _ -> pure unit
      Right grp -> do
        H.modify_ _
          { group = Just grp
          , filteredMembers = getGroupMembers grp
          }

    -- Load documents
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
        (\d -> contains (Pattern $ toLower state.documentFilter) (toLower $ DH.getName d))
        state.documents
    H.modify_ _ { filteredDocuments = filtered }

  applyMemberFilter :: H.HalogenM State Action Slots output m Unit
  applyMemberFilter = do
    state <- H.get
    let allMembers = fromMaybe [] $ getGroupMembers <$> state.group
    let
      filtered = filter
        (contains (Pattern $ toLower state.memberFilter) <<< toLower <<< getUserInfoName)
        allMembers
    H.modify_ _ { filteredMembers = filtered, memberPage = 0 }
    H.tell _memberPagination unit $ P.SetPageQ 0

  reloadGroupMembers :: H.HalogenM State Action Slots output m Unit
  reloadGroupMembers = do
    state <- H.get
    groupResult <- getGroup state.groupID
    case groupResult of
      Left _ -> pure unit
      Right grp -> do
        let members = getGroupMembers grp
        let newPage = min state.memberPage
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
