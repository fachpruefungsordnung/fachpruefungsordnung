-- | Admin group overview and management page.
-- |
-- | TODO: Implement the actual admin panel functionality.

module FPO.Page.Admin.Groups
  ( component
  ) where

import Prelude

import Data.Array (filter, find, length, replicate, slice, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( LoadState(..)
  , addGroup
  , deleteIgnore
  , getUser
  , getUserGroups
  )
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.GroupDto
  ( GroupCreate(..)
  , GroupID
  , GroupOverview(..)
  , getGroupOverviewID
  , getGroupOverviewName
  )
import FPO.Dto.UserDto (isAdmin)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addButton, addCard, addColumn, addModal, emptyEntryGen)
import FPO.UI.Style as Style
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | SetPage P.Output
  | ChangeFilterGroupName String
  | ChangeCreateGroupName String
  | ChangeCreateGroupDescription String
  | RequestCreateGroup String
  | ConfirmCreateGroup
  | CancelCreateGroup
  -- | Used to set the group name for deletion confirmation
  -- | - before the user confirms the deletion using the modal.
  | RequestDeleteGroup String
  -- | Actually deletes the group after confirmation.
  | ConfirmDeleteGroup String
  | CancelDeleteGroup
  | Filter
  -- | Navigates to the documents of a specific group.
  -- | TODO: Because we have no group member page yet,
  -- |       we simply navigate to the group documents page.
  -- |       After the group member page is implemented, we
  -- |       could consider changing this action to instead
  -- |       navigate to the group member page. Might be more
  -- |       intuitive for the user.
  | NavigateToGroupDocuments GroupID

type State = FPOState
  ( page :: Int
  , groups :: LoadState (Array GroupOverview)
  , filteredGroups :: Array GroupOverview
  , groupNameCreate :: String
  , groupDescriptionCreate :: String
  , groupNameFilter :: String
  -- | This is used to store the group name for deletion confirmation.
  , requestDelete :: Maybe String
  , requestCreate :: Maybe String
  -- | Whether or not the user is waiting for a response from the server.
  -- | This is used to disable the UI while waiting for a response when
  -- | creating or deleting a group.
  , waiting :: Boolean
  )

-- | Admin panel page component.
component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Unit output m
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
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { translator: fromFpoTranslator context
    , page: 0
    , groups: Loading
    , groupNameCreate: ""
    , groupNameFilter: ""
    , groupDescriptionCreate: ""
    , filteredGroups: []
    , requestDelete: Nothing
    , requestCreate: Nothing
    , waiting: false
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      $
        ( case state.requestDelete of
            Just groupName ->
              [ deleteConfirmationModal state.translator groupName (const groupName)
                  CancelDeleteGroup
                  ConfirmDeleteGroup
                  (translate (label :: _ "common_theGroup") state.translator)
              ]
            Nothing -> case state.requestCreate of
              Just groupName -> [ createGroupModal state groupName ]
              Nothing -> []
        ) <>
          [ renderGroupManagement state
          ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      userResult <- getUser
      case userResult of
        Left _ -> navigate Page404 -- Ignore error, redirect to 404
        Right user ->
          when (not $ isAdmin user) $ navigate Page404

      groups <- getUserGroups
      case groups of
        Left _ -> pure unit -- TODO 
        Right g -> do
          H.modify_ _ { groups = Loaded g }
          handleAction Filter
          pure unit
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    ChangeFilterGroupName group -> do
      H.modify_ _ { groupNameFilter = group }
      handleAction Filter
    Filter -> do
      s <- H.get
      case s.groups of
        Loaded gs -> do
          let
            filteredGroups = filter
              (contains (Pattern s.groupNameFilter) <<< getGroupOverviewName)
              gs
          H.modify_ _ { filteredGroups = filteredGroups }
        Loading -> do
          updateStore $ Store.AddWarning
            (translate (label :: _ "admin_groups_stillLoading") s.translator)
    ChangeCreateGroupName group -> do
      H.modify_ _ { groupNameCreate = group }
    ChangeCreateGroupDescription desc -> do
      H.modify_ _ { groupDescriptionCreate = desc }
    RequestCreateGroup groupName -> do
      H.modify_ _ { requestCreate = Just groupName }
    ConfirmCreateGroup -> do
      -- Close modal
      H.modify_ _ { requestCreate = Nothing }

      newGroupName <- H.gets _.groupNameCreate
      s <- H.get
      if newGroupName == "" then updateStore $ Store.AddWarning
        (translate (label :: _ "admin_groups_notEmpty") s.translator)
      else do
        case s.groups of
          Loaded gs -> do
            setWaiting true
            response <- addGroup $ GroupCreate
              { groupCreateName: newGroupName
              , groupCreateDescription: s.groupDescriptionCreate
              }

            case response of
              Left _ -> pure unit
              Right newID ->
                H.modify_ _
                  { groups = Loaded $
                      GroupOverview
                        { groupOverviewName: newGroupName
                        , groupOverviewID: newID
                        }
                        : gs
                  , groupNameCreate = ""
                  }
            handleAction Filter
          Loading -> do
            updateStore $ Store.AddWarning
              (translate (label :: _ "admin_groups_stillLoading") s.translator)

      setWaiting false
    CancelCreateGroup -> do
      H.modify_ _
        { groupDescriptionCreate = ""
        , waiting = false
        , requestCreate = Nothing
        }
    RequestDeleteGroup groupName -> do
      H.modify_ _ { requestDelete = Just groupName }
    CancelDeleteGroup -> do
      H.modify_ _
        { requestDelete = Nothing
        }
    ConfirmDeleteGroup groupName -> do
      -- Close modal
      H.modify_ _ { requestDelete = Nothing }

      s <- H.get
      case s.groups of
        Loaded gs -> do
          let
            groupId = getGroupOverviewID <$> find
              (\g -> getGroupOverviewName g == groupName)
              gs

          case groupId of
            Nothing -> do
              updateStore $ Store.AddWarning
                (translate (label :: _ "admin_groups_errNotFound") s.translator)
            Just gId -> do
              setWaiting true
              res <- deleteIgnore $ "/groups/" <> show gId
              case res of
                Left _ -> pure unit
                Right _ -> do
                  H.modify_ _
                    { groups = Loaded $ filter
                        (\g -> getGroupOverviewName g /= groupName)
                        gs
                    }
              setWaiting false
          handleAction Filter
        Loading -> updateStore $ Store.AddWarning
          (translate (label :: _ "admin_groups_stillLoading") s.translator)
    NavigateToGroupDocuments gID -> do
      navigate $ ViewGroupDocuments { groupID: gID }

  -- | Specifies the waiting state.
  setWaiting :: Boolean -> H.HalogenM State Action Slots output m Unit
  setWaiting w = do
    H.modify_ _ { waiting = w }

  renderGroupManagement :: State -> H.ComponentHTML Action Slots m
  renderGroupManagement state =
    HH.div_
      [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $ translate (label :: _ "au_groupManagement")
              state.translator
          ]
      , case state.groups of
          Loading ->
            HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
              [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
          Loaded _ ->
            renderGroupListView state
      ]

  renderGroupListView :: State -> H.ComponentHTML Action Slots m
  renderGroupListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ renderGroupList state
      , renderNewGroupForm state
      ]

  -- Creates a list of (dummy) groups with pagination.
  renderGroupList :: State -> H.ComponentHTML Action Slots m
  renderGroupList state =
    HH.div [ HP.classes [ HB.col12, HB.colMd6, HB.colLg5, HB.meLg5, HB.mb4 ] ]
      [ addCard (translate (label :: _ "admin_groups_listOfGroups") state.translator)
          [] $
          HH.div_
            [ addColumn
                state.groupNameFilter
                ""
                ( translate (label :: _ "admin_groups_searchForGroups")
                    state.translator
                )
                "bi-search"
                HP.InputText
                ChangeFilterGroupName
            , HH.ul [ HP.classes [ HB.listGroup ] ]
                $ map (createGroupEntry state) grps
                    <> replicate (groupsPerPage - length grps)
                      (emptyEntryGen [ buttonDeleteGroup state "(not a group)" ])
            , HH.slot _pagination unit P.component ps SetPage
            ]
      ]
    where
    grps = slice (state.page * groupsPerPage) ((state.page + 1) * groupsPerPage)
      state.filteredGroups
    ps =
      { pages: P.calculatePageCount (length state.filteredGroups) groupsPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }
    groupsPerPage = 8

  -- Creates a form to create a new (dummy) group.
  renderNewGroupForm :: forall w. State -> HH.HTML w Action
  renderNewGroupForm state =
    HH.div [ HP.classes [ HB.col12, HB.colMd4, HB.colLg3, HB.mb4 ] ]
      [ addCard
          (translate (label :: _ "admin_groups_createNewGroup") state.translator)
          []
          $ HH.div_
              [ addColumn
                  state.groupNameCreate
                  ""
                  ( translate (label :: _ "admin_groups_groupName")
                      state.translator
                  )
                  "bi-people"
                  HP.InputText
                  ChangeCreateGroupName
              , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
                  [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
                      [ addButton
                          (not state.waiting)
                          (translate (label :: _ "common_create") state.translator)
                          (Just "bi-plus-circle")
                          (const (RequestCreateGroup state.groupNameCreate))
                      ]
                  ]
              ]
      ]

  -- Creates a (dummy) group entry for the list.
  createGroupEntry :: forall w. State -> GroupOverview -> HH.HTML w Action
  createGroupEntry state (GroupOverview g) =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.span
          [ HE.onClick (const $ NavigateToGroupDocuments g.groupOverviewID)
          , Style.popover $ translate
              (label :: _ "admin_groups_viewDocumentsPage")
              state.translator
          ]
          [ HH.text g.groupOverviewName
          ]
      , buttonDeleteGroup state g.groupOverviewName
      ]

  buttonDeleteGroup :: forall w. State -> String -> HH.HTML w Action
  buttonDeleteGroup state groupName =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HE.onClick (const $ RequestDeleteGroup groupName)
      , HP.disabled state.waiting
      ]
      [ HH.i [ HP.class_ $ HH.ClassName "bi-trash" ] [] ]

  -- Modal for creating a new group.
  createGroupModal :: forall w. State -> String -> HH.HTML w Action
  createGroupModal state groupName =
    addModal (translate (label :: _ "admin_groups_createGroup") state.translator)
      (const CancelCreateGroup) $
      [ HH.div
          [ HP.classes [ HB.modalBody ] ]
          [ HH.div
              [ HP.classes [ HB.mb3 ] ]
              [ HH.label
                  [ HP.for "groupName"
                  , HP.classes [ HH.ClassName "form-label" ]
                  ]
                  [ HH.text $ translate (label :: _ "admin_groups_groupName")
                      state.translator
                  ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ HH.ClassName "form-control" ]
                  , HP.id "groupName"
                  , HP.placeholder $ translate
                      (label :: _ "admin_groups_enterGroupName")
                      state.translator
                  , HP.value groupName
                  , HP.required true
                  , HE.onValueInput ChangeCreateGroupName
                  ]
              ]
          , HH.div
              [ HP.classes [ HB.mb3 ] ]
              [ HH.label
                  [ HP.for "groupDescription"
                  , HP.classes [ HH.ClassName "form-label" ]
                  ]
                  [ HH.text $ translate (label :: _ "admin_groups_desc")
                      state.translator
                  ]
              , HH.textarea
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.id "groupDescription"
                  , HP.placeholder $ translate
                      (label :: _ "admin_groups_enterGroupDesc")
                      state.translator
                  , HP.rows 3
                  , HE.onValueInput ChangeCreateGroupDescription
                  ]
              ]
          ]
      , HH.div
          [ HP.classes [ HB.modalFooter ] ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes
                  [ HB.btn, HB.btnSecondary ]
              , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
              , HE.onClick (const CancelCreateGroup)
              ]
              [ HH.text $ translate (label :: _ "common_cancel") state.translator ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnPrimary ]
              , HE.onClick (const ConfirmCreateGroup)
              , HP.disabled (state.waiting || state.groupNameCreate == "")
              ]
              [ HH.text $ translate (label :: _ "common_create") state.translator ]
          ]
      ]
