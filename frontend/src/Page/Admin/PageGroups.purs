-- | Admin group overview and management page.
-- |
-- | TODO: Implement the actual admin panel functionality.

module FPO.Page.Admin.Groups
  ( component
  ) where

import Prelude

import Affjax (Error)
import Data.Argonaut.Decode.Decoders (decodeInt)
import Data.Array (filter, find, length, replicate, slice, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( LoadState(..)
  , addGroup
  , deleteIgnore
  , getGroups
  , getStatusCode
  , getUser
  , printError
  )
import FPO.Data.Route (Route(..))
import FPO.Data.Store (Group)
import FPO.Data.Store as Store
import FPO.Page.HTML (addButton, addCard, addColumn, addModal, emptyEntryGen)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff, liftEffect)
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

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , groups :: LoadState (Array Group)
  , filteredGroups :: Array Group
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
    , error: Nothing
    , requestDelete: Nothing
    , requestCreate: Nothing
    , waiting: false
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      $
        ( case state.requestDelete of
            Just groupName -> [ deleteConfirmationModal groupName ]
            Nothing -> case state.requestCreate of
              Just groupName -> [ createGroupModal state groupName ]
              Nothing -> []
        ) <>
          [ renderGroupManagement state
          , HH.div [ HP.classes [ HB.textCenter ] ]
              [ case state.error of
                  Just err -> HH.div
                    [ HP.classes [ HB.alert, HB.alertDanger, HB.mt5 ] ]
                    [ HH.text err ]
                  Nothing -> HH.text ""
              ]
          ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $
        navigate Page404

      g <- liftAff getGroups
      case g of
        Just gs -> do
          H.modify_ _ { groups = Loaded gs }
          handleAction Filter
          pure unit
        Nothing -> do
          navigate Login
          pure unit
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    ChangeFilterGroupName group -> do
      H.modify_ _ { groupNameFilter = group, error = Nothing }
      handleAction Filter
    Filter -> do
      s <- H.get
      case s.groups of
        Loaded gs -> do
          let
            filteredGroups = filter
              (\g -> contains (Pattern s.groupNameFilter) g.groupOverviewName)
              gs
          H.modify_ _ { filteredGroups = filteredGroups }
        Loading -> do
          H.modify_ _ { error = Just "Groups are still loading." }
    ChangeCreateGroupName group -> do
      H.modify_ _ { groupNameCreate = group, error = Nothing }
    ChangeCreateGroupDescription desc -> do
      H.modify_ _ { groupDescriptionCreate = desc, error = Nothing }
    RequestCreateGroup groupName -> do
      H.modify_ _ { requestCreate = Just groupName, error = Nothing }
    ConfirmCreateGroup -> do
      -- Close modal
      H.modify_ _ { requestCreate = Nothing }

      newGroupName <- H.gets _.groupNameCreate
      s <- H.get
      if newGroupName == "" then H.modify_ _
        { error = Just "Group name cannot be empty." }
      else do
        case s.groups of
          Loaded gs -> do
            setWaiting true
            response <- handleAuthReq (Just AdminViewGroups) $ liftAff $ addGroup
              { groupCreateName: newGroupName
              , groupCreateDescription: s.groupDescriptionCreate
              }

            case response of
              Left err -> do
                H.modify_ _
                  { error = Just $ printError "Error creating group" err
                  }
              Right content -> do
                case decodeInt content.body of
                  Left err -> do
                    H.modify_ _
                      { error = Just $ "Error decoding group ID: " <> show err
                      }
                  Right newId -> do
                    H.modify_ _
                      { error = Nothing
                      , groups = Loaded $ { groupOverviewName: newGroupName, groupOverviewId: newId }
                          : gs
                      , groupNameCreate = ""
                      }
            handleAction Filter
          Loading -> do
            H.modify_ _ { error = Just "Groups are still loading." }

      setWaiting false
    CancelCreateGroup -> do
      H.modify_ _
        { error = Nothing
        , groupDescriptionCreate = ""
        , waiting = false
        , requestCreate = Nothing
        }
    RequestDeleteGroup groupName -> do
      H.modify_ _ { requestDelete = Just groupName }
    CancelDeleteGroup -> do
      H.modify_ _
        { error = Nothing
        , requestDelete = Nothing
        }
    ConfirmDeleteGroup groupName -> do
      -- Close modal
      H.modify_ _ { requestDelete = Nothing }

      s <- H.get
      case s.groups of
        Loaded gs -> do
          let groupId = _.groupOverviewId <$> find (\g -> g.groupOverviewName == groupName) gs

          case groupId of
            Nothing -> do
              H.modify_ _ { error = Just "Group not found." }
            Just gId -> do
              setWaiting true
              res <- liftAff $ deleteIgnore $ "/groups/" <> show gId
              case res of
                Left err -> do
                  H.modify_ _ { error = Just $ printError "Error deleting group" err }
                Right status -> do
                  if getStatusCode status /= 200 then
                    H.modify_ _
                      { error = Just "Failed to delete group."
                      , requestDelete = Nothing
                      }
                  else do
                    liftEffect $ log $ "Deleted group: " <> groupName
                    H.modify_ _
                      { error = Nothing
                      , groups = Loaded $ filter (\g -> g.groupOverviewName /= groupName) gs
                      }
              setWaiting false
          handleAction Filter
        Loading -> do
          H.modify_ _ { error = Just "Groups are still loading." }

  -- | Specifies the waiting state.
  setWaiting :: Boolean -> H.HalogenM State Action Slots output m Unit
  setWaiting w = do
    H.modify_ _ { waiting = w }

  renderGroupManagement :: State -> H.ComponentHTML Action Slots m
  renderGroupManagement state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.colSm12, HB.colMd10, HB.colLg9 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "au_groupManagement") state.translator
              ]
          , case state.groups of
              Loading ->
                HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
                  [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
              Loaded _ ->
                renderGroupListView state
          ]
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
    addCard "List of Groups" [ HP.classes [ HB.col5, HB.me5 ] ] $ HH.div_
      [ HH.div [ HP.classes [ HB.col12 ] ]
          [ addColumn
              state.groupNameFilter
              ""
              "Search for Groups"
              "bi-search"
              HP.InputText
              ChangeFilterGroupName
          ]
      , HH.ul [ HP.classes [ HB.listGroup ] ]
          $ map (createGroupEntry state) grps
              <> replicate (groupsPerPage - length grps)
                (emptyEntryGen [ buttonDeleteGroup state "(not a group)" ])
      , HH.slot _pagination unit P.component ps SetPage
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
    addCard "Create New Group" [ HP.classes [ HB.col3 ] ] $ HH.div_
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.groupNameCreate
              ""
              "Group Name"
              "bi-people"
              HP.InputText
              ChangeCreateGroupName
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  (not state.waiting)
                  "Create"
                  (Just "bi-plus-circle")
                  (const (RequestCreateGroup state.groupNameCreate))
              ]
          ]
      ]

  -- Creates a (dummy) group entry for the list.
  createGroupEntry :: forall w. State -> Group -> HH.HTML w Action
  createGroupEntry state group =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.text group.groupOverviewName
      , buttonDeleteGroup state group.groupOverviewName
      ]

  buttonDeleteGroup :: forall w. State -> String -> HH.HTML w Action
  buttonDeleteGroup state groupName =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HE.onClick (const $ RequestDeleteGroup groupName)
      , HP.disabled state.waiting
      ]
      [ HH.i [ HP.class_ $ HH.ClassName "bi-trash" ] [] ]

  -- Modal for confirming the deletion of a group.
  --
  -- TODO: This only statically shows the modal whenever the user requests
  --       to delete a group. Because of this binary show/hide logic,
  --       we can't use fancy features like modal animations (fade-in, etc.).
  --       Instead, we could use JSS to toggle the modal visibility, but this
  --       would of course require external JavaScript code.
  --         See https://getbootstrap.com/docs/5.3/components/modal/.
  deleteConfirmationModal :: forall w. String -> HH.HTML w Action
  deleteConfirmationModal groupName =
    addModal "Confirm Delete" (const CancelDeleteGroup) $
      [ HH.div
          [ HP.classes [ HB.modalBody ] ]
          [ HH.text
              ( "Are you sure you want to delete group " <> groupName <>
                  "?"
              )
          ]
      , HH.div
          [ HP.classes [ HB.modalFooter ] ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes
                  [ HB.btn, HB.btnSecondary ]
              , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
              , HE.onClick (const CancelDeleteGroup)
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnDanger ]
              , HE.onClick (const $ ConfirmDeleteGroup groupName)
              ]
              [ HH.text "Delete" ]
          ]
      ]

  -- Modal for creating a new group.
  createGroupModal :: forall w. State -> String -> HH.HTML w Action
  createGroupModal state groupName =
    addModal "Create Group" (const CancelCreateGroup) $
      [ HH.div
          [ HP.classes [ HB.modalBody ] ]
          [ HH.div
              [ HP.classes [ HB.mb3 ] ]
              [ HH.label
                  [ HP.for "groupName"
                  , HP.classes [ HH.ClassName "form-label" ]
                  ]
                  [ HH.text "Group Name" ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ HH.ClassName "form-control" ]
                  , HP.id "groupName"
                  , HP.placeholder "Enter group name"
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
                  [ HH.text "Description" ]
              , HH.textarea
                  [ HP.classes [ HH.ClassName "form-control" ]
                  , HP.id "groupDescription"
                  , HP.placeholder "Enter group description (optional)"
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
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnPrimary ]
              , HE.onClick (const ConfirmCreateGroup)
              , HP.disabled (state.waiting || state.groupNameCreate == "")
              ]
              [ HH.text "Create" ]
          ]
      ]

  -- | Requests can fail because of missing credentials
  -- | (e.g., if the user is not logged in). This helper
  -- | function that wraps around these requests and handles
  -- | authentication errors, e.g., by redirecting to the
  -- | login page.
  handleAuthReq
    :: forall m' a
     . MonadAff m'
    => Navigate m'
    => MonadStore Store.Action Store.Store m'
    => Maybe Route
    -> m' (Either Error a)
    -> m' (Either Error a)
  handleAuthReq mtarget request = do
    result <- request
    case result of
      Right _ -> pure result
      Left error ->
        if isAuthenticationError error then do
          updateStore $ Store.SetLoginRedirect mtarget
          navigate Login
          pure result
        else pure result

  -- Helper to detect auth errors.
  -- This is not a robust solution, but it works for now
  -- and (only) for JSON responses.
  isAuthenticationError :: Error -> Boolean
  isAuthenticationError err =
    -- Just a simple and stupid check for common auth error messages.
    contains (Pattern "Not allowe") (printError "" err)
--                   not a typo!
