-- | Admin group overview and management page.
-- |
-- | TODO: Implement the actual admin panel functionality.

module FPO.Page.Admin.Groups
  ( component
  ) where

import Prelude

import Data.Array (filter, length, replicate, slice, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Data.User (User(..))
import FPO.Page.HTML (addButton, addCard, addColumn, emptyEntryGen)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

type Group = String

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | SetPage P.Output
  | ChangeFilterGroupName String
  | ChangeCreateGroupName String
  | CreateGroup
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
  , groups :: Array Group
  , filteredGroups :: Array Group
  , groupNameCreate :: String
  , groupNameFilter :: String
  -- | This is used to store the group name for deletion confirmation.
  , requestDelete :: Maybe String
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
    , groups: groups
    , groupNameCreate: ""
    , groupNameFilter: ""
    , filteredGroups: groups
    , error: Nothing
    , requestDelete: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      $
        ( case state.requestDelete of
            Just groupName -> [ deleteConfirmationModal groupName ]
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
      -- TODO: Usually, we would fetch some data here (and handle
      --       the error of missing credentials), but for now,
      --       we just check if the user is an admin and redirect
      --       to a 404 page if not.
      user <- H.liftAff $ getUser
      case user of
        Nothing -> navigate Page404
        Just (User { fullUserIsSuperadmin }) ->
          if not fullUserIsSuperadmin then
            navigate Page404
          else
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
      let
        filteredGroups = filter (\g -> contains (Pattern s.groupNameFilter) g)
          s.groups
      H.modify_ _ { filteredGroups = filteredGroups }
    ChangeCreateGroupName group -> do
      H.modify_ _ { groupNameCreate = group }
    CreateGroup -> do
      newGroup <- H.gets _.groupNameCreate
      if newGroup == "" then H.modify_ _
        { error = Just "Group name cannot be empty." }
      else do
        H.modify_ \s -> s
          { error = Nothing
          , groups = newGroup : s.groups
          , groupNameCreate = ""
          }
        handleAction Filter
    RequestDeleteGroup groupName -> do
      H.modify_ _ { requestDelete = Just groupName }
    CancelDeleteGroup -> do
      H.modify_ \s -> s
        { error = Nothing
        , requestDelete = Nothing
        }
    ConfirmDeleteGroup groupName -> do
      H.modify_ \s -> s
        { error = Nothing
        , groups = filter (\g -> g /= groupName) s.groups
        , requestDelete = Nothing
        }
      handleAction Filter

  renderGroupManagement :: State -> H.ComponentHTML Action Slots m
  renderGroupManagement state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.colSm12, HB.colMd10, HB.colLg9 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "au_groupManagement") state.translator
              ]
          , renderGroupListView state
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
    addCard "List of Groups"
      [ HP.classes [ HB.col6, HB.me5 ], HP.style "max-height: 200px" ] $ HH.div_
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
          $ map createGroupEntry grps
              <> replicate (groupsPerPage - length grps)
                (emptyEntryGen [ buttonDeleteGroup "(not a group)" ])
      , HH.slot _pagination unit P.component ps SetPage
      ]
    where
    groupsPerPage = 7
    grps = slice (state.page * groupsPerPage) ((state.page + 1) * groupsPerPage)
      state.filteredGroups
    ps =
      { pages: P.calculatePageCount (length state.filteredGroups) groupsPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  -- Creates a form to create a new (dummy) group.
  renderNewGroupForm :: forall w. State -> HH.HTML w Action
  renderNewGroupForm state =
    addCard "Create New Group" [ HP.classes [ HB.col4 ] ] $ HH.div_
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
                  "Create"
                  (Just "bi-plus-circle")
                  (const CreateGroup)
              ]
          ]
      ]

  -- Creates a (dummy) group entry for the list.
  createGroupEntry :: forall w. String -> HH.HTML w Action
  createGroupEntry groupName =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.text groupName
      , buttonDeleteGroup groupName
      ]

  buttonDeleteGroup :: forall w. String -> HH.HTML w Action
  buttonDeleteGroup groupName =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HE.onClick (const $ RequestDeleteGroup groupName)
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
    HH.div_
      [ HH.div
          [ HP.classes
              [ HB.modal, HB.fade, HB.show ]
          , HP.id "deleteModal"
          , HP.attr (HH.AttrName "data-bs-backdrop") "static"
          , HP.attr (HH.AttrName "data-bs-keyboard") "false"
          , HP.attr (HH.AttrName "tabindex") "-1"
          , HP.attr (HH.AttrName "aria-labelledby") "deleteModalLabel"
          , HP.attr (HH.AttrName "aria-hidden") "false"
          , HP.style "display: block;"
          ]
          [ HH.div
              [ HP.classes [ HH.ClassName "modal-dialog" ] ]
              [ HH.div
                  [ HP.classes [ HH.ClassName "modal-content" ] ]
                  [ HH.div
                      [ HP.classes [ HH.ClassName "modal-header" ] ]
                      [ HH.h1
                          [ HP.classes
                              [ HH.ClassName "modal-title", HH.ClassName "fs-5" ]
                          , HP.id "deleteModalLabel"
                          ]
                          [ HH.text "Confirm Delete" ]
                      , HH.button
                          [ HP.type_ HP.ButtonButton
                          , HP.classes [ HB.btnClose ]
                          , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                          , HP.attr (HH.AttrName "aria-label") "Close"
                          , HE.onClick (const CancelDeleteGroup)
                          ]
                          []
                      ]
                  , HH.div
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
              ]
          ]
      , HH.div
          [ HP.classes
              [ HH.ClassName "modal-backdrop"
              , HH.ClassName "show"
              ]
          ]
          []
      ]

  -- A list of dummy groups for the admin panel.
  --
  -- This is just a placeholder and should be replaced with actual data
  -- from the backend.
  groups :: Array Group
  groups = do
    a <- [ "a", "b", "c" ]
    b <- [ "a", "b", "c" ]
    c <- [ "a", "b", "c" ]
    d <- [ "0", "1" ]
    pure ("Group " <> a <> b <> c <> d)
