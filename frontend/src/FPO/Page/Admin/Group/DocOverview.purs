-- | Overview of Documents belonging to Group

-- Things to change in this file:
--   [x] always loading for group 1 (see initialize and ConfirmDeleteDocument)
--   [x] No connection to Backend yet
--   [ ] button for member overview not functional yet
--   [ ] many things same as in Home.purs or PageGroups.purs. Need to relocate reusable code fragments.
--   [ ] archive column should have checkboxes
--   [ ] move the creation modal to a separate file / component, and perhaps even to a separate page.

-- To change: The project/document structure between pages isn't standardized.
--            This must be changed. For now, the toDocument function translates as needed and makes up
--            missing data.

module FPO.Page.Admin.Group.DocOverview (component) where

import Prelude

import Affjax (printError)
import Data.Argonaut (decodeJson)
import Data.Array (filter, head, length, null, replicate, slice, (:))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.Pagination as P
import FPO.Components.Table.Head as TH
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( createDocument
  , deleteIgnore
  , getDocumentsFromURL
  , getGroup
  , getUser
  )
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.CreateDocumentDto (DocumentCreateDto(..))
import FPO.Dto.DocumentDto (DocumentHeader(..), DocumentID, getDHID, getDHName)
import FPO.Dto.GroupDto (GroupDto, GroupID, getGroupName)
import FPO.Dto.UserDto (isUserSuperadmin)
import FPO.Page.Home (formatRelativeTime)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn, addModal)
import FPO.UI.Style as Style
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

_tablehead = Proxy :: Proxy "tablehead"
_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( tablehead :: forall q. H.Slot q TH.Output Unit
  , pagination :: H.Slot P.Query P.Output Unit
  )

type Input = GroupID

-- TODO: Preliminary data type. So far, everything seems to use different data for documents, this should be changed.
--       Some fields are not (yet) available in the backend, so we need to use this data type to fill in the gaps.
--       As soon as we have decided on a common data type for documents, all this should be changed
--       (in the appropriate DocumentDTO module) and this preliminary data type should be replaced/removed.
type Document =
  { body ::
      { name :: String
      , text :: String
      }
  , header ::
      { updatedTs :: DateTime
      , id :: Int
      , archivedStatus :: Boolean
      }
  }

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | SetPage P.Output
  | ChangeFilterDocumentName String
  | Filter
  | ViewDocument DocumentID
  | ChangeSorting TH.Output
  | DoNothing
  | NavigateToMembers
  -- | Actions regarding deletion of documents.
  -- | Handles modal and deletion logic.
  | RequestDeleteDocument Int
  | ConfirmDeleteDocument Int
  -- | Actions regarding creating new documents.
  | RequestCreateDocument
  | ConfirmCreateDocument
  | CancelModal
  | ChangeCreateDocumentName String

-- | Simple "state machine" for the modal system.
data ModalState
  = NoModal
  | DeleteDocumentModal Int
  | CreateDocumentModal CreateDocumentModalState

-- | Local state of the "create document" modal.
type CreateDocumentModalState =
  { waiting :: Boolean
  , error :: Maybe String
  }

defaultCreateDocumentModalState :: CreateDocumentModalState
defaultCreateDocumentModalState = { waiting: false, error: Nothing }

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , groupID :: GroupID
  , group :: Maybe GroupDto
  , documents :: Array Document
  , filteredDocuments :: Array Document
  , currentTime :: Maybe DateTime
  , documentNameFilter :: String
  -- | This is used to store the document ID for deletion confirmation.
  , modalState :: ModalState
  , newDocumentName :: String
  )

-- | Admin panel page component.
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
    , page: 0
    , groupID: input
    , documents: []
    , documentNameFilter: ""
    , filteredDocuments: []
    , error: Nothing
    , modalState: NoModal
    , currentTime: Nothing
    , newDocumentName: ""
    , group: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      $
        ( case state.modalState of
            DeleteDocumentModal documentID ->
              [ deleteConfirmationModal state.translator documentID
                  (docNameFromID state)
                  CancelModal
                  ConfirmDeleteDocument
                  (translate (label :: _ "common_project") state.translator)
              ]
            CreateDocumentModal ms ->
              [ createDocumentModal ms state
              ]
            _ -> []
        ) <>
          [ renderDocumentManagement state
          , HH.div [ HP.classes [ HB.textCenter ] ]
              [ case state.error of
                  Just err -> HH.div
                    [ HP.classes [ HB.alert, HB.alertDanger, HB.mt5 ] ]
                    [ HH.text err ]
                  Nothing -> HH.text ""
              ]
          ]

  renderDocumentManagement :: State -> H.ComponentHTML Action Slots m
  renderDocumentManagement state =
    HH.div_
      [ HH.h2 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $
              translate (label :: _ "gp_groupProjects")
                state.translator <> " "
          , HH.span
              [ HP.classes
                  [ HB.textSecondary, HB.fwBolder, HB.dInlineBlock, HB.textWrap ]
              ]
              [ HH.text $ fromMaybe "" $ getGroupName <$> state.group ]
          ]
      , renderDocumentListView state
      ]

  renderDocumentListView :: State -> H.ComponentHTML Action Slots m
  renderDocumentListView state =
    HH.div [ HP.classes [ HB.row ] ]
      [ renderSideButtons state
      , renderDocumentsOverview state
      ]

  -- Renders the overview of projects for the user.
  renderDocumentsOverview :: State -> H.ComponentHTML Action Slots m
  renderDocumentsOverview state =
    HH.div [ HP.classes [ HB.col12, HB.colMd9, HB.colLg8 ] ]
      [ HH.div [ HP.classes [ HB.card, HB.bgLightSubtle ] ]
          [ HH.div [ HP.class_ HB.cardBody ] [ renderDocumentOverview state ] ]
      ]

  -- Search bar and list of projects.
  renderDocumentOverview :: State -> H.ComponentHTML Action Slots m
  renderDocumentOverview state =
    HH.div [ HP.classes [ HB.container ] ]
      [ HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
          [ HH.div [ HP.classes [ HB.col6 ] ]
              [ addColumn
                  state.documentNameFilter
                  ""
                  (translate (label :: _ "gp_searchProjects") state.translator)
                  "bi-search"
                  HP.InputText
                  ChangeFilterDocumentName
              ]
          , HH.div [ HP.classes [ HB.col12 ] ]
              [ renderDocumentList docs state ]
          , HH.slot _pagination unit P.component ps SetPage
          ]
      ]
    where
    -- Slice of the projects for the current page:
    docs = slice (state.page * 10) ((state.page + 1) * 10) state.filteredDocuments
    ps =
      { pages: P.calculatePageCount (length state.filteredDocuments) 10
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  -- Renders the list of projects.
  renderDocumentList :: Array Document -> State -> H.ComponentHTML Action Slots m
  renderDocumentList docs state =
    HH.table
      [ HP.classes [ HB.table, HB.tableHover, HB.tableBordered ] ]
      [ HH.colgroup_
          [ HH.col [ HP.style "width: 55%;" ] -- 'Title' column
          , HH.col [ HP.style "width: 35%;" ] -- 'Last Updated' column
          -- archiving feature not supported for now
          -- , HH.col [ HP.style "width: 25%;" ] -- 'Archived' column
          , HH.col [ HP.style "width: 10%;" ] -- 'Delete' column
          ]
      , HH.slot _tablehead unit TH.component
          { columns: tableCols, sortedBy: "Last Updated" }
          ChangeSorting
      , HH.tbody_ $
          if null docs then
            [ HH.tr []
                [ HH.td
                    [ HP.colSpan 3
                    , HP.classes [ HB.textCenter ]
                    ]
                    [ HH.i_ [ HH.text "No documents found" ] ]
                ]
            ]
          else
            ( map (renderDocumentRow state) docs
                <> replicate (10 - length docs) (emptyDocumentRow state)
            ) -- Fill up to 10 rows
      ]
    where
    tableCols = TH.createTableColumns
      [ { title: "Title"
        , style: Just TH.Alpha
        }
      , { title: "Last Updated"
        , style: Just TH.Numeric
        }
      -- archiving feature not supported for now
      {-       , { title: "Archived?"
      , style: Nothing
      } -}
      , { title: ""
        , style: Nothing
        }
      ]

  -- Renders a single project row in the table.
  renderDocumentRow :: forall w. State -> Document -> HH.HTML w Action
  renderDocumentRow state document =
    HH.tr
      [ HE.onClick $ const $ ViewDocument document.header.id
      , HP.style "cursor: pointer;"
      ]
      [ HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text document.body.name ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ formatRelativeTime state.currentTime document.header.updatedTs ]
      -- archiving feature not supported for now
      {-       , HH.td [ HP.classes [ HB.textCenter ] ]
      [ HH.text (show document.header.archivedStatus) ] -}
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ buttonDeleteDocument state document.header.id ]
      ]

  -- Renders an empty project row for padding.
  emptyDocumentRow :: forall w. State -> HH.HTML w Action
  emptyDocumentRow state =
    HH.tr []
      [ HH.td
          [ HP.colSpan 3
          , HP.classes [ HB.textCenter ]
          ]
          [ HH.div [ HP.class_ HB.invisible ]
              [ HH.text $ "Empty Row", buttonDeleteDocument state (-1) ]
          ]
      ]

  renderSideButtons :: forall w. State -> HH.HTML w Action
  renderSideButtons state =
    HH.div [ HP.classes [ HB.colMd3, HB.colLg2, HB.col12, HB.mb3 ] ]
      [ -- The grid layout allows for vertical button stacking on bigger screens
        -- and horizontal alignment on smaller screens, just above the document list.
        HH.div
          [ HP.classes [ HB.dFlex, HB.dMdGrid, HB.justifyContentCenter, HB.gap2 ] ]
          [ renderToMemberButton state
          , renderCreateDocButton state
          ]
      ]

  renderToMemberButton :: forall w. State -> HH.HTML w Action
  renderToMemberButton state =
    HH.button
      [ Style.cyanStyle
      , HE.onClick (const NavigateToMembers)
      ]
      [ HH.text $ translate (label :: _ "common_members") state.translator ]

  renderCreateDocButton :: forall w. State -> HH.HTML w Action
  renderCreateDocButton state =
    HH.button
      [ Style.cyanStyle
      , HE.onClick (const $ RequestCreateDocument)
      ]
      [ HH.text $ translate (label :: _ "gp_newProject") state.translator ]

  buttonDeleteDocument :: forall w. State -> Int -> HH.HTML w Action
  buttonDeleteDocument state documentID =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HE.onClick (const $ RequestDeleteDocument documentID)
      , Style.popover
          ( translate (label :: _ "gp_removeProject") state.translator
          )
      ]
      [ HH.i [ HP.class_ $ HH.ClassName "bi-trash" ] [] ]

  createDocumentModal
    :: forall w. CreateDocumentModalState -> State -> HH.HTML w Action
  createDocumentModal ms state =
    addModal (translate (label :: _ "gp_createNewProject") state.translator)
      (const CancelModal) $
      [ HH.div
          [ HP.classes [ HB.modalBody ] ]
          [ HH.div
              [ HP.classes [ HB.mb3 ] ]
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
                  , HP.placeholder $ translate
                      (label :: _ "gp_enterDocumentName")
                      state.translator
                  , HP.required true
                  , HE.onValueInput ChangeCreateDocumentName
                  ]
              ]
          ]
      , HH.div
          [ HP.classes [ HB.modalFooter ] ]
          ( ( if ms.waiting then
                [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary, HB.me5 ] ]
                    []
                ]
              else
                case ms.error of
                  Just err ->
                    [ HH.div [ HP.classes [ HB.alert, HB.alertDanger, HB.w100 ] ]
                        [ HH.text err ]
                    ]
                  Nothing -> []
            )
              <>
                [ HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes
                        [ HB.btn, HB.btnSecondary ]
                    , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                    , HE.onClick (const CancelModal)
                    , HP.disabled (ms.waiting)
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

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> isUserSuperadmin <$> u)) $
        navigate Page404
      now <- H.liftEffect nowDateTime
      H.modify_ _
        { documents = []
        , currentTime = Just now
        }
      s <- H.get
      documents <- liftAff
        (getDocumentsFromURL ("/groups/" <> show s.groupID <> "/documents"))
      case documents of
        Just docs -> do
          H.modify_ _ { documents = toDocs now docs }
        Nothing -> do
          navigate Page404

      g <- liftAff $ getGroup s.groupID
      case g of
        Just group -> do
          H.modify_ _ { group = Just group }
        Nothing -> do
          navigate Page404

      handleAction Filter
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    ChangeFilterDocumentName doc -> do
      H.modify_ _ { documentNameFilter = doc }
      handleAction Filter
    Filter -> do
      s <- H.get
      let
        filteredDocs = filter
          (\d -> contains (Pattern s.documentNameFilter) d.body.name)
          s.documents
      H.modify_ _ { filteredDocuments = filteredDocs }
    RequestCreateDocument -> do
      H.modify_ _
        { newDocumentName = ""
        , modalState = CreateDocumentModal defaultCreateDocumentModalState
        }
    ConfirmCreateDocument -> do
      s <- H.get
      let newDocName = s.newDocumentName
      if newDocName == "" then
        H.modify_ _ { error = Just "Document name cannot be empty." }
      else do
        log ("Trying to create new document with name \"" <> newDocName <> "\"")

        let
          dto = DocumentCreateDto
            { documentCreateGroupId: s.groupID
            , documentCreateName: newDocName
            }

        setModalWaiting true

        createResponse <- liftAff (createDocument dto)
        case createResponse of
          Left err -> do
            setModalError $ printError err
          Right result -> do
            case decodeJson result.body of
              Left err -> do
                setModalError $ "Error decoding response: " <> show err
              Right (DH h) -> do
                H.modify_ _ { modalState = NoModal, newDocumentName = "" }
                log "Created Document"

                now <- H.liftEffect nowDateTime
                let
                  newDoc =
                    { body:
                        { name: h.name
                        , text: "This text should not be read, else there is an error"
                        }
                    , header: { updatedTs: now, id: h.id, archivedStatus: false }
                    }
                H.modify_ \s' -> s'
                  { documents = newDoc : s'.documents
                  , filteredDocuments = newDoc : s'.filteredDocuments
                  , currentTime = Just now
                  }

                -- Reset the page view
                H.modify_ _ { documentNameFilter = "" }
                H.tell _pagination unit $ P.SetPageQ 0

        -- Either the document was created successfully and the modal is closed,
        -- or an error occurred and is currently displayed. The user can then
        -- interact with the modal again (try to create another document or cancel).
        setModalWaiting false
      pure unit
    RequestDeleteDocument documentID -> do
      H.modify_ _ { modalState = DeleteDocumentModal documentID }
    CancelModal -> do
      H.modify_ \s -> s
        { error = Nothing
        , modalState = NoModal
        }
    ChangeCreateDocumentName docName -> do
      H.modify_ _ { newDocumentName = docName }
    ConfirmDeleteDocument docID -> do
      deleteResponse <- liftAff (deleteIgnore ("/documents/" <> show docID))
      case deleteResponse of
        Left err -> do
          H.modify_ \s -> s
            { error = Just (printError err)
            }
        Right _ -> do
          log "Deleted Document"
          now <- H.liftEffect nowDateTime
          s <- H.get
          documents <- liftAff
            (getDocumentsFromURL ("/groups/" <> show s.groupID <> "/documents"))
          case documents of
            Just docs -> do
              H.modify_ _
                { error = Nothing
                , documents = toDocs now docs
                , modalState = NoModal
                }
            Nothing -> do
              navigate Login
      handleAction Filter
    ViewDocument documentID -> do
      s <- H.get
      case s.modalState of
        NoModal -> do
          log ("Routing to editor for project " <> ((docNameFromID s) documentID))
          navigate (Editor { docID: documentID })
        _ ->
          pure unit
    ChangeSorting (TH.Clicked title order) -> do
      state <- H.get

      -- Sorting logic based on the clicked column title:
      docs <- pure $ case title of
        "Title" ->
          TH.sortByF
            order
            (\a b -> compare a.body.name b.body.name)
            state.documents
        "Last Updated" ->
          TH.sortByF
            (TH.toggleSorting order) -- The newest project should be first.
            (\a b -> compare a.header.updatedTs b.header.updatedTs)
            state.documents
        _ -> state.documents -- Ignore other columns.

      H.modify_ _ { documents = docs }
      handleAction Filter

      -- After changing the sorting, tell the pagination component
      -- to reset to the first page:
      H.tell _pagination unit $ P.SetPageQ 0
    DoNothing ->
      pure unit
    NavigateToMembers -> do
      log "Routing to member overview"
      s <- H.get
      navigate (ViewGroupMembers { groupID: s.groupID })

  -- | Sets the modal waiting state if the current modal has waiting capabilities.
  -- | This is used to disable buttons and show a loading state, prohibiting further actions.
  setModalWaiting :: Boolean -> H.HalogenM State Action Slots output m Unit
  setModalWaiting w = do
    H.modify_ \s -> s
      { modalState = case s.modalState of
          CreateDocumentModal ms -> CreateDocumentModal ms { waiting = w }
          _ -> s.modalState
      }

  -- | Sets an error message in the modal state if the current modal supports error messages.
  setModalError :: String -> H.HalogenM State Action Slots output m Unit
  setModalError err = do
    H.modify_ \s -> s
      { modalState = case s.modalState of
          CreateDocumentModal ms -> CreateDocumentModal ms { error = Just err }
          _ -> s.modalState
      }

  -- transforms Document data from backend into data for this page
  toDocs :: DateTime -> Array DocumentHeader -> Array Document
  toDocs now = map
    ( \doc ->
        { body:
            { name: getDHName doc
            , text: "This text should not be read, else there is an error"
            }
        , header:
            { updatedTs: now
            , id: getDHID doc
            , archivedStatus: false
            }
        }
    )

  docNameFromID :: State -> Int -> String
  docNameFromID state id =
    case head (filter (\doc -> doc.header.id == id) state.documents) of
      Just doc -> doc.body.name
      Nothing -> "Unknown Name"
