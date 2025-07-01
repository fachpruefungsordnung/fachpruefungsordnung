-- | Overview of Documents belonging to Group

-- Things to change in this file:
-- No connection to Backend yet
-- different Docs lead to same editor
-- both buttons not funtional yet
-- many things same as in Home.purs or PageGroups.purs. Need to relocate reusable code fragments.
-- archive column should have checkboxes

module FPO.Page.Admin.DocOverview (component) where

import Prelude

-- | Copied over. Redundant imports to be removed later
import Data.Array (filter, head, length, null, replicate, slice, (..))
import Data.DateTime (DateTime)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Days(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.Pagination as P
import FPO.Components.Table.Head as TH
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.HTML (addCard, addColumn)
import FPO.Page.Home (adjustDateTime, formatRelativeTime)
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

_tablehead = Proxy :: Proxy "tablehead"
_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( tablehead :: forall q. H.Slot q TH.Output Unit
  , pagination :: H.Slot P.Query P.Output Unit
  )

type Input = Int
type GroupID = Int

data Sorting = TitleAsc | TitleDesc | LastUpdatedAsc | LastUpdatedDesc

-- preliminary data type. So far everything seems to use different data for documents, this should be changed.
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
  | CreateDocument
  -- | Used to set the document name for deletion confirmation
  -- | before the user confirms the deletion using the modal.
  | RequestDeleteDocument Int
  -- | Actually deletes the document after confirmation.
  | ConfirmDeleteDocument Int
  | CancelDeleteDocument
  | Filter
  | ViewDocument Int
  | ChangeSorting TH.Output
  | DoNothing

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , groupID :: GroupID
  , documents :: Array Document
  , filteredDocuments :: Array Document
  , currentTime :: Maybe DateTime
  , documentNameFilter :: String
  -- | This is used to store the document ID for deletion confirmation.
  , requestDelete :: Maybe Int
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
    , requestDelete: Nothing
    , currentTime: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      $
        ( case state.requestDelete of
            Just documentID ->
              [ deleteConfirmationModal documentID (docNameFromID state)
                  CancelDeleteDocument
                  ConfirmDeleteDocument
                  "document"
              ]
            Nothing -> []
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
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.colSm12, HB.colMd10, HB.colLg9 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "au_documentManagement")
                  state.translator
              ]
          , renderDocumentListView state
          ]
      ]

  renderDocumentListView :: State -> H.ComponentHTML Action Slots m
  renderDocumentListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ renderSideButtons
      , renderDocumentsOverview state
      ]

  -- Renders the overview of projects for the user.
  renderDocumentsOverview :: State -> H.ComponentHTML Action Slots m
  renderDocumentsOverview state =
    HH.div [ HP.classes [ HB.col9, HB.justifyContentCenter ] ]
      [ addCard
          (translate (label :: _ "au_groupDocuments") state.translator)
          [ HP.classes [ HB.colSm11, HB.colMd10, HB.colLg8 ] ]
          (renderDocumentOverview state)
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
                  "Search for Documents"
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
          [ HH.col [ HP.style "width: 40%;" ] -- 'Title' column
          , HH.col [ HP.style "width: 25%;" ] -- 'Last Updated' column
          , HH.col [ HP.style "width: 25%;" ] -- 'Archived' column
          , HH.col [ HP.style "width: 10%;" ] -- 'Delete' column
          ]
      , HH.slot _tablehead unit TH.component tableCols ChangeSorting
      , HH.tbody_ $
          if null docs then
            [ HH.tr []
                [ HH.td
                    [ HP.colSpan 4
                    , HP.classes [ HB.textCenter ]
                    ]
                    [ HH.i_ [ HH.text "No documents found" ] ]
                ]
            ]
          else
            ( map (renderDocumentRow state) docs
                <> replicate (10 - length docs) emptyDocumentRow
            ) -- Fill up to 5 rows
      ]
    where
    tableCols = TH.createTableColumns
      [ { title: "Title"
        , style: Just TH.Alpha
        }
      , { title: "Last Updated"
        , style: Just TH.Numeric
        }
      , { title: "Archived?"
        , style: Nothing
        }
      , { title: "Delete?"
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
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text (show document.header.archivedStatus) ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ buttonDeleteDocument document.header.id ]
      ]

  -- Renders an empty project row for padding.
  emptyDocumentRow :: forall w. HH.HTML w Action
  emptyDocumentRow =
    HH.tr []
      [ HH.td
          [ HP.colSpan 4
          , HP.classes [ HB.textCenter, HB.invisible ]
          ]
          [ HH.text $ "Empty Row", buttonDeleteDocument (-1) ]
      ]

  renderSideButtons :: forall w. HH.HTML w Action
  renderSideButtons =
    HH.div [ HP.classes [ HB.col, HB.justifyContentCenter ] ]
      [ renderToMemberButton
      , renderCreateDocButton
      ]

  renderToMemberButton :: forall w. HH.HTML w Action
  renderToMemberButton =
    HH.div [ HP.classes [ HB.inputGroup ] ]
      [ HH.button
          [ HP.classes [ HB.btn, HB.btnOutlineInfo, HB.btnLg, HB.p4, HB.textDark ]
          , HE.onClick (const $ DoNothing)
          ]
          [ HH.text "Members" ]
      ]

  renderCreateDocButton :: forall w. HH.HTML w Action
  renderCreateDocButton =
    HH.div [ HP.classes [ HB.inputGroup ] ]
      [ HH.button
          [ HP.classes
              [ HB.btn, HB.btnOutlineInfo, HB.btnLg, HB.p4, HB.mt5, HB.textDark ]
          , HE.onClick (const $ DoNothing)
          ]
          [ HH.text "Create Document" ]
      ]

  buttonDeleteDocument :: forall w. Int -> HH.HTML w Action
  buttonDeleteDocument documentID =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HE.onClick (const $ RequestDeleteDocument documentID)
      ]
      [ HH.i [ HP.class_ $ HH.ClassName "bi-trash" ] [] ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $
        navigate Page404
      now <- H.liftEffect nowDateTime
      H.modify_ _
        { documents = mockDocuments now
        , currentTime = Just now
        }
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
    CreateDocument -> do
      --switch to dedicated page.
      pure unit
    RequestDeleteDocument documentID -> do
      H.modify_ _ { requestDelete = Just documentID }
    CancelDeleteDocument -> do
      H.modify_ \s -> s
        { error = Nothing
        , requestDelete = Nothing
        }
    ConfirmDeleteDocument docID -> do
      H.modify_ \s -> s
        { error = Nothing
        , documents = filter (\d -> d.header.id /= docID) s.documents
        , requestDelete = Nothing
        }
      handleAction Filter
    ViewDocument documentID -> do
      s <- H.get
      case s.requestDelete of
        Nothing -> do
          log ("Routing to editor for project " <> ((docNameFromID s) documentID))
          navigate Editor
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

  docNameFromID :: State -> Int -> String
  docNameFromID state id =
    case head (filter (\doc -> doc.header.id == id) state.documents) of
      Just doc -> doc.body.name
      Nothing -> "Unknown Name"

  mockDocuments :: DateTime -> Array Document
  mockDocuments now =
    map
      ( \i ->
          { body:
              { name: "Doc" <> (show i)
              , text: "This is the" <> show i <> "-th Document"
              }
          , header:
              { updatedTs: adjustDateTime (Days (toNumber i)) now
              , id: i
              , archivedStatus: false
              }
          }
      )
      (0 .. 23)
