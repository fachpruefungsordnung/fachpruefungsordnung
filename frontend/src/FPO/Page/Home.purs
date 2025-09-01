-- | Home page of the application.
-- |
-- | It displays a list of all accessible projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

-- to change: the project/document structure between pages isn't standardized.
-- This must be changed. For now, the toProject function translates as needed and makes up
-- missing data

module FPO.Page.Home (component, adjustDateTime, formatRelativeTime) where

import Prelude

import Data.Array (filter, length, null, replicate, slice)
import Data.DateTime (DateTime, adjust, date, day, diff, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.Time.Duration (class Duration, Seconds(..), negateDuration, toDuration)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Pagination as P
import FPO.Components.Table.Head as TH
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (LoadState(..), fromLoading, getUser, getUserDocuments)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocDate as DocDate
import FPO.Dto.DocumentDto.DocumentHeader (DocumentHeader, DocumentID)
import FPO.Dto.DocumentDto.DocumentHeader as DocumentHeader
import FPO.Dto.UserDto (FullUserDto, getUserID)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addCard, addColumn, loadingSpinner)
import FPO.UI.SmoothScroll (smoothScrollToElement)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

_tablehead = Proxy :: Proxy "tablehead"
_pagination = Proxy :: Proxy "pagination"

type Input = Unit

data Sorting = TitleAsc | TitleDesc | LastUpdatedAsc | LastUpdatedDesc

data Action
  = Initialize
  | NavLogin
  | ScrollToFeatures
  | ViewProject DocumentHeader
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  | ChangeSorting TH.Output
  | HandleSearchInput String
  | SetPage P.Output
  | DownloadPdf DocumentID String MouseEvent

type State = FPOState
  ( user :: LoadState (Maybe FullUserDto)
  , projects :: LoadState (Array DocumentHeader)
  , currentTime :: Maybe DateTime
  , searchQuery :: String
  , page :: Int
  )

type Slots =
  ( tablehead :: forall q. H.Slot q TH.Output Unit
  , pagination :: H.Slot P.Query P.Output Unit
  )

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Connected FPOTranslator Input -> State
  initialState { context } =
    { user: Loading
    , translator: fromFpoTranslator context
    , projects: Loading
    , currentTime: Nothing
    , searchQuery: ""
    , page: 0
    }

  render
    :: State
    -> H.ComponentHTML Action Slots m
  render state =
    case state.user of
      Loading -> loadingSpinner
      Loaded u -> case u of
        Just _ ->
          renderProjectsOverview state
        Nothing ->
          renderNotLoggedIn state

  handleAction
    :: MonadAff m
    => Action
    -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      userWithError <- Store.preventErrorHandlingLocally getUser
      now <- liftEffect nowDateTime
      -- If the user is logged in, fetch their documents and convert them to projects.
      case userWithError of
        Left _ -> do
          H.modify_ _
            { user = Loaded Nothing
            , currentTime = Just now
            }
        Right user -> do
          H.modify_ _
            { projects = Loading
            , user = Loaded $ Just user
            }
          docsResult <- getUserDocuments $ getUserID user
          case docsResult of
            Left _ -> do -- TODO correct error handling
              H.modify_ _
                { projects = Loading
                , currentTime = Just now
                }
            Right docs -> do
              H.modify_ _
                { projects = Loaded docs
                , currentTime = Just now
                }
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ViewProject project -> do
      log $ "Routing to editor for project " <> (DocumentHeader.getName project)
      navigate (Editor { docID: DocumentHeader.getID project })
    NavLogin -> do
      updateStore $ Store.SetLoginRedirect (Just Home)
      navigate Login
    ScrollToFeatures -> do
      H.liftEffect $ smoothScrollToElement "features"
    DoNothing ->
      pure unit
    ChangeSorting (TH.Clicked title order) -> do
      state <- H.get
      case state.projects of
        Loading -> pure unit
        Loaded projects' -> do
          -- Sorting logic based on the clicked column title:
          let
            title_title = translate (label :: _ "home_title") state.translator
            title_lastUpdated = translate (label :: _ "home_lastUpdated")
              state.translator
          projects <- pure $
            if title == title_title then
              TH.sortByF
                order
                (comparing DocumentHeader.getName)
                projects'
            else if title == title_lastUpdated then
              TH.sortByF
                (TH.toggleSorting order) -- The newest project should be first.
                (comparing getEditTimestamp)
                projects'
            else
              projects' -- Ignore other columns.

          H.modify_ _ { projects = Loaded projects }

          -- After changing the sorting, tell the pagination component
          -- to reset to the first page:
          H.tell _pagination unit $ P.SetPageQ 0
    HandleSearchInput query -> do
      H.modify_ _ { searchQuery = query }
    SetPage (P.Clicked page) -> do
      H.modify_ _ { page = page }
    DownloadPdf _ _ event -> do
      H.liftEffect $ do
        preventDefault (MouseEvent.toEvent event)
        stopPropagation (MouseEvent.toEvent event)
      updateStore $ Store.AddWarning "Not yet implemented!"

  -- renderedPDF' <- Request.postBlobOrError ("/render/pdf" <> projectId)
  -- let filename = projectName <> ".pdf"
  -- case renderedPDF' of
  --   Left _ -> pure unit
  --   Right blobOrError ->
  --     case blobOrError of
  --       Left errMsg -> do
  --         updateStore $ Store.AddError $ "Failed to generate PDF: " <> errMsg
  --       Right body -> do
  --         -- create blobl link
  --         url <- H.liftEffect $ createObjectURL body
  --         -- Create an invisible link and click it to download PDF
  --         H.liftEffect $ do
  --           -- get window stuff
  --           win <- window
  --           hdoc <- document win
  --           let doc = HTMLDocument.toDocument hdoc

  --           -- create link
  --           aEl <- Document.createElement "a" doc
  --           case HTMLElement.fromElement aEl of
  --             Nothing -> pure unit
  --             Just aHtml -> do
  --               Element.setAttribute "href" url aEl
  --               Element.setAttribute "download" filename aEl
  --               HTMLElement.click aHtml
  --         -- deactivate the blob link after 1 sec
  --         _ <- H.fork do
  --           H.liftAff $ delay (Milliseconds 1000.0)
  --           H.liftEffect $ revokeObjectURL url
  --         pure unit

  -- H.liftEffect $ downloadPdf projectId

  -- Renders the overview of projects for the user.
  renderProjectsOverview :: State -> H.ComponentHTML Action Slots m
  renderProjectsOverview state = HH.div
    [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
    [ HH.h1 [ HP.classes [ HB.textCenter, HB.mt5 ] ]
        [ HH.text $ translate (label :: _ "common_home") state.translator ]
    , HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
        [ addCard
            (translate (label :: _ "home_yourProjects") state.translator)
            [ HP.classes [ HB.colSm11, HB.colMd9, HB.colLg7 ] ]
            (renderProjectOverview state)
        ]
    ]

  renderNotLoggedIn :: State -> H.ComponentHTML Action Slots m
  renderNotLoggedIn state =
    HH.div_
      [ -- Hero Section
        HH.section
          [ HP.classes
              [ HH.ClassName "hero-bg"
              , HH.ClassName "slanted-bottom"
              , HB.dFlex
              , HB.alignItemsCenter
              , HB.textWhite
              ]
          ]
          [ HH.div
              [ HP.classes [ HB.container ] ]
              [ HH.div
                  [ HP.classes [ HB.row, HB.alignItemsCenter ] ]
                  [ HH.div
                      [ HP.classes [ HB.colLg6 ] ]
                      [ HH.h1
                          [ HP.classes
                              [ HB.display4
                              , HB.fwBold
                              , HB.mb4
                              ]
                          ]
                          [ HH.text "FPO-Editor" ]
                      , HH.p
                          [ HP.classes [ HB.lead, HB.mb4 ] ]
                          [ HH.text $ translate (label :: _ "home_basicDescription")
                              state.translator
                          ]
                      , HH.button
                          [ HP.classes
                              [ HB.btn
                              , HB.btnLight
                              , HB.btnLg
                              , HB.me3
                              ]
                          , HE.onClick $ const NavLogin
                          ]
                          [ HH.i
                              [ HP.classes
                                  [ HH.ClassName "bi-box-arrow-in-right"
                                  , HB.me2
                                  ]
                              ]
                              []
                          , HH.text "Login"
                          ]
                      , HH.a
                          [ HP.classes
                              [ HB.btn
                              , HB.btnOutlineLight
                              , HB.btnLg
                              ]
                          , HE.onClick $ const ScrollToFeatures
                          ]
                          [ HH.i
                              [ HP.classes
                                  [ HH.ClassName "bi-info-circle"
                                  , HB.me2
                                  ]
                              ]
                              []
                          , HH.text $ translate (label :: _ "home_learnMore")
                              state.translator
                          ]
                      ]
                  , HH.div
                      [ HP.classes [ HB.colLg6, HB.textCenter, HB.mt3 ] ]
                      [ HH.i
                          [ HP.classes
                              [ HH.ClassName "bi-file-pdf"
                              , HB.display1
                              ]
                          , HP.style "font-size: 8rem; opacity: 0.8;"
                          ]
                          []
                      ]
                  ]
              ]
          ]

      -- Features Section
      , HH.section
          [ HP.classes [ HB.py5, HB.mt5 ]
          ]
          [ HH.div
              [ HP.classes [ HB.container ] ]
              [ HH.div
                  [ HP.classes
                      [ HB.row
                      , HB.textCenter
                      , HB.g4
                      ]
                  ]
                  [ -- Team Collaboration
                    HH.div
                      [ HP.classes [ HB.colMd4 ] ]
                      [ HH.div
                          [ HP.classes [ HB.p4 ] ]
                          [ HH.div
                              [ HP.classes
                                  [ HB.bgPrimary
                                  , HB.roundedCircle
                                  , HB.mxAuto
                                  , HB.mb3
                                  , HB.dFlex
                                  , HB.alignItemsCenter
                                  , HB.justifyContentCenter
                                  ]
                              , HP.style "width: 80px; height: 80px;"
                              ]
                              [ HH.i
                                  [ HP.classes
                                      [ HH.ClassName "bi"
                                      , HH.ClassName "bi-people"
                                      , HB.textWhite
                                      , HB.fs2
                                      ]
                                  ]
                                  []
                              ]
                          , HH.h4_
                              [ HH.text $ translate
                                  (label :: _ "home_teamCollaboration")
                                  state.translator
                              ]
                          , HH.p
                              [ HP.classes [ HB.textMuted ] ]
                              [ HH.text $ translate
                                  (label :: _ "home_teamCollaborationDescription")
                                  state.translator
                              ]
                          ]
                      ]

                  -- Version Control
                  , HH.div
                      [ HP.classes [ HB.colMd4 ] ]
                      [ HH.div
                          [ HP.classes [ HB.p4 ] ]
                          [ HH.div
                              [ HP.classes
                                  [ HB.bgPrimary
                                  , HB.roundedCircle
                                  , HB.mxAuto
                                  , HB.mb3
                                  , HB.dFlex
                                  , HB.alignItemsCenter
                                  , HB.justifyContentCenter
                                  ]
                              , HP.id "features"
                              , HP.style "width: 80px; height: 80px;"
                              ]
                              [ HH.i
                                  [ HP.classes
                                      [ HH.ClassName "bi-git"
                                      , HB.textWhite
                                      , HB.fs2
                                      ]
                                  ]
                                  []
                              ]
                          , HH.h4_
                              [ HH.text $ translate (label :: _ "home_versionControl")
                                  state.translator
                              ]
                          , HH.p
                              [ HP.classes [ HB.textMuted ] ]
                              [ HH.text $ translate
                                  (label :: _ "home_versionControlDescription")
                                  state.translator
                              ]
                          ]
                      ]

                  -- Advanced Editing
                  , HH.div
                      [ HP.classes [ HB.colMd4 ] ]
                      [ HH.div
                          [ HP.classes [ HB.p4 ] ]
                          [ HH.div
                              [ HP.classes
                                  [ HB.bgPrimary
                                  , HB.roundedCircle
                                  , HB.mxAuto
                                  , HB.mb3
                                  , HB.dFlex
                                  , HB.alignItemsCenter
                                  , HB.justifyContentCenter
                                  ]
                              , HP.style "width: 80px; height: 80px;"
                              ]
                              [ HH.i
                                  [ HP.classes
                                      [ HH.ClassName "bi-journal-code"
                                      , HB.textWhite
                                      , HB.fs2
                                      ]
                                  ]
                                  []
                              ]
                          , HH.h4_
                              [ HH.text $ translate (label :: _ "home_editing")
                                  state.translator
                              ]
                          , HH.p
                              [ HP.classes [ HB.textMuted ] ]
                              [ HH.text $ translate
                                  (label :: _ "home_editingDescription")
                                  state.translator
                              ]
                          ]
                      ]
                  ]

              -- Get Started Button
              , HH.div
                  [ HP.classes [ HB.textCenter, HB.mt5 ] ]
                  [ HH.button
                      [ HP.classes
                          [ HB.btn
                          , HB.btnPrimary
                          , HB.btnLg
                          ]
                      , HE.onClick $ const NavLogin
                      ]
                      [ HH.i
                          [ HP.classes
                              [ HH.ClassName "bi-rocket-takeoff"
                              , HB.me2
                              ]
                          ]
                          []
                      , HH.text $ translate (label :: _ "home_getStarted")
                          state.translator
                      ]
                  ]
              ]
          ]
      ]

  -- Search bar and list of projects.
  renderProjectOverview :: State -> H.ComponentHTML Action Slots m
  renderProjectOverview state =
    HH.div [ HP.classes [ HB.container ] ]
      [ HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
          [ HH.div [ HP.classes [ HB.col6 ] ]
              [ addColumn
                  state.searchQuery
                  ""
                  (translate (label :: _ "home_searchForProjects") state.translator)
                  "bi-search"
                  false
                  HP.InputText
                  HandleSearchInput
              ]
          , if state.projects == Loading then loadingSpinner
            else HH.div [ HP.classes [ HB.col12 ] ] [ renderProjectTable ps state ]
          , HH.slot _pagination unit P.component paginationSettings SetPage
          ]
      ]
    where
    -- All projects filtered by the search query:
    fps = filterProjects state.searchQuery (fromLoading state.projects [])
    -- Slice of the projects for the current page:
    ps = slice (state.page * 5) ((state.page + 1) * 5) $ fps
    paginationSettings =
      { pages: length fps `div` 5 +
          if length fps `mod` 5 > 0 then 1 else 0
      , style: P.Compact 2
      , reaction: P.FirstPage -- After changing the search query, reset to first page.
      }

  -- Renders the list of projects.
  renderProjectTable
    :: Array DocumentHeader -> State -> H.ComponentHTML Action Slots m
  renderProjectTable ps state =
    HH.table
      [ HP.classes [ HB.table, HB.tableHover, HB.tableBordered ] ]
      [ HH.colgroup_
          [ HH.col [ HP.style "width: 60%;" ] -- 'Title' column
          , HH.col [ HP.style "width: 30%;" ] -- 'Last Updated' column
          , HH.col [ HP.style "width: 10%;" ] -- 'PDF' column
          ]
      , HH.slot _tablehead unit TH.component
          { columns: tableCols state.translator
          , sortedBy: translate (label :: _ "home_lastUpdated") state.translator
          }
          ChangeSorting
      , HH.tbody_ $
          if null ps then
            [ HH.tr []
                [ HH.td
                    [ HP.colSpan 3
                    , HP.classes [ HB.textCenter ]
                    ]
                    [ HH.i_
                        [ HH.text $ translate (label :: _ "home_noProjectsFound")
                            state.translator
                        ]
                    ]
                ]
            ]
          else
            ( map (renderProjectRow state) ps
                <> replicate (5 - length ps) emptyProjectRow
            ) -- Fill up to 5 rows
      ]
    where
    tableCols translator = TH.createTableColumns
      [ { title: translate (label :: _ "home_title") translator
        , style: Just TH.Alpha
        }
      , { title: translate (label :: _ "home_lastUpdated") translator
        , style: Just TH.Numeric
        }
      , { title: "PDF"
        , style: Nothing
        }
      ]

  -- Renders a single project row in the table.
  renderProjectRow :: forall w. State -> DocumentHeader -> HH.HTML w Action
  renderProjectRow state project =
    HH.tr
      [ HE.onClick $ const $ ViewProject project
      , HP.style "cursor: pointer;"
      ]
      [ HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ DocumentHeader.getName project ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ formatRelativeTime state.currentTime $ DocDate.docDateToDateTime
              $
                DocumentHeader.getLastEdited project
          ]
      , HH.td
          [ HP.classes [ HB.dFlex, HB.justifyContentCenter, HB.alignItemsCenter ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineSecondary ]
              , HE.onClick $ \e -> DownloadPdf (DocumentHeader.getIdentifier project)
                  (DocumentHeader.getName project)
                  e
              ]
              [ HH.i
                  [ HP.classes
                      [ HH.ClassName "bi-file-earmark-pdf"
                      , HB.bi
                      ]
                  ]
                  []
              ]
          ]
      ]

  -- Renders an empty project row for padding.
  emptyProjectRow :: forall w. HH.HTML w Action
  emptyProjectRow =
    HH.tr []
      [ HH.td
          [ HP.colSpan 2
          , HP.classes [ HB.textCenter, HB.invisible ]
          ]
          [ HH.text $ "Empty Row" ]
      ]

  filterProjects :: String -> Array DocumentHeader -> Array DocumentHeader
  filterProjects query projects =
    filter
      (\p -> contains (Pattern $ toLower query) (toLower $ DocumentHeader.getName p))
      projects

-- | Helper function to adjust a DateTime by a duration (subtract from current time)
adjustDateTime :: forall d. Duration d => d -> DateTime -> DateTime
adjustDateTime duration dt =
  fromMaybe dt $ adjust (negateDuration duration) dt

getEditTimestamp ∷ DocumentHeader → DateTime
getEditTimestamp = DocDate.docDateToDateTime <<< DocumentHeader.getLastEdited

-- | Formats DateTime as relative time ("3 hours ago") or absolute date if > 1 week.
formatRelativeTime :: Maybe DateTime -> DateTime -> String
formatRelativeTime Nothing _ = "Unknown"
formatRelativeTime (Just current) updated =
  let
    timeDiff =
      if current > updated then diff current updated else diff updated current

    (Seconds seconds) = toDuration timeDiff :: Seconds
    totalMinutes = floor (seconds / 60.0)
    totalHours = floor (seconds / 3600.0)
    totalDays = floor (seconds / 86400.0)
  in
    if totalDays > 7 then
      formatAbsoluteDate updated
    else if totalDays >= 1 then
      show totalDays <> if totalDays == 1 then " day ago" else " days ago"
    else if totalHours >= 1 then
      show totalHours <> if totalHours == 1 then " hour ago" else " hours ago"
    else if totalMinutes >= 1 then
      show totalMinutes <>
        if totalMinutes == 1 then " minute ago" else " minutes ago"
    else
      "Just now"
  where
  -- Format DateTime as absolute date (YYYY-MM-DD)
  formatAbsoluteDate :: DateTime -> String
  formatAbsoluteDate dt =
    let
      d' = date dt
      y = show $ fromEnum $ year d'
      m = padZero $ fromEnum $ month d'
      d = padZero $ fromEnum $ day d'
    in
      d <> "." <> m <> "." <> y
    where
    padZero n = if n < 10 then "0" <> show n else show n
