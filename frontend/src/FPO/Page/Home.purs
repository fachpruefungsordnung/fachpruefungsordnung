-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.
-- |
-- | Right now, it displays a list of mock projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

-- to change: the project/document structure between pages isn't standardized.
-- This must be changed. For now, the toProject function translates as needed and makes up
-- missing data

module FPO.Page.Home (component, adjustDateTime, formatRelativeTime) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Array (filter, length, null, replicate, slice)
import Data.DateTime (DateTime, adjust, date, day, diff, month, year)
import Data.Enum (fromEnum)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.Time.Duration (class Duration, Seconds(..), negateDuration, toDuration)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Pagination as P
import FPO.Components.Table.Head as TH
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser, getUserDocuments)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto (NewDocumentHeader, docDateToDateTime, getNDHID, getNDHLastEdited, getNDHName)
import FPO.Dto.UserDto (FullUserDto, getUserID)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addCard, addColumn)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_tablehead = Proxy :: Proxy "tablehead"
_pagination = Proxy :: Proxy "pagination"

type Input = Unit

data Sorting = TitleAsc | TitleDesc | LastUpdatedAsc | LastUpdatedDesc

data Action
  = Initialize
  | NavLogin
  | ViewProject NewDocumentHeader
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  | ChangeSorting TH.Output
  | HandleSearchInput String
  | SetPage P.Output

type State = FPOState
  ( user :: Maybe FullUserDto
  , projects :: Array NewDocumentHeader
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
    { user: Nothing
    , translator: fromFpoTranslator context
    , projects: []
    , currentTime: Nothing
    , searchQuery: ""
    , page: 0
    }

  render
    :: State
    -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ]
      ]
      [ HH.div_
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mt5 ] ]
              [ HH.text $ translate (label :: _ "common_home") state.translator ]
          , HH.div [ HP.classes [ HB.dropdownDivider, HB.mb4 ] ] []
          , renderProjectsOverview state
          ]
      ]

  handleAction
    :: MonadAff m
    => Action
    -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      store <- getStore
      u <- liftAff getUser
      now <- liftEffect nowDateTime
      -- If the user is logged in, fetch their documents and convert them to projects.
      void $ runMaybeT do
        user <- MaybeT $ pure u
        docs <- MaybeT $ liftAff $ getUserDocuments $ getUserID user
        lift $ H.modify_ _
          { user = u
          , translator = fromFpoTranslator store.translator
          , projects = docs
          , currentTime = Just now
          }
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ViewProject project -> do
      log $ "Routing to editor for project " <> (getNDHName project)
      navigate (Editor { docID: getNDHID project })
    NavLogin -> do
      updateStore $ Store.SetLoginRedirect (Just Home)
      navigate Login
    DoNothing ->
      pure unit
    ChangeSorting (TH.Clicked title order) -> do
      state <- H.get

      -- Sorting logic based on the clicked column title:
      projects <- pure $ case title of
        "Title" ->
          TH.sortByF
            order
            (\a b -> compare (getNDHName a) (getNDHName b))
            state.projects
        "Last Updated" ->
          TH.sortByF
            (TH.toggleSorting order) -- The newest project should be first.
            (\a b -> compare (getEditTimestamp a) (getEditTimestamp b))
            state.projects
        _ -> state.projects -- Ignore other columns.

      H.modify_ _ { projects = projects }

      -- After changing the sorting, tell the pagination component
      -- to reset to the first page:
      H.tell _pagination unit $ P.SetPageQ 0
    HandleSearchInput query -> do
      H.modify_ _ { searchQuery = query }
    SetPage (P.Clicked page) -> do
      H.modify_ _ { page = page }

  -- Renders the overview of projects for the user.
  renderProjectsOverview :: State -> H.ComponentHTML Action Slots m
  renderProjectsOverview state = case state.user of
    Just _ -> HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ addCard
          (translate (label :: _ "home_yourProjects") state.translator)
          [ HP.classes [ HB.colSm11, HB.colMd9, HB.colLg7 ] ]
          (renderProjectOverview state)
      ]
    Nothing -> HH.p
      [ HP.classes [ HB.textCenter, HB.mt5 ] ]
      [ HH.text $ translate (label :: _ "home_pleaseLogInA") state.translator
      , HH.a
          [ HE.onClick $ const $ NavLogin
          , HP.classes [ HB.textDecorationUnderline, HB.textDark ]
          , HP.style "cursor: pointer;"
          ]
          [ HH.text $ translate (label :: _ "home_toLogin") state.translator ]
      , HH.text $ translate (label :: _ "home_pleaseLogInB") state.translator
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
                  "Search for Projects"
                  "bi-search"
                  HP.InputText
                  HandleSearchInput
              ]
          , HH.div [ HP.classes [ HB.col12 ] ]
              [ renderProjectTable ps state ]
          , HH.slot _pagination unit P.component paginationSettings SetPage
          ]
      ]
    where
    -- All projects filtered by the search query:
    fps = filterProjects state.searchQuery state.projects
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
    :: Array NewDocumentHeader -> State -> H.ComponentHTML Action Slots m
  renderProjectTable ps state =
    HH.table
      [ HP.classes [ HB.table, HB.tableHover, HB.tableBordered ] ]
      [ HH.colgroup_
          [ HH.col [ HP.style "width: 70%;" ] -- 'Title' column
          , HH.col [ HP.style "width: 30%;" ] -- 'Last Updated' column
          ]
      , HH.slot _tablehead unit TH.component
          { columns: tableCols, sortedBy: "Last Updated" }
          ChangeSorting
      , HH.tbody_ $
          if null ps then
            [ HH.tr []
                [ HH.td
                    [ HP.colSpan 2
                    , HP.classes [ HB.textCenter ]
                    ]
                    [ HH.i_ [ HH.text "No projects found" ] ]
                ]
            ]
          else
            ( map (renderProjectRow state) ps
                <> replicate (5 - length ps) emptyProjectRow
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
      ]

  -- Renders a single project row in the table.
  renderProjectRow :: forall w. State -> NewDocumentHeader -> HH.HTML w Action
  renderProjectRow state project =
    HH.tr
      [ HE.onClick $ const $ ViewProject project
      , HP.style "cursor: pointer;"
      ]
      [ HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ getNDHName project ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ formatRelativeTime state.currentTime $ docDateToDateTime $
              getNDHLastEdited project
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

  filterProjects :: String -> Array NewDocumentHeader -> Array NewDocumentHeader
  filterProjects query projects =
    filter (\p -> contains (Pattern $ toLower query) (toLower $ getNDHName p)) projects

-- | Helper function to adjust a DateTime by a duration (subtract from current time)
adjustDateTime :: forall d. Duration d => d -> DateTime -> DateTime
adjustDateTime duration dt =
  fromMaybe dt $ adjust (negateDuration duration) dt

getEditTimestamp ∷ NewDocumentHeader → DateTime
getEditTimestamp = docDateToDateTime <<< getNDHLastEdited

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
