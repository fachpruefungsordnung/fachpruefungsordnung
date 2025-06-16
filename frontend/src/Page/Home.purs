-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.
-- |
-- | Right now, it displays a list of mock projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

module FPO.Page.Home (component) where

import Prelude

import Data.Array (filter, null)
import Data.DateTime (DateTime, adjust, date, day, diff, month, year)
import Data.Enum (fromEnum)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.Time.Duration
  ( class Duration
  , Days(..)
  , Hours(..)
  , Seconds(..)
  , negateDuration
  , toDuration
  )
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Table.Head as TH
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store (User)
import FPO.Data.Store as Store
import FPO.Page.HTML (addCard, addColumn)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
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

type Input = Unit

data Sorting = TitleAsc | TitleDesc | LastUpdatedAsc | LastUpdatedDesc

data Action
  = Initialize
  | NavLogin
  | ViewProject String
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  | ChangeSorting TH.Output
  | HandleSearchInput String

type State = FPOState
  ( user :: Maybe User
  , projects :: Array Project
  , currentTime :: Maybe DateTime
  , searchQuery :: String
  )

-- | Model for Projects with proper DateTime.
type Project =
  { name :: String
  , description :: String
  , version :: String
  , collaborators :: Int
  , updated :: DateTime
  }

type Slots =
  ( tablehead :: forall q. H.Slot q TH.Output Unit
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
      H.modify_ _
        { user = u
        , translator = fromFpoTranslator store.translator
        , projects = mockProjects now
        , currentTime = Just now
        }
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ViewProject projectName -> do
      log $ "Routing to editor for project " <> projectName
      navigate Editor
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
            (\a b -> compare a.name b.name)
            state.projects
        "Last Updated" ->
          TH.sortByF
            (TH.toggleSorting order) -- The newest project should be first.
            (\a b -> compare a.updated b.updated)
            state.projects
        _ -> state.projects -- Ignore other columns.

      H.modify_ _ { projects = projects }
      pure unit
    HandleSearchInput query -> do
      H.modify_ _ { searchQuery = query }

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
              [ renderProjectTable state ]
          ]
      ]

  -- Renders the list of projects.
  renderProjectTable :: State -> H.ComponentHTML Action Slots m
  renderProjectTable state =
    HH.table
      [ HP.classes [ HB.table, HB.tableHover, HB.tableBordered ] ]
      [ HH.colgroup_
          [ HH.col [ HP.style "width: 70%;" ] -- 'Title' column
          , HH.col [ HP.style "width: 30%;" ] -- 'Last Updated' column
          ]
      , HH.slot _tablehead unit TH.component tableCols ChangeSorting
      , HH.tbody_
          ( let
              ps = filterProjects state.searchQuery state.projects
            in
              if null ps then
                [ HH.tr []
                    [ HH.td
                        [ HP.colSpan 2
                        , HP.classes [ HB.textCenter ]
                        ]
                        [ HH.i_ [ HH.text "No projects found" ] ]
                    ]
                ]
              else map (renderProjectRow state) ps
          )
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
  renderProjectRow :: forall w. State -> Project -> HH.HTML w Action
  renderProjectRow state project =
    HH.tr
      [ HE.onClick $ const $ ViewProject project.name
      , HP.style "cursor: pointer;"
      ]
      [ HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text project.name ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ formatRelativeTime state.currentTime project.updated ]
      ]

  filterProjects :: String -> Array Project -> Array Project
  filterProjects query projects =
    filter (\p -> contains (Pattern $ toLower query) (toLower p.name)) projects

  -- Formats DateTime as relative time ("3 hours ago") or absolute date if > 1 week.
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

  -- Create mock projects with proper DateTime values
  mockProjects :: DateTime -> Array Project
  mockProjects now =
    [ { name: "FPO Informatik 1-Fach M.Sc. 2025"
      , description: "Hier gibt es Zusatzinformationen!"
      , version: "1.2"
      , collaborators: 3
      , updated: adjustDateTime (Hours 6.0) now -- 6 hours ago
      }
    , { name: "FPO Elektrotechnik und Informationstechnik B.Sc. 2022"
      , description:
          (<>)
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed eiusmod tempor"
            "incidunt Hello! ut labore et dolore magna aliqua. Ut enim ad minim veniam"
      , version: "4.8"
      , collaborators: 4
      , updated: adjustDateTime (Days 3.0) now -- 3 days ago
      }
    , { name: "FPO Materialwissenschaft B.Sc."
      , description: "An older project for testing absolute date formatting"
      , version: "2.1"
      , collaborators: 2
      , updated: adjustDateTime (Days (179.0)) now
      }
    ]

  -- Helper function to adjust a DateTime by a duration (subtract from current time)
  adjustDateTime :: forall d. Duration d => d -> DateTime -> DateTime
  adjustDateTime duration dt =
    fromMaybe dt $ adjust (negateDuration duration) dt
