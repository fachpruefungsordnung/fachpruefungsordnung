-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.
-- |
-- | Right now, it displays a list of mock projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

module FPO.Page.Home (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store (User)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

type Input = Unit

data Action
  = Initialize
  | NavLogin
  | ViewProject String
  | Receive (Connected FPOTranslator Input)

type State = FPOState
  (user :: Maybe User)

-- Model for Projects
type Project =
  { name :: String
  , description :: String
  , version :: String
  , collaborators :: Int
  , updated :: String
  }

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
  initialState { context } = { user: Nothing, translator: fromFpoTranslator context }

  render
    :: forall slots
     . State
    -> H.ComponentHTML Action slots m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ]
      ]
      [ HH.div
          [ HP.classes
              [ HB.container
              , HB.mt5
              , HB.dFlex
              , HB.flexColumn
              , HB.justifyContentStart
              , HB.alignItemsCenter
              , HB.mb4
              ]
          ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "common_home") state.translator ]
          , HH.div [ HP.classes [ HB.dropdownDivider, HB.mb4 ] ] []
          , renderProjectsOverview state
          ]
      ]

  handleAction
    :: forall slots
     . MonadAff m
    => Action
    -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      store <- getStore
      u <- liftAff getUser
      H.modify_ _
        { user = u, translator = fromFpoTranslator store.translator }
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ViewProject projectName -> do
      log $ "Routing to editor for project " <> projectName
      navigate Editor
    NavLogin -> do
      updateStore $ Store.SetLoginRedirect (Just Home)
      navigate Login

  -- | Renders the overview of projects for the user.
  renderProjectsOverview :: forall w. State -> HH.HTML w Action
  renderProjectsOverview state = case state.user of
    Just _ -> HH.div []
      [ HH.h3 [ HP.classes [ HB.mb4 ] ]
          [ HH.text $ translate (label :: _ "home_yourProjects") state.translator ]
      , HH.div [] (map (renderProjectCard state) mockProjects)
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

  -- | Renders a single project card.
  renderProjectCard :: forall w. State -> Project -> HH.HTML w Action
  renderProjectCard _ project =
    HH.div [ HP.classes [ HB.card, HB.shadowSm, HB.mb3, HB.bgSecondarySubtle ] ]
      [ HH.div [ HP.classes [ HB.cardBody ] ]
          [ HH.h5
              [ HP.classes [ HB.cardTitle, HB.mb1, HB.textPrimary ]
              , HP.style "cursor: pointer;"
              , HE.onClick $ const $ ViewProject project.name
              ]
              [ HH.text project.name ]
          , HH.p
              [ HP.classes [ HB.cardText, HB.mb2 ] ]
              [ HH.text project.description ]
          , HH.div
              [ HP.classes
                  [ HB.dFlex
                  , HB.justifyContentBetween
                  , HB.alignItemsCenter
                  ]
              ]
              [ HH.small []
                  [ HH.i [ HP.classes [ H.ClassName "bi-tag", HB.me1 ] ] []
                  , HH.text ("v" <> project.version)
                  ]
              , HH.small []
                  [ HH.i [ HP.classes [ H.ClassName "bi-people", HB.me1 ] ] []
                  , HH.text (show project.collaborators)
                  ]
              , HH.small []
                  [ HH.i [ HP.classes [ H.ClassName "bi-clock", HB.me1 ] ] []
                  , HH.text project.updated
                  ]
              ]
          ]
      ]

  mockProjects :: Array Project
  mockProjects =
    [ { name: "FPO Informatik 1-Fach M.Sc. 2025"
      , description: "Hier gibt es Zusatzinformationen!"
      , version: "1.2"
      , collaborators: 3
      , updated: "6 hours ago"
      }
    , { name: "FPO Elektrotechnik und Informationstechnik B.Sc. 2022"
      , description:
          (<>)
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed eiusmod tempor"
            "incidunt Hello! ut labore et dolore magna aliqua. Ut enim ad minim veniam"
      , version: "4.8"
      , collaborators: 4
      , updated: "3 days ago"
      }
    ]
