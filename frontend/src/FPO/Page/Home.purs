-- | Home page of the application.
-- |
-- | It displays a list of all accessible projects and allows
-- | the user to navigate to the editor by clicking on the title.
-- | The user must be logged in to see the projects.

-- to change: the project/document structure between pages isn't standardized.
-- This must be changed. For now, the toProject function translates as needed and makes up
-- missing data

-- module FPO.Page.Home (component, adjustDateTime, formatRelativeTime) where
module FPO.Page.Home (component) where

import Prelude

import Data.Array (filter, length, null, replicate, slice)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, toLower)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( LoadState(..)
  , fromLoading
  , getBlobOrError
  , getUserDocuments
  , getUserSilent
  )
import FPO.Data.Route (Route(..), editorRoute, loginRouteWithRedirect)
import FPO.Data.Store as Store
import FPO.Data.Time (formatRelativeTime)
import FPO.Dto.DocumentDto.DocDate as DocDate
import FPO.Dto.DocumentDto.DocumentHeader
  ( DocumentHeader
  , DocumentID
  )
import FPO.Dto.DocumentDto.DocumentHeader as DocumentHeader
import FPO.Dto.UserDto (FullUserDto, getUserID)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.Css as HB
import FPO.UI.HTML (loadingSpinner)
import FPO.UI.NavbarReveal (setupNavbarReveal, teardownNavbarReveal)
import FPO.UI.SmoothScroll (smoothScrollToElement)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.Event.Event (preventDefault, stopPropagation)
import Web.File.Url (createObjectURL, revokeObjectURL)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

_pagination = Proxy :: Proxy "pagination"

type Input = Unit

data Action
  = Initialize
  | Finalize
  | NavLogin
  | ScrollToFeatures
  | ViewProject DocumentHeader
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  | HandleSearchInput String
  | SetPage P.Output
  | DownloadPdf DocumentID String MouseEvent -- Id, Project Name, Event (used for prevent default)
  | DownloadZip DocumentID String MouseEvent -- Id, Project Name, Event (used for prevent default)

type State = FPOState
  ( user :: LoadState (Maybe FullUserDto)
  , projects :: LoadState (Array DocumentHeader)
  , currentTime :: Maybe DateTime
  , searchQuery :: String
  , page :: Int
  )

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
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
        , finalize = Just Finalize
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
      userWithError <- getUserSilent
      now <- liftEffect nowDateTime
      -- If the user is logged in, fetch their documents and convert them to projects.
      case userWithError of
        Left _ -> do
          H.modify_ _
            { user = Loaded Nothing
            , currentTime = Just now
            }
          -- The user is not logged in → show landing hero.
          -- Hide the navbar and reveal it only after scrolling.
          H.liftEffect $ setupNavbarReveal 80
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
    Finalize -> do
      H.liftEffect teardownNavbarReveal
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ViewProject project -> do
      navigate (editorRoute (DocumentHeader.getID project))
    NavLogin -> do
      H.liftEffect teardownNavbarReveal
      navigate (loginRouteWithRedirect Home)
    ScrollToFeatures -> do
      H.liftEffect $ smoothScrollToElement "features"
    DoNothing ->
      pure unit
    HandleSearchInput query -> do
      H.modify_ _ { searchQuery = query }
    SetPage (P.Clicked page) -> do
      H.modify_ _ { page = page }
    DownloadPdf projectId projectName event -> do
      H.liftEffect $ do
        preventDefault (MouseEvent.toEvent event)
        stopPropagation (MouseEvent.toEvent event)

      renderedPdf' <- getBlobOrError ("/docs/" <> show projectId <> "/rev/latest/pdf")
      let filename = projectName <> ".pdf"
      case renderedPdf' of
        Left _ -> pure unit
        Right blobOrError ->
          case blobOrError of
            Left _ -> pure unit
            Right body -> do
              -- create blobl link
              url <- H.liftEffect $ createObjectURL body
              -- Create an invisible link and click it to download Pdf
              H.liftEffect $ do
                -- get window stuff
                win <- window
                hdoc <- document win
                let doc = HTMLDocument.toDocument hdoc

                -- create link
                aEl <- Document.createElement "a" doc
                case HTMLElement.fromElement aEl of
                  Nothing -> pure unit
                  Just aHtml -> do
                    Element.setAttribute "href" url aEl
                    Element.setAttribute "download" filename aEl
                    HTMLElement.click aHtml
              -- deactivate the blob link after 1 sec
              _ <- H.fork do
                H.liftAff $ delay (Milliseconds 1000.0)
                H.liftEffect $ revokeObjectURL url
              pure unit

    -- H.liftEffect $ downloadPdf projectId
    DownloadZip projectId projectName event -> do
      H.liftEffect $ do
        preventDefault (MouseEvent.toEvent event)
        stopPropagation (MouseEvent.toEvent event)

      renderedZip' <- getBlobOrError
        ("/docs/" <> show projectId <> "/rev/latest/html")
      let filename = projectName <> ".zip"
      case renderedZip' of
        Left _ -> pure unit
        Right blobOrError ->
          case blobOrError of
            Left _ -> pure unit
            Right body -> do
              -- create blob link
              url <- H.liftEffect $ createObjectURL body
              -- Create an invisible link and click it to download ZIP
              H.liftEffect $ do
                -- get window stuff
                win <- window
                hdoc <- document win
                let doc = HTMLDocument.toDocument hdoc

                -- create link
                aEl <- Document.createElement "a" doc
                case HTMLElement.fromElement aEl of
                  Nothing -> pure unit
                  Just aHtml -> do
                    Element.setAttribute "href" url aEl
                    Element.setAttribute "download" filename aEl
                    HTMLElement.click aHtml
              -- deactivate the blob link after 1 sec
              _ <- H.fork do
                H.liftAff $ delay (Milliseconds 1000.0)
                H.liftEffect $ revokeObjectURL url
              pure unit

  -- H.liftEffect $ downloadPdf projectId

  -- Renders the overview of projects for the user.
  renderProjectsOverview :: State -> H.ComponentHTML Action Slots m
  renderProjectsOverview state = HH.div
    [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
    [ HH.h1 [ HP.classes [ HB.textCenter, HB.mt5 ] ]
        [ HH.text $ translate (label :: _ "common_home") state.translator ]
    , HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
        [ renderProjectOverview state
        ]
    ]

  renderNotLoggedIn :: State -> H.ComponentHTML Action Slots m
  renderNotLoggedIn state =
    HH.div_
      [ -- ── Hero Section ──────────────────────────────────────────
        HH.section
          [ HP.classes [ HH.ClassName "landing-hero" ] ]
          [ -- Decorative gradient orbs
            HH.div
              [ HP.classes
                  [ HH.ClassName "landing-orb"
                  , HH.ClassName "landing-orb--1"
                  ]
              ]
              []
          , HH.div
              [ HP.classes
                  [ HH.ClassName "landing-orb"
                  , HH.ClassName "landing-orb--2"
                  ]
              ]
              []
          , HH.div
              [ HP.classes
                  [ HH.ClassName "landing-orb"
                  , HH.ClassName "landing-orb--3"
                  ]
              ]
              []

          -- Centred hero content
          , HH.div
              [ HP.classes [ HH.ClassName "landing-hero__content" ] ]
              [ -- Small badge / chip
                HH.span
                  [ HP.classes [ HH.ClassName "landing-badge" ] ]
                  [ HH.i
                      [ HP.classes
                          [ HH.ClassName "bi-mortarboard"
                          , HB.me1
                          ]
                      ]
                      []
                  , HH.text "FPO-Editor"
                  ]

              -- Large title
              , HH.h1
                  [ HP.classes [ HH.ClassName "landing-hero__title" ] ]
                  [ HH.text "FPO-Editor" ]

              -- Description
              , HH.p
                  [ HP.classes [ HH.ClassName "landing-hero__subtitle" ] ]
                  [ HH.text $ translate
                      (label :: _ "home_basicDescription")
                      state.translator
                  ]

              -- Action buttons
              , HH.div
                  [ HP.classes [ HH.ClassName "landing-hero__actions" ] ]
                  [ HH.button
                      [ HP.classes
                          [ HB.btn
                          , HH.ClassName "landing-btn-primary"
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
                          , HH.ClassName "landing-btn-secondary"
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
                      , HH.text $ translate
                          (label :: _ "home_learnMore")
                          state.translator
                      ]
                  ]
              ]

          -- Scroll-down chevron
          , HH.div
              [ HP.classes [ HH.ClassName "landing-scroll-indicator" ]
              , HE.onClick $ const ScrollToFeatures
              ]
              [ HH.i
                  [ HP.classes [ HH.ClassName "bi-chevron-down" ] ]
                  []
              ]
          ]

      -- ── Features Section ────────────────────────────────────────
      , HH.section
          [ HP.classes [ HH.ClassName "landing-features" ]
          , HP.id "features"
          ]
          [ HH.div
              [ HP.classes [ HB.container ] ]
              [ HH.div
                  [ HP.classes [ HH.ClassName "landing-features__grid" ] ]
                  [ -- Team Collaboration
                    HH.div
                      [ HP.classes [ HH.ClassName "landing-feature-card" ] ]
                      [ HH.div
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__icon" ]
                          ]
                          [ HH.i
                              [ HP.classes [ HH.ClassName "bi-people" ] ]
                              []
                          ]
                      , HH.h3
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__title" ]
                          ]
                          [ HH.text $ translate
                              (label :: _ "home_teamCollaboration")
                              state.translator
                          ]
                      , HH.p
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__desc" ]
                          ]
                          [ HH.text $ translate
                              (label :: _ "home_teamCollaborationDescription")
                              state.translator
                          ]
                      ]

                  -- Version Control
                  , HH.div
                      [ HP.classes [ HH.ClassName "landing-feature-card" ] ]
                      [ HH.div
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__icon"
                              , HH.ClassName "landing-feature-card__icon--success"
                              ]
                          ]
                          [ HH.i
                              [ HP.classes [ HH.ClassName "bi-git" ] ]
                              []
                          ]
                      , HH.h3
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__title" ]
                          ]
                          [ HH.text $ translate
                              (label :: _ "home_versionControl")
                              state.translator
                          ]
                      , HH.p
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__desc" ]
                          ]
                          [ HH.text $ translate
                              (label :: _ "home_versionControlDescription")
                              state.translator
                          ]
                      ]

                  -- Advanced Editing
                  , HH.div
                      [ HP.classes [ HH.ClassName "landing-feature-card" ] ]
                      [ HH.div
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__icon"
                              , HH.ClassName "landing-feature-card__icon--warning"
                              ]
                          ]
                          [ HH.i
                              [ HP.classes [ HH.ClassName "bi-journal-code" ] ]
                              []
                          ]
                      , HH.h3
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__title" ]
                          ]
                          [ HH.text $ translate
                              (label :: _ "home_editing")
                              state.translator
                          ]
                      , HH.p
                          [ HP.classes
                              [ HH.ClassName "landing-feature-card__desc" ]
                          ]
                          [ HH.text $ translate
                              (label :: _ "home_editingDescription")
                              state.translator
                          ]
                      ]
                  ]
              ]
          ]

      -- ── CTA Section ─────────────────────────────────────────────
      , HH.section
          [ HP.classes [ HH.ClassName "landing-cta" ] ]
          [ HH.div
              [ HP.classes [ HH.ClassName "landing-cta__inner" ] ]
              [ HH.h2
                  [ HP.classes [ HH.ClassName "landing-cta__title" ] ]
                  [ HH.text $ translate
                      (label :: _ "home_getStarted")
                      state.translator
                  ]
              , HH.p
                  [ HP.classes [ HH.ClassName "landing-cta__subtitle" ] ]
                  [ HH.text $ translate
                      (label :: _ "home_basicDescription")
                      state.translator
                  ]
              , HH.button
                  [ HP.classes
                      [ HB.btn
                      , HH.ClassName "landing-btn-primary"
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
                  , HH.text $ translate
                      (label :: _ "home_getStarted")
                      state.translator
                  ]
              ]
          ]
      ]

  -- Search bar and list of projects.
  renderProjectOverview :: State -> H.ComponentHTML Action Slots m
  renderProjectOverview state =
    HH.div
      [ HP.classes [ HB.colSm11, HB.colMd9, HB.colLg7, HH.ClassName "fpo-data-list" ]
      ]
      [ -- Header
        HH.div [ HP.classes [ HH.ClassName "fpo-data-list__header" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__title" ] ]
              [ HH.text $ translate (label :: _ "home_yourProjects") state.translator
              ]
          ]
      -- Search
      , HH.div [ HP.classes [ HH.ClassName "fpo-data-list__search" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__search-wrapper" ] ]
              [ HH.i
                  [ HP.classes
                      [ HH.ClassName "bi-search"
                      , HH.ClassName "fpo-data-list__search-icon"
                      ]
                  ]
                  []
              , HH.input
                  [ HP.classes [ HH.ClassName "fpo-data-list__search-input" ]
                  , HP.placeholder $ translate (label :: _ "home_searchForProjects")
                      state.translator
                  , HP.value state.searchQuery
                  , HE.onValueInput HandleSearchInput
                  ]
              ]
          ]
      -- Body
      , HH.div
          [ HP.classes [ HH.ClassName "fpo-data-list__body" ] ]
          ( if state.projects == Loading then
              [ loadingSpinner ]
            else if null ps then
              [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__empty" ] ]
                  [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__empty-icon" ] ]
                      [ HH.i [ HP.classes [ HH.ClassName "bi-folder2-open" ] ] [] ]
                  , HH.div [ HP.classes [ HH.ClassName "fpo-data-list__empty-text" ] ]
                      [ HH.text $ translate (label :: _ "home_noProjectsFound")
                          state.translator
                      ]
                  ]
              ]
            else
              map (renderProjectRow state) ps
                <> placeholderRows 5 (length ps) pageCount
          )
      -- Footer
      , HH.div [ HP.classes [ HH.ClassName "fpo-data-list__footer" ] ]
          [ HH.slot _pagination unit P.component paginationSettings SetPage
          , if length fps == 0 then HH.text ""
            else
              let
                startItem = state.page * 5 + 1
                endItem = min ((state.page + 1) * 5) (length fps)
              in
                HH.span [ HP.classes [ HH.ClassName "fpo-data-list__entry-count" ] ]
                  [ HH.text $
                      show startItem <> "-" <> show endItem <> " / " <> show
                        (length fps)
                  ]
          ]
      ]
    where
    fps = filterProjects state.searchQuery (fromLoading state.projects [])
    ps = slice (state.page * 5) ((state.page + 1) * 5) fps
    pageCount = length fps `div` 5 +
      if length fps `mod` 5 > 0 then 1 else 0
    paginationSettings =
      { pages: pageCount
      , style: P.Compact 2
      , reaction: P.FirstPage
      }

  -- Renders a single project row.
  renderProjectRow :: forall w. State -> DocumentHeader -> HH.HTML w Action
  renderProjectRow state project =
    HH.div
      [ HP.classes
          [ HH.ClassName "fpo-data-list__row"
          , HH.ClassName "fpo-data-list__row--clickable"
          ]
      , HE.onClick $ const $ ViewProject project
      ]
      [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-info" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-primary" ] ]
              [ HH.text $ DocumentHeader.getName project ]
          , HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-secondary" ] ]
              [ HH.text $ formatRelativeTime state.currentTime
                  $ DocDate.docDateToDateTime
                  $
                    DocumentHeader.getLastEdited project
              ]
          ]
      , HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-actions" ] ]
          [ HH.button
              [ HP.classes
                  [ HH.ClassName "fpo-data-list__action-btn"
                  , HH.ClassName "fpo-data-list__action-btn--accent"
                  ]
              , HE.onClick $ \e -> DownloadPdf (DocumentHeader.getIdentifier project)
                  (DocumentHeader.getName project)
                  e
              ]
              [ HH.i [ HP.classes [ HH.ClassName "bi-file-earmark-pdf" ] ] [] ]
          , HH.button
              [ HP.classes
                  [ HH.ClassName "fpo-data-list__action-btn"
                  , HH.ClassName "fpo-data-list__action-btn--accent"
                  ]
              , HE.onClick $ \e -> DownloadZip (DocumentHeader.getIdentifier project)
                  (DocumentHeader.getName project)
                  e
              ]
              [ HH.i [ HP.classes [ HH.ClassName "bi-file-zip" ] ] [] ]
          ]
      ]

  -- Render invisible placeholder rows so the list body keeps a constant
  -- height across pages (prevents pagination controls from jumping).
  -- Only emits placeholders when there are multiple pages.
  placeholderRows :: forall w. Int -> Int -> Int -> Array (HH.HTML w Action)
  placeholderRows perPage currentCount pages =
    let
      needed = perPage - currentCount
    in
      if pages <= 1 || needed <= 0 then []
      else replicate needed $
        HH.div
          [ HP.classes [ HH.ClassName "fpo-data-list__row" ]
          , HP.style "visibility: hidden; pointer-events: none;"
          ]
          [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-info" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-primary" ] ]
                  [ HH.text "\x00a0" ] -- non-breaking space for height
              , HH.div [ HP.classes [ HH.ClassName "fpo-data-list__row-secondary" ] ]
                  [ HH.text "\x00a0" ]
              ]
          ]

  filterProjects :: String -> Array DocumentHeader -> Array DocumentHeader
  filterProjects query projects =
    filter
      (\p -> contains (Pattern $ toLower query) (toLower $ DocumentHeader.getName p))
      projects
