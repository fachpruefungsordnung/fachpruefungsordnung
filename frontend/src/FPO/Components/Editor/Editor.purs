module FPO.Components.Editor
  ( Action(..)
  , Input
  , Output(..)
  , Query(..)
  , State
  , CommentState
  , SaveState
  , editor
  ) where

import Prelude

import Ace (ace, editNode) as Ace
import Ace.Anchor as Anchor
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types as Types
import Ace.UndoManager as UndoMgr
import Control.Alt ((<|>))
import Data.Array (catMaybes, filter, intercalate, null, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (find, for_, traverse_)
import Data.HashMap (delete, insert, lookup)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FPO.Components.Editor.AceExtra
  ( addClass
  , clearSelection
  , removeClass
  , screenToText
  , setAnchorPosition
  )
import FPO.Components.Editor.Keybindings
  ( keyBinding
  , makeBold
  , makeItalic
  , underscore
  )
import FPO.Components.Editor.Types
  ( AnnotationMaps
  , DragHandle(..)
  , ElementData
  , HandleBorder
  , HistoryOp(..)
  , LiveMarker
  , Path
  , RenderKind(..)
  , addAnnotationMaps
  , createMarkerRange
  , cursorInRange
  , deleteAnnotationMaps
  , emptyAnnotationMaps
  , failureLiveMarker
  , hideHandlesFrom
  , highlightSelection
  , near
  , rebuildAnnotations
  , removeLiveMarker
  , setAnnotations
  , setMarkerSelectedClass
  , showHandlesFor
  , updateMarkers
  )
import FPO.Components.TOC (Version)
import FPO.Data.AppError (AppError(..))
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Request as Request
import FPO.Data.Store (preventErrorHandlingLocally)
import FPO.Data.Store as Store
import FPO.Dto.ContentDto
  ( Content
  , ContentWrapper
  , getContentParent
  , getWrapperContent
  , setContentParent
  , setWrapperContent
  )
import FPO.Dto.ContentDto as ContentDto
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.UserDto (getUserName)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( AnnotatedMarker
  , CommentSection
  , FirstComment
  , TOCEntry
  )
import FPO.UI.HTML (decodeHtmlEntity)
import FPO.UI.Modals.DiscardModal (discardModal)
import FPO.UI.Modals.InfoModal (infoModal)
import FPO.Util (prependIf)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, enabled, ref, style, title) as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType(..), preventDefault)
import Web.Event.EventTarget
  ( EventListener
  , addEventListener
  , eventListener
  , removeEventListener
  )
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window as Win
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.UIEvent.MouseEvent as ME

foreign import _resize :: Types.Editor -> Effect Unit

type CommentState =
  {
    -- Hashmaps for Annotations
    annoMaps :: AnnotationMaps
  -- to move comment anchors
  , dragState :: Maybe { which :: DragHandle, lm :: LiveMarker }
  , startHandleMarkerId :: Maybe Int
  , endHandleMarkerId :: Maybe Int
  , mPrevHandler :: Maybe Types.Position
  , dragRowAS :: Int
  , dragColAS :: Int
  -- for not letting a drag Handler move past its partner
  , mHandleBorder :: Maybe HandleBorder
  -- tmpLiveMarker is a temporary comment and marker. Only set, if first comment is sent in Comment component
  -- Otherwise delete them later
  , tmpLiveMarker :: Maybe LiveMarker
  , selectedLiveMarker :: Maybe LiveMarker
  -- comment markers and its corresponding livemarker
  -- mostly going to use livemarkers. Markers are used for API requests
  , markers :: Array AnnotatedMarker
  , liveMarkers :: Array LiveMarker
  , commentProblem :: Boolean
  , reAnchor :: Maybe CommentSection
  }

type SaveState =
  {
    -- only save when there are new changes in editor
    mDirtyRef :: Maybe (Ref Boolean)
  -- copy of isOnMerge state label only used for beforeUnload
  , mIsOnMergeRef :: Maybe (Ref Boolean)
  -- Prevent to have multiple saving processes at the same time
  , mIsSaving :: Maybe (Ref Boolean)
  -- Are there new changes during saving?
  , mQueuedSave :: Maybe (Ref Boolean)
  , mBeforeUnloadListener :: Maybe EventListener
  , isManualSaved :: Boolean
  -- saved icon
  , showSavedIcon :: Boolean
  , mSavedIconF :: Maybe H.ForkId
  -- for periodically saving the content
  , mPendingDebounceF :: Maybe H.ForkId -- 2s-Timer
  , mPendingMaxWaitF :: Maybe H.ForkId -- 20s-Max-Timer
  }

type State = FPOState
  ( docID :: DocumentID
  , mEditor :: Maybe Types.Editor
  , mTocEntry :: Maybe TOCEntry
  , currentVersion :: String
  , mNodePath :: Maybe Path
  , mTitle :: Maybe String
  , mContent :: Maybe Content
  , html :: String
  -- comments
  , commentState :: CommentState
  , fontSize :: Int
  , mListener :: Maybe (HS.Listener Action)
  , wrapEnabled :: Boolean
  , showButtonText :: Boolean
  , showButtons :: Boolean
  -- for autosave
  , saveState :: SaveState
  -- note: this value is only used for initialisation and won't necessarily stay up to date
  -- it stores the needed input for the Init action. Receive did not work, as the page
  -- get's rendered over and over, meaning receive get's triggered over and over and the
  -- number of requests to the backend would be ridiculous
  -- furthermore, it is used to identify whether this editor is the one on the right side.
  , compareToElement :: ElementData
  , isEditorOutdated :: Boolean
  , outdatedInfoPopup :: Boolean
  , discardPopup :: Boolean
  -- similar to mDirtyRef, but for Drafts. causes popup if user tries changing version with open draft, as that would discard the draft.
  , mDirtyVersion :: Maybe (Ref Boolean)
  -- Determines whether the user is on the merge view.
  , isOnMerge :: Boolean
  -- previous isOnMerge flag for ContinueToChange
  , wasLoadedInMergeMode :: Boolean
  -- obtained from TOC. Used when merging. Set to the version details of the Version loaded into the right editor.
  , upToDateVersion :: Maybe Version
  , isLoading :: Boolean
  )

type Input = { docID :: DocumentID, elementData :: ElementData }

data Output
  = AddComment
  | ClickedQuery String
  | PostPDF String
  | RenamedNode String Path
  | RequestComments Int Int (Array Int) Boolean
  | SelectedCommentSection Int
  | ShowAllCommentsOutput
  | RaiseDiscard
  | RaiseMergeMode String
  | Merged
  | RaiseUpdateVersion (Maybe Int)
  | UpdateFullTitle
  | UpdateComment (Array Int)
  | ReaddedAnchor
  | ToUpdateCommentProblem Int

data Action
  = Init
  | DoNothing
  | Comment
  | ChangeToSection TOCEntry (Maybe Int) (Maybe String) Boolean
  | ContinueChangeToSection (Array FirstComment) Boolean Boolean
  | SelectComment
  | Font (Types.Editor -> Effect Unit)
  | FontSize (Int -> Int)
  | History HistoryOp
  | WrapToggle
  | Save Boolean
  -- Subsection of Save
  | Upload TOCEntry ContentWrapper Boolean
  | SetManualSavedFlag Boolean
  | SavedIcon
  -- new change in editor -> reset timer
  | AutoSaveTimer
  -- called by AutoSaveTimer subscription
  | AutoSave
  | TryStartDrag Number Number -- clientX, clientY
  | StartDrag DragHandle LiveMarker Number Number -- dragHandle, lm, clientX, clientY
  | DragMove Number Number -- mouse move: clientX, clientY
  | EndDrag -- mouse up
  | ShowHandles LiveMarker -- set Handles
  | HideHandles -- remove Handles
  | AddAnnotation LiveMarker Boolean
  | DeleteAnnotation LiveMarker Boolean Boolean
  | UpdateAnnotation LiveMarker
  | NotifyCommentProblem Int
  | DeleteComment Int Boolean
  | Render RenderKind
  | ShowAllComments
  | Receive (Connected FPOTranslator Input)
  | HandleResize Number
  | ToggleOutdatedInfoPopup
  | Finalize
  | Resize
  | Discard
  | CancelDiscardAction
  | ConfirmDiscardAction

-- We use a query to get the content of the editor
data Query a
  = ReceiveFullTitle (Maybe String) a
  | SetDirtyFlag a
  -- | save the current content and send it to splitview
  | SaveSection a
  -- | receive the selected TOC and put its content into the editor
  | ChangeSection TOCEntry (Maybe Int) (Maybe String) a
  | ContinueChangeSection (Array FirstComment) Boolean a
  -- | Open and edit a raw string outside the TOCEntry structure.
  --   This is used to make the editor available for editing
  --   the section names (headings) of non-leaf nodes.
  | ChangeToNode String Path (Maybe String) a
  -- | Update the position of a node in the editor, if existing.
  | UpdateNodePosition Path a
  | ConfirmComment CommentSection a
  | SelectCommentSection Int a
  | UnselectCommentSection a
  | ToDeleteComment Int Boolean a
  | RequestDirtyVersion (Boolean -> a)
  | ResetDirtyVersion a
  | ReceiveUpToDateUpdate (Maybe Version) a
  | IsOnMerge (Boolean -> a)
  | PreventChangeSection a
  | UpdateCommentProblem Boolean a
  | SetReAnchor (Maybe CommentSection) a
  | SetContent String a
  | UpdateEditorSize Number a

-- | UpdateCompareToElement ElementData a

editor
  :: forall m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
editor = connect selectTranslator $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      , finalize = Just Finalize
      }
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ]
      , HP.style "min-height: 0;"
      ] $
      [ renderAll state ]
        <> renderInfoModal
        <> renderDiscardModal
    where
    renderInfoModal = case state.outdatedInfoPopup of
      false -> []
      true ->
        [ infoModal
            state.translator
            ToggleOutdatedInfoPopup
            DoNothing
        ]
    renderDiscardModal = case state.discardPopup of
      false -> []
      true ->
        [ discardModal
            state.translator
            CancelDiscardAction
            ConfirmDiscardAction
            DoNothing
        ]

  renderAll :: State -> H.ComponentHTML Action () m
  renderAll state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ]
      , HP.style "min-height: 0;"
      ]
      [ HH.div
          -- toolbar
          [ HP.classes [ HB.dFlex, HB.justifyContentBetween ] ]
          if (not state.showButtons) then
            -- keep the toolbar even though there is not space, so that the screen doesnt pop higher
            [ HH.div
                [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter ] ]
                [ HH.div
                    [ HP.style
                        "visibility: hidden; height: 1.5rem; min-height: 1.5rem;"
                    ]
                    []
                ]
            ]
          else
            [ HH.div
                [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter ] ]
                [ case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_textBold") state.translator)
                        []
                        (Font makeBold)
                        "bi-type-bold"
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_textItalic") state.translator)
                        []
                        (Font makeItalic)
                        "bi-type-italic"
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        ( translate (label :: _ "editor_textUnderline")
                            state.translator
                        )
                        []
                        (Font underscore)
                        "bi-type-underline"

                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      buttonDivisor
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_fontSizeUp") state.translator)
                    []
                    (FontSize (\x -> x + 2))
                    "bi-plus-square"
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_fontSizeDown") state.translator)
                    []
                    (FontSize (\x -> x - 2))
                    "bi-dash-square"

                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      buttonDivisor
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_undo") state.translator)
                        []
                        (History HUndo)
                        "bi-arrow-counterclockwise"
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_redo") state.translator)
                        []
                        (History HRedo)
                        "bi-arrow-clockwise"

                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      buttonDivisor
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        (fullFeatures && state.currentVersion == "latest")
                        (translate (label :: _ "editor_comment") state.translator)
                        ( if (isJust state.commentState.reAnchor) then
                            [ H.ClassName "icon-orange" ]
                          else []
                        )
                        Comment
                        ( if
                            (not state.isEditorOutdated) &&
                              (isJust state.commentState.reAnchor) then
                            "bi-chat-square-quote-fill"
                          else "bi-chat-square-text"
                        )

                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      buttonDivisor

                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_wrapToggle") state.translator)
                        (if state.wrapEnabled then [ H.ClassName "toolbar-btn--active" ] else [])
                        WrapToggle
                        "bi-text-wrap"
                ]
            , case state.compareToElement of
                Just _ -> HH.text ""
                Nothing ->
                  HH.div
                    [ HP.classes
                        ( [ HB.m1, HB.dFlex, HB.alignItemsCenter ]
                            <> if state.showButtonText then [ HB.gap1 ] else []
                        )
                    , HP.style "min-width: 0;"
                    ]
                    [ makeEditorToolbarButtonWithText
                        true
                        state.showButtonText
                        []
                        (Save false)
                        "bi-floppy"
                        case state.isOnMerge of
                          false ->
                            (translate (label :: _ "editor_save") state.translator)
                          true ->
                            (translate (label :: _ "editor_merge") state.translator)
                    , makeEditorToolbarButtonWithText
                        fullFeatures
                        state.showButtonText
                        []
                        (Render RenderHTML)
                        "bi-file-richtext"
                        (translate (label :: _ "editor_preview") state.translator)
                    , makeEditorToolbarButtonWithText
                        fullFeatures
                        state.showButtonText
                        []
                        (Render RenderPDF)
                        "bi-filetype-pdf"
                        (translate (label :: _ "editor_pdf") state.translator)
                    , makeEditorToolbarButtonWithText
                        fullFeatures
                        state.showButtonText
                        ( case
                            state.commentState.commentProblem,
                            state.isEditorOutdated
                            of
                            true, false -> [ H.ClassName "btn-orange" ]
                            true, true -> [ H.ClassName "btn-blue" ]
                            _, _ -> []
                        )
                        ShowAllComments
                        ( case
                            state.commentState.commentProblem,
                            state.isEditorOutdated
                            of
                            true, false -> "bi-exclamation-circle-fill"
                            true, true -> "bi-clock-history"
                            _, _ -> "bi-chat-square"
                        )
                        (translate (label :: _ "editor_allComments") state.translator)
                    ]
            ]
      -- show selected TOC title below tool bar
      , HH.div
          [ HP.classes [ HB.dFlex, HB.justifyContentBetween ]
          , HP.style "padding: .5rem 1rem; border-bottom: 1px solid rgba(0,0,0,.1);"
          ]
          [ HH.h2
              [ HP.classes [ HH.ClassName "text-truncate" ]
              , HP.style "font-size: 1rem; margin: 0;"
              ]
              [ HH.text $
                  case state.mTitle of
                    Just title -> decodeHtmlEntity title
                    Nothing -> translate (label :: _ "editor_no_title")
                      state.translator
              ]
          ]
      , case state.compareToElement of
          Nothing ->
            if state.isEditorOutdated then
              HH.div
                -- toolbar
                [ HP.classes [ HB.dFlex, HB.justifyContentCenter ]
                , HP.style
                    "border-top-style: solid; border-bottom-style: solid; border-color: blue; border-width: 1px; background-color:rgba(255, 238, 164, 1);"
                ]
                if (not state.showButtons) then
                  -- keep the toolbar even though there is not space, so that the screen doesnt pop higher
                  [ HH.div
                      [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
                      [ HH.div
                          [ HP.style
                              "visibility: hidden; height: 1.5rem; min-height: 1.5rem;"
                          ]
                          []
                      ]
                  ]
                else
                  [ HH.div
                      [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
                      [ HH.text
                          case state.isOnMerge of
                            false ->
                              ( translate (label :: _ "editor_oldVersion")
                                  state.translator
                              )
                            true ->
                              ( translate (label :: _ "editor_mergingNow")
                                  state.translator
                              )
                      , makeEditorToolbarButton
                          true
                          ""
                          []
                          ToggleOutdatedInfoPopup
                          "bi bi-info-circle"
                      , makeEditorToolbarButtonWithText
                          true
                          state.showButtonText
                          []
                          Discard
                          "bi bi-trash"
                          (translate (label :: _ "editor_discard") state.translator)
                      ]
                  ]
            else
              HH.text ""
          Just _ ->
            HH.div
              -- toolbar
              [ HP.classes [ HB.dFlex, HB.justifyContentCenter ]
              , HP.style
                  "border-top-style: solid; border-bottom-style: solid; border-color: blue; border-width: 1px;"
              ]
              if (not state.showButtons) then
                -- keep the toolbar even though there is not space, so that the screen doesnt pop higher
                [ HH.div
                    [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
                    [ HH.div
                        [ HP.style
                            "visibility: hidden; height: 1.5rem; min-height: 1.5rem;"
                        ]
                        []
                    ]
                ]
              else
                [ HH.div
                    [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
                    [ HH.text
                        (translate (label :: _ "editor_readonly") state.translator)
                    ]
                ]
      , HH.div -- Editor container

          [ HP.ref (H.RefLabel "container")
          , HP.style "flex:1 1 0; min-height:0; position:relative;"
          ]
          [ -- Loading overlay only when isLoading
            if state.isLoading then
              HH.div
                [ HP.classes
                    [ HB.positionAbsolute
                    , HB.top0
                    , HB.start0
                    , HB.w100
                    , HB.h100
                    , HB.dFlex
                    , HB.justifyContentCenter
                    , HB.alignItemsCenter
                    ]
                , HP.style
                    "background: rgba(255,255,255,0.8); z-index: 30;"
                ]
                [ HH.text "Loading…" ]
            else
              HH.text ""
          ,
            -- Add overlay when right side
            case state.compareToElement of
              Just _ ->
                HH.div
                  [ HP.classes
                      [ HB.positionAbsolute
                      , HB.top0
                      , HB.start0
                      , HB.w100
                      , HB.h100
                      , HB.dFlex
                      , HB.justifyContentCenter
                      , HB.alignItemsEnd
                      , HB.peNone
                      ]
                  , HP.style
                      "background: rgba(0,0,0,0.1); z-index: 20; padding-bottom: 1.5rem;"
                  ]
                  []
              Nothing ->
                HH.text ""

          ]
      -- Saved Icon
      , if state.saveState.showSavedIcon then
          HH.div
            [ HP.classes [ HH.ClassName "save-toast" ]
            , HP.style "position: absolute; right: .5rem; bottom: .5rem; z-index: 10;"
            ]
            [ HH.text $ (translate (label :: _ "editor_save") state.translator) <>
                " 💾"
            ]
        else
          HH.text ""
      ]
    where
    fullFeatures = isJust state.mTocEntry

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Init -> do
      compareToElement <- H.gets _.compareToElement
      { emitter, listener } <- H.liftEffect HS.create
      _ <- H.subscribe emitter

      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
        editor_ <- H.liftEffect $ Ace.editNode el Ace.ace
        fontSize <- H.gets _.fontSize

        -- setting for both instances and later add more specific settings
        H.liftEffect $ setupAce editor_ fontSize

        case compareToElement of
          Nothing -> do

            -- Setup editor functionality (keydown listeners, Ace editor stuff, etc.)
            let
              onSave :: Effect Unit
              onSave = HS.notify listener (Save false)

            H.modify_ _
              { wrapEnabled = true
              , mEditor = Just editor_
              , mListener = Just listener
              }

            H.liftEffect $ do
              eventListen <- eventListener (keyBinding onSave editor_)
              container <- Editor.getContainer editor_
              addEventListener keydown eventListen true
                (toEventTarget $ toElement container)

              -- Set the editor's theme and mode
              Editor.setEnableLiveAutocompletion true editor_

          -- If a comparison element is loaded, also load the current content in the primary editor
          Just { tocEntry: tocEntry, revID: revID } -> handleAction
            (ChangeToSection tocEntry revID Nothing false)

        -- New Ref for keeping track, if the content in editor has changed
        -- 1. since last save
        -- 2. since opening version
        dirtyRef <- H.liftEffect $ Ref.new false
        isOnMergeRef <- H.liftEffect $ Ref.new false
        versionRef <- H.liftEffect $ Ref.new false
        H.modify_ _ { mDirtyVersion = Just versionRef }

        -- create eventListener for preventing the tab from closing
        -- when content has not been saved (Not changing through Navbar)
        addBeforeUnloadListener dirtyRef isOnMergeRef listener

        -- add and start Editor listeners
        H.liftEffect $ addChangeListenerWithRef editor_ dirtyRef versionRef listener
        container <- H.liftEffect $ Editor.getContainer editor_
        H.liftEffect $ addMouseDragListeners container listener

    DoNothing -> do
      pure unit

    -- Resize the editor's rendering (called when content changes internally)
    Resize -> do
      state <- H.get
      H.liftEffect
        case state.mEditor of
          Nothing -> pure unit
          Just e ->
            _resize e

    CancelDiscardAction -> do
      H.modify_ _ { discardPopup = false }

    ConfirmDiscardAction -> do
      H.modify_ _ { discardPopup = false }
      H.raise RaiseDiscard

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

    ToggleOutdatedInfoPopup -> do
      state <- H.get
      let toggledInfo = (state.outdatedInfoPopup == false)
      H.modify_ _ { outdatedInfoPopup = toggledInfo }

    Font format -> do
      compareToElement <- H.gets _.compareToElement
      when (compareToElement == Nothing) do
        H.gets _.mEditor >>= traverse_ \ed ->
          H.liftEffect $ do
            format ed
            Editor.focus ed

    FontSize change -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        let newSize = change state.fontSize
        H.modify_ _ { fontSize = newSize }
        -- Set the new font size in the editor
        H.liftEffect $ do
          Editor.setFontSize (show newSize <> "px") ed
          Editor.focus ed

    History format -> do
      compareToElement <- H.gets _.compareToElement
      when (compareToElement == Nothing) $ do
        H.gets _.mEditor >>= traverse_ \ed -> do
          H.liftEffect $ do
            case format of
              HUndo -> Editor.undo ed
              HRedo -> Editor.redo ed
            Editor.focus ed

    WrapToggle -> do
      editor_ <- H.gets _.mEditor
      wrapEnabled <- H.gets _.wrapEnabled
      case editor_ of
        Nothing -> pure unit
        Just e -> do
          let newWrapEnabled = not wrapEnabled
          H.liftEffect $ do
            session <- Editor.getSession e
            Session.setUseWrapMode newWrapEnabled session
          H.modify_ _ { wrapEnabled = newWrapEnabled }

    Discard ->
      H.modify_ _ { discardPopup = true }

    Render renderType -> do
      case renderType of
        RenderHTML -> do
          html <- H.gets _.html
          H.raise (ClickedQuery html)
        -- TODO change this later when backend is ready
        RenderPDF -> do
          state <- H.get
          H.raise (PostPDF state.currentVersion)
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ Editor.focus ed

    ShowAllComments -> do
      H.raise $ ShowAllCommentsOutput
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ Editor.focus ed
      H.modify_ \st -> st { commentState = st.commentState { reAnchor = Nothing } }

    Save isAutoSave -> do
      when (not isAutoSave) $
        H.gets _.mEditor >>= traverse_ \ed ->
          H.liftEffect $ Editor.focus ed
      state <- H.get
      when (state.compareToElement == Nothing) $ do
        isSaving <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
          _.saveState.mIsSaving
        isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
          _.saveState.mDirtyRef
        -- Only save, when dirty flag is true or we are in older version
        -- TODO: Add another flag instead of using isEditorOutdated
        if
          ( (not isSaving) &&
              ( isDirty || (not state.saveState.isManualSaved) ||
                  state.isEditorOutdated
              )
          ) then do
          -- mIsSaving := true, mDirtyRef := false
          for_ state.saveState.mIsSaving \r -> H.liftEffect $ Ref.write true r
          for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write false r
          when (not isAutoSave)
            $ handleAction
            $ SetManualSavedFlag true
          allLines <- H.gets _.mEditor >>= traverse \ed -> do
            H.liftEffect $ Editor.getSession ed
              >>= Session.getDocument
              >>= Document.getAllLines

          let
            contentLines = intercalate "\n" (fromMaybe [] allLines)

          case state.mTocEntry of
            Nothing -> do
              -- No leaf entity was selected, so if a nodePath is set,
              -- we can emit an event to rename the node.
              case state.mNodePath of
                Nothing -> do
                  pure unit -- Nothing to do
                Just path -> do
                  H.raise $ RenamedNode contentLines path
                  H.modify_ _ { mTitle = Just contentLines }
              freeSaveFlagsAndMaybeRerun
            Just entry ->
              case state.mContent of
                Nothing -> do
                  freeSaveFlagsAndMaybeRerun
                  pure unit
                Just content -> do
                  -- Save the current content of the editor and send it to the server
                  let
                    -- place it in contentDto
                    newContent = ContentDto.setContentText contentLines content

                  -- Since the ids and postions in liveMarkers are changing constantly,
                  -- extract them now and store them
                  updatedMarkers <- H.liftEffect do
                    for state.commentState.markers \m -> do
                      case
                        find (\lm -> lm.annotedMarkerID == m.id)
                          state.commentState.liveMarkers
                        of
                        -- TODO Should we add other markers in liveMarkers such as errors?
                        Nothing -> pure m
                        Just lm -> do
                          start <- Anchor.getPosition lm.startAnchor
                          end <- Anchor.getPosition lm.endAnchor
                          pure m
                            { startRow = Types.getRow start
                            , startCol = Types.getColumn start
                            , endRow = Types.getRow end
                            , endCol = Types.getColumn end
                            }

                  -- convert into a newtype for encodeJson
                  let
                    comments = map ContentDto.convertToCommentAnchor updatedMarkers
                    newWrapper = ContentDto.setWrapper newContent comments
                  -- Try to upload
                  handleAction $ Upload entry newWrapper isAutoSave
        else if (isSaving && (isDirty || not state.saveState.isManualSaved)) then do
          for_ state.saveState.mQueuedSave \r -> H.liftEffect $ Ref.write true r
          when (not isAutoSave)
            $ handleAction
            $ SetManualSavedFlag true
        -- Users want to have a visual indicator, that they have saved. So when manual saving without any changes since
        -- last Save, just show the notification
        else
          when
            ( not isAutoSave && state.saveState.isManualSaved && isJust
                state.mTocEntry
            )
            $ updateStore
            $ Store.AddSuccess
                (translate (label :: _ "editor_already_saved") state.translator)

    Upload newEntry newWrapper isAutoSave -> do
      state <- H.get
      let
        --modify the wrapper if merging to allow saving old versions.
        modifiedWrapper = case isAutoSave of
          true -> newWrapper
          false -> case state.isOnMerge of
            true -> case state.upToDateVersion of
              Nothing -> newWrapper
              Just { author: _, identifier: id, timestamp: _ } ->
                case id of
                  Nothing -> newWrapper
                  Just i ->
                    ( setWrapperContent
                        (setContentParent i (getWrapperContent newWrapper))
                        newWrapper
                    )
            false -> newWrapper
        jsonContent = ContentDto.encodeWrapper modifiedWrapper
        newContent = ContentDto.getWrapperContent modifiedWrapper
      -- send the new content as POST to the server
      response <- Request.postJson (ContentDto.extractDraft newContent)
        ( "/docs/" <> show state.docID <> "/text/" <> show newEntry.id
            <> "/rev?isAutoSave="
            <> show isAutoSave
        )
        jsonContent
      -- handle errors in pos and decodeJson
      case response of
        -- if error, try to Save again (Maybe ParentID is lost?)
        Left err -> Store.addError err

        -- extract and insert new parentID into newContent
        -- not updating the received comment anchors, as we send those same anchors to backend
        Right { content: updatedContent, typ, html } -> do

          -- Show saved icon or toast
          case isAutoSave, state.isEditorOutdated of
            -- auto save interaction
            true, _ -> do
              handleAction SavedIcon
              case typ of
                "noConflict" -> pure unit
                "draftCreated" -> do
                  H.raise (RaiseUpdateVersion (Just $ getContentParent newContent))
                  pure unit --raise something to update version
                "conflict" -> pure unit --should not happen here also raise something just in case
                _ -> pure unit
            -- manual save while working in latest version
            false, false -> do
              updateStore $ Store.AddSuccess
                (translate (label :: _ "editor_save_success") state.translator)
              case typ of
                "noConflict" -> do
                  case state.isOnMerge of
                    false -> pure unit
                    true -> H.raise Merged
                  pure unit
                "draftCreated" -> --should not happen here. just copy autosave case in case

                  pure unit
                "conflict" -> do --raise something to update version
                  setIsOnMerge true
                  H.raise $ RaiseMergeMode $ ContentDto.getContentText $
                    ContentDto.getWrapperContent newWrapper
                  handleAction $ ChangeToSection newEntry Nothing state.mTitle true
                _ -> pure unit
            -- manual save, draft mode => publish
            false, true -> do
              case typ of
                --happens if parent was updated due to merge view being present.
                "noConflict" -> do
                  case state.isOnMerge of
                    false -> pure unit
                    true -> H.raise Merged
                  pure unit
                "draftCreated" -> --should not happen here. just copy autosave case in case

                  pure unit
                "conflict" -> do --raise something to update version
                  setIsOnMerge true
                  H.raise $ RaiseMergeMode $ ContentDto.getContentText $
                    ContentDto.getWrapperContent newWrapper
                  handleAction $ ChangeToSection newEntry Nothing state.mTitle true
                  pure unit
                _ -> pure unit

          when (typ /= "conflict") do
            H.modify_ _ { mContent = Just updatedContent, html = html }
            H.raise UpdateFullTitle
            isOnMerge' <- H.gets _.isOnMerge
            when (not isOnMerge')
              $ H.raise
              $ ClickedQuery html

          pure unit

      H.raise $ UpdateComment $ map ContentDto.getCommentAnchorID
        (ContentDto.getWrapperComments newWrapper)
      freeSaveFlagsAndMaybeRerun

    SetManualSavedFlag flag ->
      H.modify_ \st -> st { saveState = st.saveState { isManualSaved = flag } }

    SavedIcon -> do
      mSavedIconF <- H.gets _.saveState.mSavedIconF
      -- restart saved icon
      for_ mSavedIconF H.kill
      -- start new fiber
      iFib <- H.fork do
        H.liftAff $ delay (Milliseconds 1200.0)
        H.modify_ \st -> st
          { saveState = st.saveState
              { showSavedIcon = false
              , mSavedIconF = Nothing
              }
          }
      H.modify_ \st -> st
        { saveState =
            st.saveState
              { mSavedIconF = Just iFib
              , showSavedIcon = true
              }
        }

    AutoSaveTimer -> do
      -- restart 5 sec timer after every new input
      -- first kill the maybe running fiber (kinda like a thread)
      debounce <- H.gets _.saveState.mPendingDebounceF
      traverse_ H.kill debounce

      -- start a new fiber
      dFib <- H.fork do
        H.liftAff $ delay (Milliseconds 5000.0)
        mMax <- H.gets _.saveState.mPendingMaxWaitF
        traverse_ H.kill mMax
        H.modify_ \st -> st
          { saveState = st.saveState
              { mPendingDebounceF = Nothing
              , mPendingMaxWaitF = Nothing
              }
          }
        isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
          _.saveState.mDirtyRef
        when isDirty $ handleAction AutoSave
      H.modify_ \st -> st
        { saveState = st.saveState { mPendingDebounceF = Just dFib } }

      -- This is a seperate 60 sec timer, which forces to save, in case of a long edit
      -- does not reset with new input
      mPendingMaxWaitF <- H.gets _.saveState.mPendingMaxWaitF
      case mPendingMaxWaitF of
        -- timer already running
        Just _ -> pure unit
        -- no timer there
        Nothing -> do
          mFib <- H.fork do
            H.liftAff $ delay (Milliseconds 60000.0)
            latestDebounce <- H.gets _.saveState.mPendingDebounceF
            traverse_ H.kill latestDebounce
            H.modify_ \st -> st
              { saveState = st.saveState
                  { mPendingDebounceF = Nothing
                  , mPendingMaxWaitF = Nothing
                  }
              }
            isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
              _.saveState.mDirtyRef
            when isDirty $ handleAction AutoSave
          H.modify_ \st -> st
            { saveState = st.saveState { mPendingMaxWaitF = Just mFib } }

    AutoSave -> do
      mDeb <- H.gets _.saveState.mPendingDebounceF
      traverse_ H.kill mDeb
      mMax <- H.gets _.saveState.mPendingMaxWaitF
      traverse_ H.kill mMax
      H.modify_ \st -> st
        { saveState = st.saveState
            { mPendingDebounceF = Nothing
            , mPendingMaxWaitF = Nothing
            }
        }
      -- only save, if dirty
      isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
        _.saveState.mDirtyRef
      when isDirty do
        handleAction $ Save true

    Comment -> do
      state <- H.get
      userWithError <- getUser
      case userWithError, state.mEditor, state.mListener of
        Right user, Just ed, Just listener -> do
          session <- H.liftEffect $ Editor.getSession ed
          range <- H.liftEffect $ Editor.getSelectionRange ed
          start <- H.liftEffect $ Range.getStart range
          end <- H.liftEffect $ Range.getEnd range
          let
            sRow = Types.getRow start
            sCol = Types.getColumn start
            eRow = Types.getRow end
            eCol = Types.getColumn end

          if (sRow /= eRow || sCol /= eCol) then do
            case state.commentState.reAnchor of
              Just reAnchor -> do
                for_ reAnchor.first \first -> do
                  let
                    newMarker =
                      { id: first.markerID
                      , type: "info"
                      , startRow: sRow
                      , startCol: sCol
                      , endRow: eRow
                      , endCol: eCol
                      , markerText: first.comment.author
                      , mCommentSection: Just reAnchor
                      }
                    newMarkers = snoc state.commentState.markers newMarker
                  newLiveMarker <- H.liftEffect $ addAnchor newMarker session listener
                    true
                  case newLiveMarker of
                    Nothing -> do
                      pure unit
                    -- Optionally, show an error modal or log the error
                    -- H.liftEffect $ infoModal "Failed to add anchor. Please try again."
                    Just lm' -> do
                      let
                        newCommentState =
                          state.commentState
                            { selectedLiveMarker = Just lm'
                            , markers = newMarkers
                            , liveMarkers = snoc state.commentState.liveMarkers lm'
                            , reAnchor = Nothing
                            }
                      H.modify_ _ { commentState = newCommentState }
                      H.raise (ReaddedAnchor)
                      -- show comment dragger handles
                      H.liftEffect $ highlightSelection ed
                        (snoc state.commentState.liveMarkers lm')
                        lm'
                      handleAction (ShowHandles lm')
                      -- Save readded comment anchor
                      -- set dirty to true to be able to save
                      for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write
                        true
                        r
                      handleAction $ SetManualSavedFlag false
                      handleAction $ Save true
              _ -> do

                -- delete temporary live marker, as the first comment has not been sent
                case state.commentState.tmpLiveMarker of
                  Just lm -> do
                    H.liftEffect $ removeLiveMarker lm session
                    handleAction $ DeleteAnnotation lm false false
                  Nothing -> pure unit

                let
                  userName = getUserName user
                  newMarker =
                    { id: -360
                    , type: "info"
                    , startRow: sRow
                    , startCol: sCol
                    , endRow: eRow
                    , endCol: eCol
                    , markerText: userName
                    , mCommentSection: Nothing
                    }

                mLiveMarker <- H.liftEffect $ addAnchor newMarker session listener
                  true

                case state.mTocEntry of
                  Just _ -> do
                    H.modify_ \st -> st
                      { commentState = st.commentState
                          { tmpLiveMarker = mLiveMarker
                          , selectedLiveMarker = mLiveMarker
                          }
                      }
                    H.raise AddComment
                  Nothing -> pure unit

                -- show comment dragger handles
                case mLiveMarker of
                  Nothing -> pure unit
                  Just lm -> do
                    H.liftEffect $ highlightSelection ed
                      (snoc state.commentState.liveMarkers lm)
                      lm
                    handleAction (ShowHandles lm)
            -- remove the selection
            H.liftEffect $ clearSelection ed
          else
            H.liftEffect $ Editor.focus ed

        _, _, _ -> pure unit -- TODO error handling

    SelectComment -> do
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        range <- H.liftEffect $ Editor.getSelectionRange ed
        start <- H.liftEffect $ Range.getStart range
        end <- H.liftEffect $ Range.getEnd range
        let
          sRow = Types.getRow start
          sCol = Types.getColumn start
          eRow = Types.getRow end
          eCol = Types.getColumn end

        if (sRow /= eRow || sCol /= eCol) then
          pure unit
        else do
          let
            liveMarkers = case state.commentState.tmpLiveMarker of
              Nothing -> state.commentState.liveMarkers
              Just lm -> snoc state.commentState.liveMarkers lm
          cursor <- H.liftEffect $ Editor.getCursorPosition ed
          session <- H.liftEffect $ Editor.getSession ed
          foundLM <- H.liftEffect $ cursorInRange liveMarkers cursor
          -- comment section dragger handles
          case foundLM of
            Nothing -> do
              -- remove selection and remove handles
              H.modify_ \st -> st
                { commentState = st.commentState
                    { selectedLiveMarker = Nothing
                    , dragState = Nothing
                    }
                }
              case state.commentState.selectedLiveMarker of
                Nothing -> pure unit
                Just lm -> H.liftEffect $ setMarkerSelectedClass session lm false
              handleAction HideHandles
            Just lm -> do
              case state.commentState.selectedLiveMarker of
                Just old | old.annotedMarkerID /= lm.annotedMarkerID ->
                  H.liftEffect $ setMarkerSelectedClass session old false
                _ -> pure unit
              -- set selection and highlight it
              H.modify_ \st -> st
                { commentState = st.commentState { selectedLiveMarker = Just lm } }
              H.liftEffect $ highlightSelection ed liveMarkers lm
              handleAction (ShowHandles lm)
              when (lm.annotedMarkerID >= 0 || lm.annotedMarkerID == -360) $
                H.raise (SelectedCommentSection lm.annotedMarkerID)

    -- Comment Section Dragger Actions

    -- Try to get mouse position and maybe selected handle
    TryStartDrag clientX clientY -> do
      mEditor <- H.gets _.mEditor
      selectedLiveMarker <- H.gets _.commentState.selectedLiveMarker
      case mEditor, selectedLiveMarker of
        Just ed, Just lm -> do
          -- mouse position -> text position
          pos <- H.liftEffect $ screenToText ed clientX clientY
          sPos <- H.liftEffect $ Anchor.getPosition lm.startAnchor
          ePos <- H.liftEffect $ Anchor.getPosition lm.endAnchor

          if near pos sPos then do
            let
              row = Types.getRow ePos
              column = Types.getColumn ePos
            H.modify_ \st -> st
              { commentState = st.commentState
                  { mPrevHandler = Just sPos
                  , mHandleBorder = Just
                      { row
                      , column
                      , side: DragStart
                      }
                  }
              }
            handleAction (StartDrag DragStart lm clientX clientY)
          else if near pos ePos then do
            let
              row = Types.getRow sPos
              column = Types.getColumn sPos
            H.modify_ \st -> st
              { commentState = st.commentState
                  { mPrevHandler = Just ePos
                  , mHandleBorder = Just
                      { row
                      , column
                      , side: DragEnd
                      }
                  }
              }
            handleAction (StartDrag DragEnd lm clientX clientY)
          else
            pure unit
        _, _ -> pure unit

    StartDrag which lm _clientX _clientY -> do
      state <- H.get
      when (state.compareToElement == Nothing) do
        case state.mEditor of
          Just ed -> do
            session <- H.liftEffect $ Editor.getSession ed
            container <- H.liftEffect $ Editor.getContainer ed
            -- For CSS identification and prevent from highlighting while dragging
            H.liftEffect do
              addClass container "fpo-dragging"
            -- remove old Handles
            H.liftEffect $ hideHandlesFrom session
              state.commentState.startHandleMarkerId
              state.commentState.endHandleMarkerId
            -- set new Handles
            ids <- H.liftEffect $ showHandlesFor session lm
            H.modify_ \st -> st
              { commentState = st.commentState
                  { dragState = Just { which, lm }
                  , startHandleMarkerId = ids.startId
                  , endHandleMarkerId = ids.endId
                  }
              }
          Nothing -> pure unit

    DragMove clientX clientY -> do
      commentState <- H.gets _.commentState
      mEditor <- H.gets _.mEditor
      case commentState.dragState, mEditor, commentState.mHandleBorder of
        Just { which, lm }, Just ed, Just { row, column, side } ->
          do
            -- screen -> Textcoord.
            pos <- H.liftEffect $ screenToText ed clientX clientY
            let
              r0 = Types.getRow pos
              c0 = Types.getColumn pos
              -- Should not overstep the row
              row' = case side of
                DragStart -> if r0 > row then row else r0
                DragEnd -> if r0 < row then row else r0
              -- If same row, than check the column
              col' =
                if row' == row then
                  case side of
                    DragStart -> if c0 >= column then max 0 (column - 1) else c0
                    DragEnd -> if c0 <= column then column + 1 else c0
                else c0

              wouldOverlapSameRow =
                row' == row &&
                  case side of
                    DragStart -> col' >= column
                    DragEnd -> col' <= column

            Tuple rowFinal colFinal <-
              if wouldOverlapSameRow then do
                cur <- H.liftEffect case which of
                  DragStart -> Anchor.getPosition lm.startAnchor
                  DragEnd -> Anchor.getPosition lm.endAnchor
                pure $ Tuple (Types.getRow cur) (Types.getColumn cur)
              else
                pure $ Tuple row' col'

            -- set drag anchor
            case which of
              DragStart -> H.liftEffect $ setAnchorPosition lm.startAnchor rowFinal
                colFinal
              DragEnd -> H.liftEffect $ setAnchorPosition lm.endAnchor rowFinal
                colFinal

            -- draw new Handles (current position)
            session <- H.liftEffect $ Editor.getSession ed
            H.liftEffect $ hideHandlesFrom session commentState.startHandleMarkerId
              commentState.endHandleMarkerId
            ids <- H.liftEffect $ showHandlesFor session lm
            H.modify_ \st -> st
              { commentState = st.commentState
                  { startHandleMarkerId = ids.startId
                  , endHandleMarkerId = ids.endId
                  , dragRowAS = rowFinal
                  , dragColAS = colFinal
                  }
              }
        _, _, _ -> pure unit

    EndDrag -> do
      dragState <- H.gets _.commentState.dragState
      when (isJust dragState) do
        H.modify_ \st -> st { commentState = st.commentState { dragState = Nothing } }
        state <- H.get
        case state.mEditor of
          Just ed -> do
            container <- H.liftEffect $ Editor.getContainer ed
            -- For CSS styling
            H.liftEffect do
              removeClass container "fpo-dragging"
              -- remove the selected text in editor
              clearSelection ed

            -- Auto save
            case state.commentState.mPrevHandler, state.saveState.mDirtyRef of
              Just prev, Just dirtyRef -> do
                let
                  pRow = Types.getRow prev
                  pCol = Types.getColumn prev
                when
                  ( pRow /= state.commentState.dragRowAS || pCol /=
                      state.commentState.dragColAS
                  )
                  do
                    H.modify_ \st -> st
                      { commentState = st.commentState { mPrevHandler = Nothing } }
                    -- set dirty flag and autosave
                    H.liftEffect $ Ref.write true dirtyRef
                    handleAction $ SetManualSavedFlag false
                    handleAction AutoSaveTimer
              _, _ -> pure unit
          Nothing -> pure unit

    ShowHandles lm -> do
      mEditor <- H.gets _.mEditor
      commentState <- H.gets _.commentState
      case mEditor of
        Nothing -> pure unit
        Just ed -> do
          session <- H.liftEffect $ Editor.getSession ed
          -- remove old markers
          H.liftEffect $ hideHandlesFrom session commentState.startHandleMarkerId
            commentState.endHandleMarkerId
          -- set new ones
          ids <- H.liftEffect $ showHandlesFor session lm -- :: { startId :: Maybe Int, endId :: Maybe Int }
          H.modify_ \st -> st
            { commentState = st.commentState
                { startHandleMarkerId = ids.startId
                , endHandleMarkerId = ids.endId
                }
            }

    HideHandles -> do
      mEditor <- H.gets _.mEditor
      commentState <- H.gets _.commentState
      case mEditor of
        Nothing -> pure unit
        Just ed -> do
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ hideHandlesFrom session commentState.startHandleMarkerId
            commentState.endHandleMarkerId
          H.modify_ \st -> st
            { commentState = st.commentState
                { startHandleMarkerId = Nothing
                , endHandleMarkerId = Nothing
                }
            }

    AddAnnotation lm setAnn -> do
      commentState <- H.gets _.commentState
      maps <- H.liftEffect $ addAnnotationMaps lm commentState.annoMaps
      H.modify_ \st -> st
        { commentState = st.commentState { annoMaps = maps } }
      when setAnn do
        mEditor <- H.gets _.mEditor
        H.liftEffect $ setAnnotations maps.markerAnnoHS mEditor

    DeleteAnnotation lm reAdd setAnn -> do
      commentState <- H.gets _.commentState
      let
        maps = deleteAnnotationMaps lm commentState.annoMaps
      H.modify_ \st -> st
        { commentState = st.commentState { annoMaps = maps } }
      -- if we want to update the live marker annotation
      if reAdd then
        handleAction $ AddAnnotation lm setAnn
      else
        when setAnn do
          mEditor <- H.gets _.mEditor
          H.liftEffect $ setAnnotations maps.markerAnnoHS mEditor

    -- delete and readd Annotation if the startRow of live marker has changed
    UpdateAnnotation lm -> do
      oldMarkerAnnoPos <- H.gets _.commentState.annoMaps.oldMarkerAnnoPos
      -- get startRow from live marker
      pos <- H.liftEffect $ Anchor.getPosition lm.startAnchor
      let startRow = Types.getRow pos

      -- checking if the startRow has changed
      case lookup lm.annotedMarkerID oldMarkerAnnoPos of
        Nothing -> pure unit
        Just oldRow ->
          when (startRow /= oldRow)
            $ handleAction
            $ DeleteAnnotation lm true true

    NotifyCommentProblem markerID -> do
      state <- H.get

      let
        markers' = state.commentState.markers <#> \m ->
          if m.id == markerID then
            m
              { startRow = m.endRow
              , startCol = m.endCol
              }
          else m

      H.modify_ \st -> st
        { commentState = st.commentState
            { commentProblem = true
            , markers = markers'
            }
        }

      H.raise $ ToUpdateCommentProblem markerID

    DeleteComment markerID commentProblem -> do
      state <- H.get
      mLm <- H.liftEffect $ resolveDeleteLiveMarker markerID state.commentState
      let
        cs = state.commentState
        targetId = maybe markerID _.annotedMarkerID mLm
        isTarget lm = lm.annotedMarkerID == targetId

        dropIfTarget mlm =
          mlm >>= \lm -> if isTarget lm then Nothing else Just lm

        newTmp = dropIfTarget cs.tmpLiveMarker
        newLiveMarkers = filter (not <<< isTarget) cs.liveMarkers
        liveMarkersForAnno =
          case newTmp of
            Nothing -> newLiveMarkers
            Just lm -> snoc newLiveMarkers lm
        wasSelected = maybe false isTarget cs.selectedLiveMarker
        isTargetMarker m = m.id == markerID || m.id == targetId
        newOldMarkerAnnoPos =
          delete targetId (delete markerID cs.annoMaps.oldMarkerAnnoPos)

      didDelete <- case state.mEditor, mLm of
        Just ed, Just lm -> do
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ removeLiveMarker lm session
          handleAction $ DeleteAnnotation lm false true
          when wasSelected (handleAction HideHandles)
          pure true
        _, _ -> pure false

      mAnno <-
        if didDelete then
          pure Nothing
        else do
          Just <$> H.liftEffect
            (rebuildAnnotations liveMarkersForAnno state.mEditor)

      H.modify_ \st ->
        st
          { commentState = st.commentState
              { commentProblem = commentProblem
              , reAnchor = case st.commentState.reAnchor of
                  Just r | r.markerID == markerID -> Nothing
                  other -> other
              , selectedLiveMarker =
                  dropIfTarget st.commentState.selectedLiveMarker
              , tmpLiveMarker = newTmp
              , liveMarkers = newLiveMarkers
              , markers = filter (not <<< isTargetMarker)
                  st.commentState.markers
              , annoMaps =
                  st.commentState.annoMaps { oldMarkerAnnoPos = newOldMarkerAnnoPos }
              }
          }
      case mAnno of
        Nothing -> pure unit
        Just a ->
          H.modify_ \st ->
            st { commentState = st.commentState { annoMaps = a } }

      -- save as before
      for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write true r
      handleAction $ SetManualSavedFlag false
      handleAction $ Save true

    -- Handle external resize from parent component (Splitview)
    -- Updates UI elements based on width and triggers editor resize
    HandleResize width -> do
      -- Decides whether to show button text based on the width.
      -- Because german labels are longer, we need to adjust the cutoff
      -- threshold dynamically.
      lang <- H.liftEffect $ Store.loadLanguage
      let cutoff = if lang == Just "de-DE" then 690.0 else 592.0
      let noButtonsCutoff = 350.0

      H.modify_ _
        { showButtonText = width >= cutoff, showButtons = width >= noButtonsCutoff }

      -- Let the editor itself handle internal details like line breaks with resizing
      editor_ <- H.gets _.mEditor
      case editor_ of
        Nothing -> pure unit
        Just ed -> H.liftEffect $ Editor.resize (Just true) ed

    Finalize -> do
      -- Save in case, the user changes the page (via Navbar)
      state <- H.get
      if state.isOnMerge then
        handleAction ConfirmDiscardAction
      else
        handleAction $ Save true
      win <- H.liftEffect window
      let
        tgt = Win.toEventTarget win
        beforeunload = EventType "beforeunload"
      case state.saveState.mBeforeUnloadListener of
        Just l -> H.liftEffect $ removeEventListener beforeunload l false tgt
        _ -> pure unit
      for_ state.saveState.mPendingDebounceF H.kill
      for_ state.saveState.mPendingMaxWaitF H.kill

    ChangeToSection entry rev mTitle loadInMergeMode -> do
      state <- H.get
      let
        version = case rev of
          Nothing -> "latest"
          Just v -> show v
      -- Prevent of loading the same Section from backend again
      when
        ( map _.id state.mTocEntry /= Just entry.id || version /= state.currentVersion
            || state.wasLoadedInMergeMode /= loadInMergeMode
        )
        do
          -- Get the content from server here
          -- We need Aff for that and thus cannot go inside Eff
          -- TODO: After creating a new Leaf, we get Nothing in loadedContent
          -- See, why and fix it

          H.modify_ _ { isLoading = true }

          --first we look whether a draft to load is present. The right editor does not load drafts
          loadedDraftContent <- case state.compareToElement, loadInMergeMode of
            Nothing, false ->
              preventErrorHandlingLocally $ Request.getJson
                ContentDto.decodeContentWrapper
                ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
                    <> "/draft"
                )
            _, _ -> pure $ Left $ NotFoundError "No Draft Found"

          -- check, if draft is present. Otherwise get from version
          loadedContent <- case loadedDraftContent of
            Right res -> do
              pure (Right res)
            Left _ -> do
              Request.getJson
                ContentDto.decodeContentWrapper
                ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
                    <> "/rev/"
                    <> version
                )

          case loadedContent of
            Left err -> do
              H.modify_ _ { isLoading = false }
              Store.addError err
            Right wrapper -> do
              let
                content = ContentDto.getWrapperContent wrapper
                html = ContentDto.getWrapperHtml wrapper

              H.modify_ _
                { mTocEntry = Just entry
                , currentVersion = version
                , mTitle = mTitle
                , mContent = Just content
                , html = html
                , wasLoadedInMergeMode = loadInMergeMode
                }
              setIsOnMerge loadInMergeMode

              -- Only secondary Editor has ElementData
              -- Only first Editor gets to load the comments
              if isJust state.compareToElement then do
                handleAction $ ContinueChangeToSection [] false false
              else do
                -- Get comments
                let
                  comments = ContentDto.getWrapperComments wrapper
                  -- convert markers
                  markers = map ContentDto.convertToAnnotetedMarker comments
                  validMarker m =
                    m.startRow < m.endRow ||
                      (m.startRow == m.endRow && m.startCol < m.endCol)
                  validMarkers = filter validMarker markers
                  markerIDs = map (\m -> m.id) validMarkers
                  isDraftAvailable = case loadedDraftContent of
                    Right _ -> true
                    Left _ -> false
                  isEditorOutdated' = version /= "latest" || loadInMergeMode ||
                    isDraftAvailable
                -- update the markers into state
                H.modify_ \st -> st
                  { commentState = st.commentState
                      { selectedLiveMarker = Nothing
                      , annoMaps = emptyAnnotationMaps
                      , markers = validMarkers
                      }
                  , isEditorOutdated = isEditorOutdated'
                  }
                -- Get comments information from Comment Child
                H.raise
                  ( RequestComments state.docID entry.id markerIDs
                      (not isEditorOutdated')
                  )

          --will be set to true right now, but should be set to false if didn't change to draft
          case loadedDraftContent of
            Right _ -> pure unit
            Left _ -> for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write false r
      pure unit

    -- After getting information from from Comment
    ContinueChangeToSection fCs showHtml commentProblem -> do
      state <- H.get
      -- Put the content of the section into the editor and update markers
      H.gets _.mEditor >>= traverse_ \ed -> do
        let
          commentState = state.commentState
          filMarkers = updateMarkers fCs commentState.markers
          content = case state.mContent of
            Nothing -> ""
            Just c -> ContentDto.getContentText c
        handleAction Resize
        newLiveMarkers <- H.liftEffect do
          session <- Editor.getSession ed
          document <- Session.getDocument session

          -- Set the content of the editor
          Document.setValue content document
          Editor.setReadOnly (state.compareToElement /= Nothing) ed

          -- reset Ref, because loading new content is considered
          -- changing the existing content, which would set the flag
          for_ state.saveState.mDirtyRef \r -> Ref.write false r

          -- Reset Undo history
          undoMgr <- Session.getUndoManager session
          UndoMgr.reset undoMgr

          case state.mListener of
            Nothing -> pure []
            Just listener -> do
              -- Remove existing markers
              for_ commentState.liveMarkers \lm -> do
                removeLiveMarker lm session

              -- Clear annotations
              Session.clearAnnotations session

              -- Add annotations from marker
              tmp <- for filMarkers \marker -> do
                addAnchor marker session listener true

              pure (catMaybes tmp)

        -- Update state with new marker IDs
        when (state.compareToElement == Nothing) do
          mDeb <- H.gets _.saveState.mPendingDebounceF
          traverse_ H.kill mDeb
          mMax <- H.gets _.saveState.mPendingMaxWaitF
          traverse_ H.kill mMax
        H.modify_ \st -> st
          { commentState = st.commentState
              { liveMarkers = newLiveMarkers, commentProblem = commentProblem }
          , saveState = st.saveState
              { mPendingDebounceF = Nothing
              , mPendingMaxWaitF = Nothing
              , isManualSaved = true
              }
          , isLoading = false
          }
        -- lastly show html in preview
        when (showHtml && not state.isOnMerge) $ do
          html' <- H.gets _.html
          H.raise $ ClickedQuery html'

  -- convert Hashmap to Annotations and show them
  -- H.liftEffect $ setAnnotations commentState.annoMaps.markerAnnoHS state.mEditor

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveUpToDateUpdate mVersion a -> do
      H.modify_ _ { upToDateVersion = mVersion }
      -- there is a new container above the editor. Resize editor to be able to scroll all the way down
      handleAction Resize
      pure (Just a)

    ChangeSection entry rev mTitle a -> do
      handleAction (ChangeToSection entry rev mTitle false)
      pure (Just a)

    ContinueChangeSection fCs commentProblem a -> do
      handleAction (ContinueChangeToSection fCs true commentProblem)
      pure (Just a)

    ChangeToNode heading path mTitle a -> do
      -- Change the editor to a raw string outside the TOCEntry structure.
      H.modify_ _
        { mTocEntry = Nothing
        , mNodePath = Just path
        , mTitle = mTitle
        , isLoading = true
        }
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        -- Set the content of the editor
        H.liftEffect $ do
          session <- Editor.getSession ed
          document <- Session.getDocument session
          Document.setValue heading document
          Editor.setReadOnly false ed

          -- Reset Undo history
          undoMgr <- Session.getUndoManager session
          UndoMgr.reset undoMgr

          -- Clear annotations
          Session.clearAnnotations session

      -- reset Ref, because loading new content is considered
      -- changing the existing content, which would set the flag
      for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write false r
      H.modify_ _ { isLoading = false }
      handleAction $ SetManualSavedFlag false
      pure (Just a)

    UpdateNodePosition path a -> do
      H.modify_ \s ->
        case s.mNodePath of
          Just _ -> s { mNodePath = Just path }
          Nothing -> s
      pure (Just a)

    ReceiveFullTitle mTitle a -> do
      H.modify_ _ { mTitle = mTitle }
      pure (Just a)

    SetDirtyFlag a -> do
      state <- H.get
      for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write true r
      pure (Just a)

    SaveSection a -> do
      handleAction $ Save true
      pure (Just a)

    -- Confirm the first comment was sent
    ConfirmComment newCommentSection a -> do
      state <- H.get
      case
        state.commentState.tmpLiveMarker,
        state.mEditor,
        state.mListener,
        newCommentSection.first
        of
        Just lm, Just ed, Just listener, Just first -> do
          start <- H.liftEffect $ Anchor.getPosition lm.startAnchor
          end <- H.liftEffect $ Anchor.getPosition lm.endAnchor
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ removeLiveMarker lm session
          handleAction $ DeleteAnnotation lm false false
          let
            newMarker =
              { id: newCommentSection.markerID
              , type: "info"
              , startRow: Types.getRow start
              , startCol: Types.getColumn start
              , endRow: Types.getRow end
              , endCol: Types.getColumn end
              , markerText: first.comment.author
              , mCommentSection: Just newCommentSection
              }
            newMarkers = snoc state.commentState.markers newMarker
            -- delete temp id from hash map
            newOldMarkerAnnoPos = delete (-360)
              state.commentState.annoMaps.oldMarkerAnnoPos
            -- add the real id instead
            newOldMarkerAnnoPos' = insert newMarker.id newMarker.startRow
              newOldMarkerAnnoPos
            newAnnoMaps =
              state.commentState.annoMaps
                { oldMarkerAnnoPos = newOldMarkerAnnoPos' }
          newLiveMarker <- H.liftEffect $ addAnchor newMarker session listener true
          let
            newLM = case newLiveMarker of
              Nothing -> failureLiveMarker
              Just lm' -> lm'
            newLiveMarkers = case newLiveMarker of
              Nothing -> state.commentState.liveMarkers
              Just lm' -> snoc state.commentState.liveMarkers lm'
            newCommentState =
              state.commentState
                { tmpLiveMarker = Nothing
                , annoMaps = newAnnoMaps
                , selectedLiveMarker = Just newLM
                , markers = newMarkers
                , liveMarkers = newLiveMarkers
                }
          H.modify_ _ { commentState = newCommentState }
          -- Save new created comment
          -- set dirty to true to be able to save
          for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write true r
          handleAction $ SetManualSavedFlag false
          handleAction $ Save true
        _, _, _, _ -> pure unit
      pure (Just a)

    -- Comes from CommentOverview
    SelectCommentSection markerID a -> do
      lms <- H.gets _.commentState.liveMarkers
      editor_ <- H.gets _.mEditor
      handleAction HideHandles
      case (find (\m -> m.annotedMarkerID == markerID) lms) of
        Nothing -> do
          selectedLiveMarker <- H.gets _.commentState.selectedLiveMarker
          case selectedLiveMarker, editor_ of
            Just lm, Just ed -> do
              session <- H.liftEffect $ Editor.getSession ed
              H.liftEffect $ setMarkerSelectedClass session lm false
            _, _ -> pure unit
          H.modify_ \st -> st
            { commentState = st.commentState { selectedLiveMarker = Nothing } }
        Just lm -> do
          H.modify_ \st -> st
            { commentState = st.commentState { selectedLiveMarker = Just lm } }
          -- show comment drag handles
          case editor_ of
            Nothing -> pure unit
            Just ed -> H.liftEffect $ highlightSelection ed lms lm
          handleAction (ShowHandles lm)
      pure (Just a)

    UnselectCommentSection a -> do
      handleAction HideHandles
      editor_ <- H.gets _.mEditor
      selectedLiveMarker <- H.gets _.commentState.selectedLiveMarker
      case selectedLiveMarker, editor_ of
        Just lm, Just ed -> do
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ setMarkerSelectedClass session lm false
        _, _ -> pure unit
      H.modify_ \st -> st
        { commentState = st.commentState
            { selectedLiveMarker = Nothing, reAnchor = Nothing }
        }
      pure (Just a)

    ToDeleteComment markerID commentProblem a -> do
      handleAction $ DeleteComment markerID commentProblem
      pure (Just a)

    RequestDirtyVersion reply -> do
      isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
        _.mDirtyVersion
      pure (Just (reply isDirty))

    ResetDirtyVersion a -> do
      state <- H.get
      for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write false r
      pure $ Just a

    IsOnMerge reply -> do
      isOnMerge <- H.gets _.isOnMerge
      pure $ Just $ reply isOnMerge

    PreventChangeSection a -> do
      H.modify_ _ { discardPopup = true }
      pure $ Just a

    UpdateCommentProblem commentProblem a -> do
      H.modify_ \st -> st
        { commentState = st.commentState { commentProblem = commentProblem } }
      pure (Just a)

    SetReAnchor reAnchor a -> do
      H.modify_ \st -> st { commentState = st.commentState { reAnchor = reAnchor } }
      pure (Just a)

    UpdateEditorSize width a -> do
      handleAction $ HandleResize width
      pure (Just a)

    -- Only used to send the draft from the main editor (primary/left) to the secondary editor
    SetContent draft a -> do
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        handleAction Resize
        H.liftEffect do
          session <- Editor.getSession ed
          document <- Session.getDocument session
          Document.setValue draft document
          Editor.setReadOnly true ed
          -- Reset dirty flag: content is loaded from a draft and should be treated as the current saved state
          for_ state.saveState.mDirtyRef \r -> Ref.write false r
          -- Reset Undo history
          undoMgr <- Session.getUndoManager session
          UndoMgr.reset undoMgr
      H.modify_ _ { isLoading = false }
      pure (Just a)

  -- free up the save flags for the next save session
  -- check if there are new requests for saving during saving
  freeSaveFlagsAndMaybeRerun
    :: forall slots
     . H.HalogenM State Action slots Output m Unit
  freeSaveFlagsAndMaybeRerun = do
    -- free up the save flags for the next save session
    qSaving <- maybe (pure false) (H.liftEffect <<< Ref.read)
      =<< H.gets _.saveState.mQueuedSave
    isManualSaved <- H.gets _.saveState.isManualSaved
    state <- H.get
    -- check if there are new requests for saving during saving
    for_ state.saveState.mIsSaving \r -> H.liftEffect $ Ref.write false r
    when qSaving do
      for_ state.saveState.mQueuedSave \r -> H.liftEffect $ Ref.write false r
      handleAction $ Save (not isManualSaved)

  setIsOnMerge
    :: forall slots
     . Boolean
    -> H.HalogenM State Action slots Output m Unit
  setIsOnMerge flag = do
    H.modify_ _ { isOnMerge = flag }
    mRef <- H.gets _.saveState.mIsOnMergeRef
    for_ mRef \r -> H.liftEffect $ Ref.write flag r

  -- | Configure a newly created Ace editor instance.
  --   Sets the font size, enables line wrapping, hides the print margin,
  --   applies the default theme and mode, and marks the editor as read-only
  --   until a document entry is explicitly selected elsewhere.
  --
  --   * First argument: the Ace editor instance to configure.
  --   * Second argument: desired font size in pixels (without the \"px\" suffix).
  setupAce :: Types.Editor -> Int -> Effect Unit
  setupAce editor_ fontSize = do
    Editor.setFontSize (show fontSize <> "px") editor_
    session <- Editor.getSession editor_

    -- Set line break
    Editor.resize (Just true) editor_
    Session.setUseWrapMode true session

    -- remove the gray margin line
    Editor.setShowPrintMargin false editor_

    -- Set the editor's theme and mode
    Editor.setTheme "ace/theme/github" editor_
    Session.setMode "ace/mode/custom_mode" session

    -- set read only at the start to prevent users to write in not selected entry
    Editor.setReadOnly true editor_

-- | Change listener for the editor.
--
--   This function should implement stuff like parsing and syntax analysis,
--   linting, code completion, etc.
--   For now, it puts "  " in front of "#", if it is placed at the
--   beginning of a line
--  Update: it also detects, when content is changed and set dirtyRef flag
addChangeListenerWithRef
  :: Types.Editor
  -> Ref Boolean
  -> Ref Boolean
  -> HS.Listener Action
  -> Effect Unit
addChangeListenerWithRef editor_ dirtyRef versionRef listener = do
  session <- Editor.getSession editor_
  -- in order to prevent an ifinite loop with this listener
  guardRef <- Ref.new false
  Session.onChange session \(Types.DocumentEvent { action, start, end: _, lines }) ->
    do
      -- set dirty flag
      Ref.write true dirtyRef
      Ref.write true versionRef
      HS.notify listener $ SetManualSavedFlag false
      HS.notify listener AutoSaveTimer

      -- '#' → '  #' at beginning of a line with Reentrancy-Guard
      let isInsert = (unsafeCoerce action :: String) == "insert"
      when isInsert do
        let sCol = Types.getColumn start
        when (sCol == 0 && lines == [ "#" ]) do
          busy <- Ref.read guardRef
          when (not busy) do
            Ref.write true guardRef
            let sRow = Types.getRow start
            range <- Range.create sRow sCol sRow (sCol)
            Session.replace range "  " session
            Ref.write false guardRef

resolveDeleteLiveMarker
  :: Int
  -> CommentState
  -> Effect (Maybe LiveMarker)
resolveDeleteLiveMarker markerID commentState = do
  let
    isTargetById lm = lm.annotedMarkerID == markerID

    keepIf p x = if p x then Just x else Nothing

    mLmById =
      (commentState.tmpLiveMarker >>= keepIf isTargetById)
        <|> (commentState.selectedLiveMarker >>= keepIf isTargetById)
        <|> find isTargetById commentState.liveMarkers

    mTargetMarker = find (\m -> m.id == markerID) commentState.markers

    candidateLms =
      catMaybes
        [ commentState.tmpLiveMarker
        , commentState.selectedLiveMarker
        ] <> commentState.liveMarkers

    oneItem xs =
      case uncons xs of
        Nothing -> Nothing
        Just { head: x, tail: rest } ->
          case uncons rest of
            Nothing -> Just x
            Just _ -> Nothing

    findUnique target matchText lms =
      let
        matches lm = do
          start <- Anchor.getPosition lm.startAnchor
          end <- Anchor.getPosition lm.endAnchor
          let
            sameRange =
              Types.getRow start == target.startRow
                && Types.getColumn start == target.startCol
                && Types.getRow end == target.endRow
                &&
                  Types.getColumn end == target.endCol
            ok =
              if matchText then
                sameRange && lm.markerText == target.markerText
              else
                sameRange
          pure $ if ok then Just lm else Nothing
      in
        oneItem <$> (catMaybes <$> for lms matches)

  case mLmById, mTargetMarker of
    Just lm, _ -> pure (Just lm)
    Nothing, Just target -> do
      mByText <- findUnique target true candidateLms
      if isJust mByText then
        pure mByText
      else
        findUnique target false candidateLms
    Nothing, Nothing -> pure Nothing

addAnchor
  :: AnnotatedMarker
  -> Types.EditSession
  -> HS.Listener Action
  -> Boolean
  -> Effect (Maybe LiveMarker)
addAnchor marker session listener action =
  if (marker.startRow == marker.endRow && marker.startCol == marker.endCol) then
    pure Nothing -- No valid range, so no marker
  else do
    document <- Session.getDocument session
    startAnchor <- Document.createAnchor marker.startRow marker.startCol document
    endAnchor <- Document.createAnchor marker.endRow marker.endCol document
    Anchor.setInsertRight true endAnchor

    range <- createMarkerRange marker
    id <- Session.addMarker range "my-marker" "text" false session
    markerRef <- Ref.new id
    deletedRef <- Ref.new false

    let
      lm =
        { annotedMarkerID: marker.id
        , startAnchor: startAnchor
        , endAnchor: endAnchor
        , markerText: marker.markerText
        , ref: markerRef
        }

      rerenderMarker
        :: { old :: Types.Position, value :: Types.Position }
        -> Effect Unit
      rerenderMarker _ = do
        wasDeleted <- Ref.read deletedRef
        when (not wasDeleted) do
          Ref.read markerRef >>= flip Session.removeMarker session
          Types.Position { row: startRow, column: startColumn } <- Anchor.getPosition
            startAnchor
          Types.Position { row: endRow, column: endColumn } <- Anchor.getPosition
            endAnchor
          -- TODO: check if anchors are the same => anchors got deleted by user
          if startRow > endRow || (startRow == endRow && startColumn >= endColumn) then
            do
              Ref.write true deletedRef
              HS.notify listener (DeleteComment marker.id true)
              HS.notify listener (NotifyCommentProblem marker.id)
              pure unit
          else do
            markRange <- Range.create
              startRow
              startColumn
              endRow
              endColumn
            newId <- Session.addMarker
              markRange
              "my-marker"
              "text"
              false
              session

            Ref.write newId markerRef
            HS.notify listener (UpdateAnnotation lm)
            pure unit

    Anchor.onChange startAnchor rerenderMarker
    Anchor.onChange endAnchor rerenderMarker
    when action $
      HS.notify listener (AddAnnotation lm true)
    pure (Just lm)

buttonDivisor :: forall m. H.ComponentHTML Action () m
buttonDivisor = HH.div
  [ HP.classes [ HB.vr, HB.mx1 ] ]
  []

initialCommentState :: CommentState
initialCommentState =
  { annoMaps: emptyAnnotationMaps
  , dragState: Nothing
  , startHandleMarkerId: Nothing
  , endHandleMarkerId: Nothing
  , mPrevHandler: Nothing
  , dragRowAS: -1
  , dragColAS: -1
  , mHandleBorder: Nothing
  , tmpLiveMarker: Nothing
  , selectedLiveMarker: Nothing
  , markers: []
  , liveMarkers: []
  , commentProblem: false
  , reAnchor: Nothing
  }

initialSaveState :: SaveState
initialSaveState =
  { mDirtyRef: Nothing
  , mIsOnMergeRef: Nothing
  , mIsSaving: Nothing
  , mQueuedSave: Nothing
  , mBeforeUnloadListener: Nothing
  , isManualSaved: true
  , showSavedIcon: false
  , mSavedIconF: Nothing
  , mPendingDebounceF: Nothing
  , mPendingMaxWaitF: Nothing
  }

initialState :: Connected FPOTranslator Input -> State
initialState { context, input } =
  { docID: input.docID
  , translator: fromFpoTranslator context
  , mEditor: Nothing
  , mTocEntry: Nothing
  , currentVersion: ""
  , mNodePath: Nothing
  , mTitle: Nothing
  , mContent: Nothing
  , html: ""
  , commentState: initialCommentState
  , fontSize: 12
  , mListener: Nothing
  , showButtonText: true
  , showButtons: true
  , wrapEnabled: false
  , saveState: initialSaveState
  , compareToElement: input.elementData
  , isEditorOutdated: false
  , outdatedInfoPopup: false
  , discardPopup: false
  , mDirtyVersion: Nothing
  , isOnMerge: false
  , wasLoadedInMergeMode: false
  , upToDateVersion: Nothing
  , isLoading: true
  }

makeEditorToolbarButton
  :: forall m
   . Boolean
  -> String
  -> Array H.ClassName
  -> Action
  -> String
  -> H.ComponentHTML Action () m
makeEditorToolbarButton enabled tooltip btnClasses action biName =
  let
    toolbarDefaultClasses =
      if null btnClasses then [ H.ClassName "editor-toolbar-btn--default" ] else []
    baseClasses =
      [ HB.btn
      , HB.p0
      , HB.m0
      , HB.border0
      , H.ClassName "editor-toolbar-btn"
      , H.ClassName "editor-toolbar-btn--icon"
      ]
  in
    HH.button
      [ HP.classes
          ( prependIf (not enabled) HB.opacity25
              ( baseClasses <> toolbarDefaultClasses <> btnClasses )
          )
      , HP.title tooltip
      , HE.onClick \_ -> action
      , HP.enabled enabled
      ]
      [ HH.i
          [ HP.classes [ HB.bi, H.ClassName biName ]
          ]
          []
      ]

-- Here, no tooltip is needed as the text is shown in the button
makeEditorToolbarButtonWithText
  :: forall m
   . Boolean
  -> Boolean
  -> Array H.ClassName
  -> Action
  -> String
  -> String
  -> H.ComponentHTML Action () m
makeEditorToolbarButtonWithText enabled asText btnClasses action biName smallText =
  let
    toolbarDefaultClasses =
      if null btnClasses then [ H.ClassName "editor-toolbar-btn--default" ] else []
    baseClasses =
      [ HB.btn
      , HB.px1
      , HB.py0
      , HB.m0
      , HB.border0
      , H.ClassName "editor-toolbar-btn"
      , H.ClassName "editor-toolbar-btn--text"
      ]
  in
    HH.button
      ( prependIf (not asText) (HP.title smallText)
          [ HP.classes
              ( prependIf (not enabled) HB.opacity25
                  ( baseClasses <> toolbarDefaultClasses <> btnClasses )
              )
          , HP.style "white-space: nowrap;"
          , HE.onClick \_ -> action
          , HP.enabled enabled
          ]
      )
      ( prependIf asText
          (HH.small [ HP.style "margin-right: 0.25rem;" ] [ HH.text smallText ])
          [ HH.i
              [ HP.classes [ HB.bi, H.ClassName biName ]
              ]
              []
          ]
      )

addMouseDragListeners :: HTMLElement -> HS.Listener Action -> Effect Unit
addMouseDragListeners container listener = do
  downL <- eventListener \ev -> do
    case ME.fromEvent ev of
      Just mev -> do
        let
          x = toNumber (ME.clientX mev)
          y = toNumber (ME.clientY mev)
        -- try to drag comment section dragger marker
        HS.notify listener (TryStartDrag x y)
      Nothing ->
        pure unit
  addEventListener (EventType "mousedown") downL true
    (toEventTarget $ toElement container)

  moveL <- H.liftEffect $ eventListener \ev -> do
    case ME.fromEvent ev of
      Just mev -> do
        let
          x = toNumber (ME.clientX mev)
          y = toNumber (ME.clientY mev)
        HS.notify listener (DragMove x y)
      Nothing -> pure unit
  addEventListener (EventType "mousemove") moveL true
    (toEventTarget $ toElement container)

  upL <- H.liftEffect $ eventListener \_ -> do
    -- find potentially selected Comment
    HS.notify listener SelectComment
    -- stop dragging the comment dragger
    HS.notify listener EndDrag
  addEventListener (EventType "mouseup") upL true
    (toEventTarget $ toElement container)

addBeforeUnloadListener
  :: forall slots m
   . MonadAff m
  => Ref Boolean
  -> Ref Boolean
  -> HS.Listener Action
  -> H.HalogenM State Action slots Output m Unit
addBeforeUnloadListener dirtyRef isOnMergeRef listener = do
  win <- H.liftEffect window
  let
    winTarget = Win.toEventTarget win
    -- creating EventTypes
    beforeunload = EventType "beforeunload"

  beforeUnloadListener <- H.liftEffect $ eventListener \ev -> do
    isDirty <- traverse Ref.read (Just dirtyRef)
    isOnMerge <- traverse Ref.read (Just isOnMergeRef)
    case isOnMerge, isDirty of
      -- Prevent the tab from closing in a certain way
      Just true, _ -> do
        preventDefault ev
      _, Just true -> do
        preventDefault ev
        HS.notify listener (Save true)
      _, _ -> pure unit

  sref <- H.liftEffect $ Ref.new false
  qref <- H.liftEffect $ Ref.new false
  H.modify_ \st -> st
    { saveState = st.saveState
        { mDirtyRef = Just dirtyRef
        , mIsOnMergeRef = Just isOnMergeRef
        , mIsSaving = Just sref
        , mQueuedSave = Just qref
        , mBeforeUnloadListener = Just beforeUnloadListener
        }
    }
  H.liftEffect $ addEventListener beforeunload beforeUnloadListener false
    winTarget
