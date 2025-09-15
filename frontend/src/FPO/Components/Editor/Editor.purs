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

{- import Data.Argonaut.Core (jsonEmptyObject) -}
import Ace (ace, editNode) as Ace
import Ace.Anchor as Anchor
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types as Types
import Ace.UndoManager as UndoMgr
import Data.Array (catMaybes, deleteBy, intercalate, snoc)
import Data.Either (Either(..))
import Data.Foldable (find, for_, traverse_)
import Data.HashMap (HashMap, delete, empty, insert, lookup, size)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class as EC
import Effect.Console (log)
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
  ( DragHandle(..)
  , ElementData
  , HandleBorder
  , HistoryOp(..)
  , LiveMarker
  , Path
  , RenderKind(..)
  , createMarkerRange
  , cursorInRange
  , failureLiveMarker
  , hideHandlesFrom
  , highlightSelection
  , near
  , removeLiveMarker
  , setAnnotations
  , setMarkerSelectedClass
  , showHandlesFor
  , updateMarkers
  )
import FPO.Components.Modals.DiscardModal (discardModal)
import FPO.Components.Modals.InfoModal (infoModal)
import FPO.Components.TOC (Version)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.ContentDto 
  (Content(..)
  , ContentWrapper
  , convertDCWToCW
  , getContentParent
  , getWrapperContent
  , setContentParent
  , setWrapperContent)
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
  , emptyTOCEntry
  )
import FPO.Util (prependIf)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, enabled, ref, style, title) as HP
import Halogen.Query.HalogenM (SubscriptionId)
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
import Web.HTML (window)
import Web.HTML.HTMLElement (offsetWidth, toElement)
import Web.HTML.Window as Win
import Web.ResizeObserver as RO
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.UIEvent.MouseEvent as ME

foreign import _resize :: Types.Editor -> Effect Unit

type CommentState =
  {
    -- Hashmaps for Annotations
    -- Row line -> Hashmap of Username -> how many times the use has comments in the line
    markerAnnoHS :: HashMap Int (HashMap String Int)
  -- markerID -> old row position in Annotation
  , oldMarkerAnnoPos :: HashMap Int Int
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
  }

type SaveState =
  {
    -- for saving when closing window
    mDirtyRef :: Maybe (Ref Boolean)
  , mBeforeUnloadL :: Maybe EventListener
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
  , mContent :: Maybe Content
  -- comments
  , commentState :: CommentState
  , fontSize :: Int
  , mListener :: Maybe (HS.Listener Action)
  , resizeObserver :: Maybe RO.ResizeObserver
  , resizeSubscription :: Maybe SubscriptionId
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
  -- similar to mDirtyRef, but for Drafts. causes popup is user tries changing version with open draft, as that would discard the draft.
  , mDirtyVersion :: Maybe (Ref Boolean)
  -- Determines whether the user is on the merge view
  , isOnMerge :: Boolean
  -- obtained from TOC. Used when merging. Set to the version details of the Version loaded into the right editor.
  , upToDateVersion :: Maybe Version
  )

type Input = { docID :: DocumentID, elementData :: ElementData }

data Output
  = AddComment Int Int
  | ClickedQuery (Array String)
  | DeletedComment TOCEntry (Array Int)
  | PostPDF String
  | RenamedNode String Path
  | RequestComments Int Int
  | SelectedCommentSection Int Int
  | ShowAllCommentsOutput Int Int
  | RaiseDiscard
  | RaiseMergeMode
  | Merged

data Action
  = Init
  | Comment
  | ChangeToSection TOCEntry (Maybe Int)
  | ContinueChangeToSection (Array FirstComment)
  | SelectComment
  | Font (Types.Editor -> Effect Unit)
  | FontSize (Int -> Int)
  | History HistoryOp
  | Save Boolean
  -- Subsection of Save
  | Upload TOCEntry ContentWrapper Boolean
  | SavedIcon
  -- new change in editor -> reset timer
  | AutoSaveTimer
  -- called by AutoSaveTimer subscription
  | AutoSave
  | TryStartDrag Number Number -- clientX, clientY
  | StartDrag DragHandle LiveMarker Number Number -- mouse down: which, lm, clientX, clientY
  | DragMove Number Number -- mouse move: clientX, clientY
  | EndDrag -- mouse up
  | ShowHandles LiveMarker -- set Handles
  | HideHandles -- remove Handles
  | AddAnnotation LiveMarker Boolean
  | DeleteAnnotation LiveMarker Boolean Boolean
  | UpdateAnnotation LiveMarker
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
  = EditorResize a
  -- | save the current content and send it to splitview
  | SaveSection a
  -- | receive the selected TOC and put its content into the editor
  | ChangeSection TOCEntry (Maybe Int) a
  | ContinueChangeSection (Array FirstComment) a
  -- | Update the position of a node in the editor, if existing.
  | UpdateNodePosition Path a
  | UpdateComment CommentSection a
  | SelectCommentSection Int a
  | ToDeleteComment a
  | RequestDirtyVersion (Boolean -> a)
  | ResetDirtyVersion a
  | ReceiveUpToDateUpdate (Maybe Version) a

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
        ]
    renderDiscardModal = case state.discardPopup of
      false -> []
      true ->
        [ discardModal
            state.translator
            CancelDiscardAction
            ConfirmDiscardAction

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
                [ case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_textBold") state.translator)
                        (Font makeBold)
                        "bi-type-bold"
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_textItalic") state.translator)
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
                        (Font underscore)
                        "bi-type-underline"

                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      buttonDivisor
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_fontSizeUp") state.translator)
                    (FontSize (\x -> x + 2))
                    "bi-plus-square"
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_fontSizeDown") state.translator)
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
                        (History HUndo)
                        "bi-arrow-counterclockwise"
                , case state.compareToElement of
                    Just _ -> HH.text ""
                    Nothing ->
                      makeEditorToolbarButton
                        true
                        (translate (label :: _ "editor_redo") state.translator)
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
                        fullFeatures
                        (translate (label :: _ "editor_comment") state.translator)
                        Comment
                        "bi-chat-square-text"

                ]
            , case state.compareToElement of
                Just _ -> HH.text ""
                Nothing ->
                  HH.div
                    [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ]
                    , HP.style "min-width: 0;"
                    ]
                    [ makeEditorToolbarButtonWithText
                        true
                        state.showButtonText
                        (Save false)
                        "bi-floppy"
                        case state.isOnMerge of 
                          false ->
                            (translate (label :: _ "editor_save") state.translator)
                          true ->
                            (translate (label :: _ "editor_merge") state.translator)
                    , makeEditorToolbarButtonWithText
                        true
                        state.showButtonText
                        (Render RenderHTML)
                        "bi-file-richtext"
                        (translate (label :: _ "editor_preview") state.translator)
                    , makeEditorToolbarButtonWithText
                        true
                        state.showButtonText
                        (Render RenderPDF)
                        "bi-filetype-pdf"
                        (translate (label :: _ "editor_pdf") state.translator)
                    , makeEditorToolbarButtonWithText
                        fullFeatures
                        state.showButtonText
                        ShowAllComments
                        "bi-chat-square"
                        (translate (label :: _ "editor_allComments") state.translator)
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
                          ToggleOutdatedInfoPopup
                          "bi bi-info-circle"
                      , makeEditorToolbarButtonWithText
                          true
                          state.showButtonText
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
          , HP.classes [ HB.flexGrow1 ]
          , HP.style "min-height: 0; flex-grow: 1; flex-basis: 0"
          ]
          [ -- Add overlay when right side
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
      -- create subscription for later use
      state <- H.get
      { emitter, listener } <- H.liftEffect HS.create
      -- Subscribe to resize events and store subscription for cleanup
      subscription <- H.subscribe emitter
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
        editor_ <- H.liftEffect $ Ace.editNode el Ace.ace

        H.modify_ _
          { mEditor = Just editor_
          , mListener = Just listener
          , resizeSubscription = Just subscription
          }
        fontSize <- H.gets _.fontSize

        H.liftEffect $ do
          Editor.setFontSize (show fontSize <> "px") editor_
          eventListen <- eventListener (keyBinding editor_)
          container <- Editor.getContainer editor_
          addEventListener keydown eventListen true
            (toEventTarget $ toElement container)
          session <- Editor.getSession editor_

          -- Set line break
          Editor.resize (Just true) editor_
          Session.setUseWrapMode true session

          -- remove the gray margin line
          Editor.setShowPrintMargin false editor_

          -- Set the editor's theme and mode
          Editor.setTheme "ace/theme/github" editor_
          Session.setMode "ace/mode/custom_mode" session
          Editor.setEnableLiveAutocompletion true editor_
          case state.compareToElement of
            Just _ -> do
              Editor.setReadOnly true editor_
            Nothing ->
              Editor.setReadOnly false editor_

      -- New Ref for keeping track, if the content in editor has changed
      -- 1. since last save
      -- 2. since opening version
      dref <- H.liftEffect $ Ref.new false
      vref <- H.liftEffect $ Ref.new false

      win <- H.liftEffect window
      let
        winTarget = Win.toEventTarget win
        -- creating EventTypes
        beforeunload = EventType "beforeunload"

      -- create eventListener for preventing the tab from closing
      -- when content has not been saved (Not changing through Navbar)
      buL <- H.liftEffect $ eventListener \ev -> do
        readRef <- traverse Ref.read (Just dref)
        case readRef of
          -- Prevent the tab from closing in a certain way
          Just true -> do
            preventDefault ev
            HS.notify listener (Save true)
          _ -> pure unit
      H.modify_ \st -> st
        { saveState = st.saveState
            { mDirtyRef = Just dref
            , mBeforeUnloadL = Just buL
            }
        , mDirtyVersion = Just vref
        }
      H.liftEffect $ addEventListener beforeunload buL false winTarget

      -- Setup ResizeObserver for the container element
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \element -> do

        let
          callback _ _ = do
            -- Get the current width directly from the element
            width <- offsetWidth element
            HS.notify listener (HandleResize width)

        observer <- H.liftEffect $ RO.resizeObserver callback
        H.liftEffect $ RO.observe (toElement element) {} observer
        H.modify_ _ { resizeObserver = Just observer }
      compareTo <- H.gets _.compareToElement
      case compareTo of
        Nothing
        -> pure unit
        Just { tocEntry: tocEntry, revID: revID }
        -> handleAction (ChangeToSection tocEntry revID)

      -- add and start Editor listeners
      H.gets _.mEditor >>= traverse_ \ed -> do

        -- change Editor content listener
        H.liftEffect $ addChangeListenerWithRef ed dref vref listener
        container <- H.liftEffect $ Editor.getContainer ed

        -- Mouse events

        downL <- H.liftEffect $ eventListener \ev -> do
          case ME.fromEvent ev of
            Just mev -> do
              let
                x = toNumber (ME.clientX mev)
                y = toNumber (ME.clientY mev)
              -- try to drag comment section dragger marker
              HS.notify listener (TryStartDrag x y)
            Nothing ->
              pure unit
        H.liftEffect $ addEventListener (EventType "mousedown") downL true
          (toEventTarget $ toElement container)

        moveL <- H.liftEffect $ eventListener \ev -> do
          case ME.fromEvent ev of
            Just mev -> do
              let
                x = toNumber (ME.clientX mev)
                y = toNumber (ME.clientY mev)
              HS.notify listener (DragMove x y)
            Nothing -> pure unit
        H.liftEffect $ addEventListener (EventType "mousemove") moveL true
          (toEventTarget $ toElement container)

        upL <- H.liftEffect $ eventListener \_ -> do
          -- find potentially selected Comment
          HS.notify listener SelectComment
          -- stop dragging the comment dragger
          HS.notify listener EndDrag
        H.liftEffect $ addEventListener (EventType "mouseup") upL true
          (toEventTarget $ toElement container)

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
        H.liftEffect $ log $ show case state.mContent of 
                              Just (Content mC) -> mC.parent
                              _ -> 0
        let newSize = change state.fontSize
        H.modify_ \st -> st { fontSize = newSize }
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

    Discard ->
      H.modify_ _ { discardPopup = true }

    Render renderType -> do
      allLines <- H.gets _.mEditor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines
      case renderType of
        RenderHTML -> H.raise (ClickedQuery $ fromMaybe [] allLines)
        RenderPDF -> do
          let
            content = case allLines of
              Nothing -> ""
              Just ls -> intercalate "\n" ls
          H.raise (PostPDF content)

    ShowAllComments -> do
      state <- H.get
      let tocID = maybe (-1) _.id state.mTocEntry
      H.raise $ ShowAllCommentsOutput state.docID tocID

    -- Save section

    Save isAutoSave -> do
      state <- H.get
      when (state.compareToElement == Nothing) $ do
        isDirty <- EC.liftEffect $ Ref.read =<< case state.saveState.mDirtyRef of
          Just r -> pure r
          Nothing -> EC.liftEffect $ Ref.new false
        -- Only save, when dirty flag is true or we are in older version
        -- TODO: Add another flag instead of using isEditorOutdated
        when (isDirty || state.isEditorOutdated) $ do
          allLines <- H.gets _.mEditor >>= traverse \ed -> do
            H.liftEffect $ Editor.getSession ed
              >>= Session.getDocument
              >>= Document.getAllLines

          let
            contentLines = intercalate "\n" (fromMaybe [] allLines)

          case state.mTocEntry of
            Nothing -> pure unit
            Just entry ->
              case state.mContent of
                Nothing -> pure unit
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

    Upload newEntry newWrapper isAutoSave -> do
      state <- H.get
      H.liftEffect $ log $ case (state.upToDateVersion) of 
                            Nothing -> "empty"
                            Just version -> show version.identifier
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
                  Just i -> (setWrapperContent (setContentParent i (getWrapperContent newWrapper)) newWrapper)
            false -> newWrapper
        jsonContent = ContentDto.encodeWrapper modifiedWrapper
        newContent = ContentDto.getWrapperContent modifiedWrapper
      -- send the new content as POST to the server
      H.liftEffect $ log ("wrappedID" <> show (getContentParent (getWrapperContent modifiedWrapper)))
      H.liftEffect $ log $ show state.isOnMerge
      response <- Request.postJson (ContentDto.extractDraft newContent)
        ( "/docs/" <> show state.docID <> "/text/" <> show newEntry.id
            <> "/rev?isAutoSave="
            <> show isAutoSave
        )
        jsonContent
      -- handle errors in pos and decodeJson
      case response of
        -- if error, try to Save again (Maybe ParentID is lost?)
        Left err -> updateStore $ Store.AddError $ err
        -- extract and insert new parentID into newContent
        Right { content: updatedContent, typ: typ } -> do

          H.modify_ _ { mContent = Just updatedContent }

          -- Show saved icon or toast
          case isAutoSave, state.isEditorOutdated of
            -- auto save interaction
            true, _ -> do
              -- H.modify_ _ { mContent = Just updatedContent }
              handleAction SavedIcon
              case typ of
                "noConflict" -> pure unit
                "draftCreated" -> pure unit --raise something to update version
                "conflict" -> pure unit --should not happen here also raise something just in case
                _ -> H.liftEffect $ log "case 1"
            -- manuell saving and working in latest version
            false, false -> do
              -- H.modify_ _ { mContent = Just updatedContent }
              updateStore $ Store.AddSuccess "Saved successfully"
              case typ of
                "noConflict" -> do
                  H.modify_ _ {isOnMerge = false}
                  case state.isOnMerge of
                    false -> pure unit
                    true -> H.raise Merged
                  H.liftEffect $ log "case 2 1"
                "draftCreated" -> do --should not happen here. just copy autosave case in case
                  H.modify_ _ {isOnMerge = true}
                  H.liftEffect $ log "case 2 2"
                "conflict" -> do --raise something to update version
                  H.modify_ _ {isOnMerge = true}
                  H.liftEffect $ log "case 2 3"
                  H.raise RaiseMergeMode
                _ -> H.liftEffect $ log "case 2"
            -- manuell saving, draft mode => publish
            false, true -> do
              case typ of
                --happends if parent was updated due to merge view being present.
                "noConflict" -> do
                  H.modify_ _ {isOnMerge = false}
                  case state.isOnMerge of
                    false -> pure unit
                    true -> H.raise Merged
                  H.liftEffect $ log "case 3 1"
                "draftCreated" -> do --should not happen here. just copy autosave case in case
                  H.modify_ _ {isOnMerge = true}
                  H.liftEffect $ log "case 3 2"
                "conflict" -> do --raise something to update version
                  H.modify_ _ {isOnMerge = true}
                  H.raise RaiseMergeMode
                  H.liftEffect $ log "case 3 3"
                _ -> H.liftEffect $ log "case 3"
{-               res <- Request.postJson (ContentDto.extractDraft updatedContent)
                ( "/docs/" <> show state.docID <> "/text/" <> show newEntry.id
                    <> "/draft/publish"
                )
                jsonEmptyObject
              
              case res of
                Left err' -> updateStore $ Store.AddError $ err'
                Right upCon -> do
                  H.modify_ _ { mContent = Just upCon }
                  -- Put merged content into editor
                  H.gets _.mEditor >>= traverse_ \ed -> do
                    H.liftEffect do
                      session <- Editor.getSession ed
                      document <- Session.getDocument session
                      Document.setValue (ContentDto.getContentText upCon) document
                      -- reset Ref, because loading new content is considered 
                      -- changing the existing content, which would set the flag
                      for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write false r
                  updateStore $ Store.AddSuccess "Saved and Merged successfully."
                  pure unit -}

          -- mDirtyRef := false
          for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write false r
          pure unit

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
      saveState <- H.gets _.saveState
      -- restart 2 sec timer after every new input
      -- first kill the maybe running fiber (kinda like a thread)
      for_ saveState.mPendingDebounceF H.kill

      -- start a new fiber
      dFib <- H.fork do
        H.liftAff $ delay (Milliseconds 2000.0)
        isDirty <- EC.liftEffect $ Ref.read =<< case saveState.mDirtyRef of
          Just r -> pure r
          Nothing -> EC.liftEffect $ Ref.new false
        when isDirty $ handleAction AutoSave
      H.modify_ _ { saveState = saveState { mPendingDebounceF = Just dFib } }

      -- This is a seperate 20 sec timer, which forces to save, in case of a long edit
      -- does not reset with new input
      case saveState.mPendingMaxWaitF of
        -- timer already running
        Just _ -> pure unit
        -- no timer there
        Nothing -> do
          mFib <- H.fork do
            H.liftAff $ delay (Milliseconds 20000.0)
            isDirty <- EC.liftEffect $ Ref.read =<< case saveState.mDirtyRef of
              Just r -> pure r
              Nothing -> EC.liftEffect $ Ref.new false
            when isDirty $ handleAction AutoSave
          H.modify_ _ { saveState = saveState { mPendingMaxWaitF = Just mFib } }

    AutoSave -> do
      -- only save, if dirty
      isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
        _.saveState.mDirtyRef
      when isDirty do
        handleAction $ Save true
        -- after Save: dirty false + stop timer
        mRef <- H.gets _.saveState.mDirtyRef
        for_ mRef \r -> H.liftEffect $ Ref.write false r
        saveState <- H.gets _.saveState
        for_ saveState.mPendingDebounceF H.kill
        for_ saveState.mPendingMaxWaitF H.kill
        H.modify_ _
          { saveState = saveState
              { mPendingDebounceF = Nothing
              , mPendingMaxWaitF = Nothing
              }
          }

    Comment -> do
      state <- H.get
      userWithError <- getUser
      case userWithError, state.mListener of
        Right user, Just listener -> do
          H.gets _.mEditor >>= traverse_ \ed -> do
            session <- H.liftEffect $ Editor.getSession ed
            range <- H.liftEffect $ Editor.getSelectionRange ed
            start <- H.liftEffect $ Range.getStart range
            end <- H.liftEffect $ Range.getEnd range

            -- delete temporary live marker, as the first comment has not been sent
            case state.commentState.tmpLiveMarker of
              Just lm -> do
                H.liftEffect $ removeLiveMarker lm session
                handleAction $ DeleteAnnotation lm false false
              Nothing -> pure unit

            let
              sRow = Types.getRow start
              sCol = Types.getColumn start
              eRow = Types.getRow end
              eCol = Types.getColumn end
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

            mLiveMarker <- H.liftEffect $ addAnchor newMarker session listener true

            case state.mTocEntry of
              Just entry -> do
                H.modify_ \st -> st
                  { commentState = st.commentState
                      { tmpLiveMarker = mLiveMarker
                      , selectedLiveMarker = mLiveMarker
                      }
                  }
                H.raise (AddComment state.docID entry.id)
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

        _, _ -> pure unit -- TODO error handling 

    SelectComment -> do
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
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
            -- set selection and highlight it
            H.modify_ \st -> st
              { commentState = st.commentState { selectedLiveMarker = Just lm } }
            H.liftEffect $ highlightSelection ed liveMarkers lm
            handleAction (ShowHandles lm)
        let
          lm = case foundLM of
            Nothing -> failureLiveMarker
            Just found -> found
          foundID = lm.annotedMarkerID

          -- need it only for its ID
          tocEntry = case state.mTocEntry of
            Nothing -> emptyTOCEntry
            Just e -> e
        H.modify_ \st -> st
          { commentState = st.commentState { selectedLiveMarker = Just lm } }
        when (foundID >= 0 || foundID == -360) $
          H.raise (SelectedCommentSection tocEntry.id foundID)

    -- Comment Section Dragger Actions

    -- Try to get mouse position and maybe selected handle
    TryStartDrag clientX clientY -> do
      mEditor <- H.gets _.mEditor
      selectedLiveMarker <- H.gets _.commentState.selectedLiveMarker
      case mEditor, selectedLiveMarker of
        Just ed, Just lm -> do
          -- Mauspos -> Textpos
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
            -- For CSS identification
            H.liftEffect do
              addClass container "fpo-no-select"
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
                    DragStart -> if c0 > column then column - 1 else c0
                    DragEnd -> if c0 < column then column + 1 else c0
                else c0

            -- set drag anchor
            case which of
              DragStart -> H.liftEffect $ setAnchorPosition lm.startAnchor row' col'
              DragEnd -> H.liftEffect $ setAnchorPosition lm.endAnchor row' col'

            -- draw new Handles (current position)
            session <- H.liftEffect $ Editor.getSession ed
            H.liftEffect $ hideHandlesFrom session commentState.startHandleMarkerId
              commentState.endHandleMarkerId
            ids <- H.liftEffect $ showHandlesFor session lm
            H.modify_ \st -> st
              { commentState = st.commentState
                  { startHandleMarkerId = ids.startId
                  , endHandleMarkerId = ids.endId
                  , dragRowAS = row'
                  , dragColAS = col'
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
              removeClass container "fpo-no-select"
              removeClass container "fpo-dragging"
              -- remove the selected text in editor
              clearSelection ed

            -- Auto save
            case state.commentState.mPrevHandler, state.saveState.mDirtyRef of
              Just prev, Just dref -> do
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
                    H.liftEffect $ Ref.write true dref
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
      pos <- H.liftEffect $ Anchor.getPosition lm.startAnchor
      let
        startRow = Types.getRow pos
        newOldMarkerAnnoPos = insert lm.annotedMarkerID startRow
          commentState.oldMarkerAnnoPos
        newMarkerAnnoHS = case lookup startRow commentState.markerAnnoHS of
          Nothing ->
            let
              newEntry = insert lm.markerText 1 empty
            in
              insert startRow newEntry commentState.markerAnnoHS
          Just entry ->
            let
              oldValue = fromMaybe 0 (lookup lm.markerText entry)
              newEntry = insert lm.markerText (oldValue + 1) entry
            in
              insert startRow newEntry commentState.markerAnnoHS
      H.modify_ \st -> st
        { commentState = st.commentState
            { markerAnnoHS = newMarkerAnnoHS
            , oldMarkerAnnoPos = newOldMarkerAnnoPos
            }
        }
      when setAnn do
        mEditor <- H.gets _.mEditor
        H.liftEffect $ setAnnotations newMarkerAnnoHS mEditor

    DeleteAnnotation lm reAdd setAnn -> do
      commentState <- H.gets _.commentState
      let
        -- get old row number for this marker
        oldRow = fromMaybe 0 (lookup lm.annotedMarkerID commentState.oldMarkerAnnoPos)
        -- update Annotation HashMap
        newMarkerAnnoHS = case lookup oldRow commentState.markerAnnoHS of
          -- should not happen
          Nothing -> commentState.markerAnnoHS
          -- update this HashMap entry
          Just entry -> do
            let
              -- if 1 then delete it, bigger just decreament the value
              oldValue = fromMaybe 0 (lookup lm.markerText entry)
              newEntry =
                if oldValue <= 1 then
                  delete lm.markerText entry
                else
                  insert lm.markerText (oldValue - 1) entry
            -- delete entry if empty
            if ((size newEntry) == 0) then
              delete oldRow commentState.markerAnnoHS
            else
              insert oldRow newEntry commentState.markerAnnoHS
      H.modify_ \st -> st
        { commentState = st.commentState { markerAnnoHS = newMarkerAnnoHS } }
      -- if we want to update the live marker annotation
      if reAdd then
        handleAction $ AddAnnotation lm setAnn
      else
        when setAnn do
          mEditor <- H.gets _.mEditor
          H.liftEffect $ setAnnotations newMarkerAnnoHS mEditor

    -- delete and readd Annotation if the startRow of live marker has changed
    UpdateAnnotation lm -> do
      oldMarkerAnnoPos <- H.gets _.commentState.oldMarkerAnnoPos
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

    HandleResize width -> do
      -- Decides whether to show button text based on the width.
      -- Because german labels are longer, we need to adjust the cutoff
      -- threshold dynamically. Pretty sure this is not the best solution,
      -- but it works.

      lang <- H.liftEffect $ Store.loadLanguage
      let cutoff = if lang == Just "de-DE" then 690.0 else 592.0
      let noButtonsCutoff = 350.0

      H.modify_ _
        { showButtonText = width >= cutoff, showButtons = width >= noButtonsCutoff }

    Finalize -> do
      state <- H.get
      win <- H.liftEffect window
      let
        tgt = Win.toEventTarget win
        beforeunload = EventType "beforeunload"
      -- Cleanup observer and subscription
      H.liftEffect $ case state.resizeObserver of
        Just obs -> RO.disconnect obs
        Nothing -> pure unit
      case state.resizeSubscription of
        Just subscription -> H.unsubscribe subscription
        Nothing -> pure unit
      case state.saveState.mBeforeUnloadL of
        Just l -> H.liftEffect $ removeEventListener beforeunload l false tgt
        _ -> pure unit
      for_ state.saveState.mPendingDebounceF H.kill
      for_ state.saveState.mPendingMaxWaitF H.kill

    ChangeToSection entry rev -> do
      state <- H.get
      let
        version = case rev of
          Nothing -> "latest"
          Just v -> show v
      -- Prevent of loading the same Section from backend again
      when (Just entry /= state.mTocEntry || version /= state.currentVersion) do
        -- Get the content from server here
        -- We need Aff for that and thus cannot go inside Eff
        -- TODO: After creating a new Leaf, we get Nothing in loadedContent
        -- See, why and fix it

        --first we look whether a draft to load is present
        loadedDraftContent <- Request.getJson
          ContentDto.decodeDraftContentWrapper
          ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
              <> "/draft"
          )

        -- when a draft was found, set the dirtyVersion ref to true so user doesn't swap without discarding.
        -- otherwise, switching the section/version means that it can be set to false
{-         case loadedDraftContent of 
          Right _ -> do 
            for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write true r
            isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
              _.mDirtyVersion
            H.liftEffect $ log ("ended up in right segment" <> (show isDirty))
          Left _ -> do 
            pure unit
            for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write false r
            H.liftEffect $ log "ended up in left segment" -}

        loadedContent <- case loadedDraftContent of
          Right res -> pure (Right $ convertDCWToCW res) 
          Left _ -> do 
            Request.getJson
              ContentDto.decodeContentWrapper
              ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
                  <> "/rev/"
                  <> version
              )

{-         loadedContent <- Request.getJson
          ContentDto.decodeContentWrapper
          ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
              <> "/rev/"
              <> version
          ) -}
        let
          wrapper = case loadedContent of
            Left _ -> ContentDto.failureContentWrapper
            Right res -> res
          content = ContentDto.getWrapperContent wrapper

        H.modify_ _
          { mTocEntry = Just entry
          , mContent = Just content
          , isEditorOutdated = version /= "latest"
          , isOnMerge = false
          }

        -- Only secondary Editor has ElementData
        -- Only first Editor gets to load the comments
        if isJust state.compareToElement then do
          handleAction $ ContinueChangeToSection []
        else do
          -- Get comments
          let
            comments = ContentDto.getWrapperComments wrapper
            -- convert markers
            markers = map ContentDto.convertToAnnotetedMarker comments
          -- update the markers into state
          H.modify_ \st -> st
            { commentState = st.commentState
                { selectedLiveMarker = Nothing
                , markerAnnoHS = empty
                , oldMarkerAnnoPos = empty
                , markers = markers
                }
            , isEditorOutdated = version /= "latest"
            }
          -- Get comments information from Comment Child
          H.raise (RequestComments state.docID entry.id)
        --will be set to true right now, but should be set to false if didn't change to draft
        case loadedDraftContent of 
          Right _ -> do 
            pure unit
          Left _ -> do 
            for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write false r
      pure unit

    -- After getting information from from Comment
    ContinueChangeToSection fCs -> do
      state <- H.get
      case state.mListener of
        Nothing -> pure unit
        Just listener -> do
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
              Document.setValue content
                document
              Editor.setReadOnly (state.compareToElement /= Nothing) ed

              -- reset Ref, because loading new content is considered 
              -- changing the existing content, which would set the flag
              for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write false r

              -- Reset Undo history
              undoMgr <- Session.getUndoManager session
              UndoMgr.reset undoMgr

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
            H.modify_ \st -> st
              { commentState = st.commentState { liveMarkers = newLiveMarkers } }

  -- convert Hashmap to Annotations and show them
  -- H.liftEffect $ setAnnotations commentState.markerAnnoHS state.mEditor

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveUpToDateUpdate mVersion a -> do
      H.modify_ _ { upToDateVersion = mVersion }
      pure (Just a)

    EditorResize a -> do
      editor_ <- H.gets _.mEditor
      case editor_ of
        Nothing -> pure unit
        Just ed -> H.liftEffect $ Editor.resize (Just true) ed
      pure (Just a)

    ChangeSection entry rev a -> do
      handleAction (ChangeToSection entry rev)
      pure (Just a)

    ContinueChangeSection fCs a -> do
      handleAction (ContinueChangeToSection fCs)
      pure (Just a)

    UpdateNodePosition path a -> do
      H.modify_ \state ->
        case state.mNodePath of
          Just _ -> state { mNodePath = Just path }
          Nothing -> state
      pure (Just a)

    SaveSection a -> do
      handleAction $ Save false
      pure (Just a)

    -- Repurpose it to confirm, that the first comment was sent
    UpdateComment newCommentSection a -> do
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
              , markerText: first.author
              , mCommentSection: Just newCommentSection
              }
            newMarkers = snoc state.commentState.markers newMarker
            -- delete temp id from hash map
            newOldMarkerAnnoPos = delete (-360) state.commentState.oldMarkerAnnoPos
            -- add the real id instead
            newOldMarkerAnnoPos' = insert newMarker.id newMarker.startRow
              newOldMarkerAnnoPos
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
                { oldMarkerAnnoPos = newOldMarkerAnnoPos'
                , tmpLiveMarker = Nothing
                , selectedLiveMarker = Just newLM
                , markers = newMarkers
                , liveMarkers = newLiveMarkers
                }
          H.modify_ _ { commentState = newCommentState }
          -- Save new created comment
          -- set dirty to true to be able to save
          for_ state.saveState.mDirtyRef \r -> H.liftEffect $ Ref.write true r
          handleAction $ Save false
        _, _, _, _ -> pure unit
      pure (Just a)

    -- Comes from CommentOverview
    SelectCommentSection markerID a -> do
      lms <- H.gets _.commentState.liveMarkers
      editor_ <- H.gets _.mEditor
      case (find (\m -> m.annotedMarkerID == markerID) lms) of
        -- Comment not found because it is resolved
        Nothing -> do
          handleAction HideHandles
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

    ToDeleteComment a -> do
      state <- H.get
      case state.mEditor, state.commentState.selectedLiveMarker of
        Just ed, Just lm -> do
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ removeLiveMarker lm session
          handleAction $ DeleteAnnotation lm false true
          handleAction $ HideHandles

          let
            newCommentState = state.commentState
              { selectedLiveMarker = Nothing
              , tmpLiveMarker = case state.commentState.tmpLiveMarker of
                  Just tmpLM ->
                    if (tmpLM.annotedMarkerID == lm.annotedMarkerID) then
                      Nothing
                    else
                      Just tmpLM
                  Nothing -> Nothing
              -- delete this marker from state. Otherwise, it can be still selected
              , liveMarkers = deleteBy
                  (\b c -> b.annotedMarkerID == c.annotedMarkerID)
                  lm
                  state.commentState.liveMarkers
              }
          H.modify_ \st -> st
            { commentState = newCommentState }
        _, _ -> pure unit
      pure (Just a)

    RequestDirtyVersion reply -> do
      isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets
        _.mDirtyVersion
      pure (Just (reply isDirty))

    ResetDirtyVersion a -> do
      state <- H.get
      for_ state.mDirtyVersion \r -> H.liftEffect $ Ref.write false r
      pure (Just a)

-- | Change listener for the editor.
--
--   This function should implement stuff like parsing and syntax analysis,
--   linting, code completion, etc.
--   For now, it puts "  " in front of "#", if it is placed at the
--   beginning of a line
--  Update: it also detects, when content is changed and set dref flag
addChangeListenerWithRef
  :: Types.Editor
  -> Ref Boolean
  -> Ref Boolean
  -> HS.Listener Action
  -> Effect Unit
addChangeListenerWithRef editor_ dref vref listener = do
  session <- Editor.getSession editor_
  -- in order to prevent an ifinite loop with this listener
  guardRef <- Ref.new false
  Session.onChange session \(Types.DocumentEvent { action, start, end: _, lines }) ->
    do
      -- set dirty flag
      Ref.write true dref
      Ref.write true vref
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
            range <- Range.create sRow sCol sRow (sCol + 1)
            Session.replace range "  #" session
            Ref.write false guardRef

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
        Ref.read markerRef >>= flip Session.removeMarker session
        Types.Position { row: startRow, column: startColumn } <- Anchor.getPosition
          startAnchor
        Types.Position { row: endRow, column: endColumn } <- Anchor.getPosition
          endAnchor
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
  { markerAnnoHS: empty
  , oldMarkerAnnoPos: empty
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
  }

initialSaveState :: SaveState
initialSaveState =
  { mDirtyRef: Nothing
  , mBeforeUnloadL: Nothing
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
  , mContent: Nothing
  , commentState: initialCommentState
  , fontSize: 12
  , mListener: Nothing
  , resizeObserver: Nothing
  , resizeSubscription: Nothing
  , showButtonText: true
  , showButtons: true
  , saveState: initialSaveState
  , compareToElement: input.elementData
  , isEditorOutdated: false
  , outdatedInfoPopup: false
  , discardPopup: false
  , mDirtyVersion: Nothing
  , isOnMerge: false
  , upToDateVersion: Nothing
  }

makeEditorToolbarButton
  :: forall m. Boolean -> String -> Action -> String -> H.ComponentHTML Action () m
makeEditorToolbarButton enabled tooltip action biName = HH.button
  [ HP.classes
      ( prependIf (not enabled) HB.opacity25
          [ HB.btn, HB.p0, HB.m0, HB.border0 ]
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
  -> Action
  -> String
  -> String
  -> H.ComponentHTML Action () m
makeEditorToolbarButtonWithText enabled asText action biName smallText = HH.button
  ( prependIf (not asText) (HP.title smallText)
      [ HP.classes
          ( prependIf (not enabled) HB.opacity25
              [ HB.btn, HB.btnOutlineDark, HB.px1, HB.py0, HB.m0 ]
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