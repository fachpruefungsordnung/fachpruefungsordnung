-- | It has 3 split views: a sidebar, an editor, and a preview area.
-- | Between each of the views, there are resizers that allow the user to adjust the width
-- | of each section. The sidebar contains a table of contents (TOC) with clickable entries
-- | that jump to specific sections in the editor. The editor allows users to edit content,
-- | and the preview area displays the output based on the editor's content.

module FPO.Components.Splitview where

import Prelude

import Data.Array
  ( cons
  , deleteAt
  , head
  , insertAt
  , mapWithIndex
  , null
  , snoc
  , uncons
  , updateAt
  , (!!)
  )
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Formatter.DateTime (Formatter, parseFormatString)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Unsafe (unsafePerformEffect)
import FPO.Components.Comment as Comment
import FPO.Components.CommentOverview as CommentOverview
import FPO.Components.Editor as Editor
import FPO.Components.Editor.Types (ElementData)
import FPO.Components.Preview as Preview
import FPO.Components.TOC (Path, SelectedEntity(..), Version)
import FPO.Components.TOC as TOC
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (LoadState(..))
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Data.Time (defaultFormatter, timeStampsVersions)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.DocumentDto.DocumentTree as DT
import FPO.Dto.DocumentDto.MetaTree (emptyMetaMap)
import FPO.Dto.DocumentDto.MetaTree as MM
import FPO.Dto.DocumentDto.TreeDto
  ( Edge(..)
  , RootTree(..)
  , Tree(..)
  , TreeHeader(..)
  , errorMeta
  , findRootTree
  , modifyNodeRootTree
  , updateHeading
  )
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( TOCEntry
  , TOCTree
  , documentTreeToTOCTree
  , emptyTOCEntry
  , findTOCEntry
  , findTitleTOCEntry
  , tocTreeToDocumentTree
  )
import FPO.UI.Modals.DirtyVersionModal (dirtyVersionModal)
import FPO.UI.Resizing
  ( ResizeState
  , resizeFromLeft
  , resizeFromRight
  , resizersTotalWidth
  , togglePreview
  , toggleSidebar
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(Proxy))
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.File.Url (createObjectURL, revokeObjectURL)
import Web.HTML (window)
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)
import Web.HTML.Window as Web.HTML.Window
import Web.ResizeObserver (ResizeObserver, disconnect, observe, resizeObserver)
import Web.UIEvent.MouseEvent (MouseEvent, clientX)
import Web.UIEvent.MouseEvent as MouseEvent

data DragTarget = LeftResizer | RightResizer

derive instance eqDragTarget :: Eq DragTarget

type Output = Unit
type Input = DocumentID

data Query a = UnitQuery a

data Action
  = Init
  | Receive (Connected FPOTranslator Input)
  -- Resizing Actions
  | StartResize DragTarget MouseEvent
  | StopResize MouseEvent
  | HandleMouseMove MouseEvent
  | HandleWindowResize Number
  -- Toggle buttons
  | CloseComment
  | ToggleCommentOverview Boolean
  | ToggleSidebar
  | SwitchPreview
  | TogglePreview
  -- Query Output
  | HandleComment Comment.Output
  | HandleCommentOverview CommentOverview.Output
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output
  | HandleTOC TOC.Output
  | GET
  | POST
  -- left part is elementID, center is for left editor, right is for comparison editor
  -- outer maybe determines whether to change this part of the versionMapping,
  -- inner maybe for how. Nothing for left Version means newest version, nothing
  -- for right version means no comparison
  | ModifyVersionMapping Int (Maybe (Maybe Int)) (Maybe (ElementData))
  | UpdateMSelectedTocEntry
  | SetComparison Int (Maybe Int)
  | UpdateVersionMapping
  | UpdateDirtyVersion
  | HideDirtyVersionModal
  -- continues ModifyVersion when approved through the modal
  | ModifyVersionFromModal Int (Maybe Int)
  | DeleteDraft
  | DoNothing
  | Finalize

type State = FPOState
  ( docID :: DocumentID
  , mDragTarget :: Maybe DragTarget

  , resizeState :: ResizeState
  , mStartResizeState :: Maybe ResizeState -- store the state at the start of resizing

  , renderedHtml :: Maybe (LoadState String)
  , testDownload :: String

  -- Store tocEntries tree and send some parts or whole to its children components
  , tocEntries :: TOCTree

  -- store for each element which version should be shown. Nothing means the most recent version should be shown
  -- in the case of the comparison, nothing instead means the comparison is not present
  , versionMapping :: (RootTree ElemVersion)

  -- How the timestamp has to be formatted
  , mTimeFormatter :: Maybe Formatter

  -- Boolean flags for UI state
  -- , compareToElement :: ElementData
  -- this value is updated from the same value in TOC
  , mSelectedTocEntry :: Maybe SelectedEntity
  , dirtyVersion :: Boolean
  , modalData :: Maybe { elementID :: Int, versionID :: Maybe Int }
  -- obtained from TOC
  , upToDateVersion :: Maybe Version
  , pendingUpdateElementID :: Maybe Int
  , inLatest :: Boolean
  , mListener :: Maybe (HS.Listener Action)
  , mResizeObserver :: Maybe ResizeObserver
  , mResizeSubscriptionId :: Maybe SubscriptionId
  )

type ElemVersion =
  { elementID :: Int, versionID :: Maybe Int, comparisonData :: ElementData }

type Slots =
  ( comment :: H.Slot Comment.Query Comment.Output Unit
  , commentOverview :: H.Slot CommentOverview.Query CommentOverview.Output Unit
  , editor :: H.Slot Editor.Query Editor.Output Int
  , preview :: H.Slot Preview.Query Preview.Output Unit
  , toc :: H.Slot TOC.Query TOC.Output Unit
  )

_comment = Proxy :: Proxy "comment"
_commentOverview = Proxy :: Proxy "commentOverview"
_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"
_toc = Proxy :: Proxy "toc"

splitview
  :: forall query m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
splitview = connect selectTranslator $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< Receive
      , finalize = Just Finalize
      }
  }
  where
  initialState :: Connected FPOTranslator Input -> State
  initialState { context, input } =
    { docID: input
    , translator: fromFpoTranslator context
    , mDragTarget: Nothing
    , resizeState:
        { windowWidth: 0.0
        , sidebarRatio: 0.2
        , previewRatio: 0.4
        , editorRatio: 0.4
        , lastExpandedSidebarRatio: 0.2
        , lastExpandedPreviewRatio: 0.4
        , sidebarClosed: false
        , previewClosed: false
        , tocClosed: false
        , commentClosed: true
        , commentOverviewClosed: true
        }
    , mStartResizeState: Nothing
    , renderedHtml: Nothing
    , testDownload: ""
    , tocEntries: Empty
    , versionMapping: Empty
    , mTimeFormatter: Nothing
    , mSelectedTocEntry: Nothing
    , dirtyVersion: false
    , modalData: Nothing
    , upToDateVersion: Nothing
    , pendingUpdateElementID: Nothing
    , inLatest: true
    , mListener: Nothing
    , mResizeObserver: Nothing
    , mResizeSubscriptionId: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_ $
      [ renderSplit state ]
        <> renderDirtyVersionModal
    where
    renderDirtyVersionModal = case state.modalData of
      Nothing -> []
      Just { elementID: eID, versionID: mVID } ->
        [ dirtyVersionModal
            state.translator
            HideDirtyVersionModal
            ModifyVersionFromModal
            eID
            mVID
            DoNothing
        ]

  renderSplit :: State -> H.ComponentHTML Action Slots m
  renderSplit state =
    -- We have to manually shrink the size of those elements, otherwise if they overflow the bottom
    -- of the elements wont be visible anymore
    let
      navbarHeight = 56 -- px, height of the navbar
      contentWidth = state.resizeState.windowWidth - resizersTotalWidth
      ratioScaleFactor = contentWidth / state.resizeState.windowWidth
      absoluteEditorRatio = state.resizeState.editorRatio * ratioScaleFactor
      isDragging = state.mDragTarget /= Nothing
      dragStyle =
        if isDragging then
          "user-select: none; -webkit-user-select: none;"
        else
          ""
    -- toolbarHeight :: Int
    -- toolbarHeight = 31 -- px, height of the toolbar
    in
      HH.div
        [ HE.onMouseMove HandleMouseMove
        , HE.onMouseUp StopResize
        , HE.onMouseLeave StopResize
        , HP.classes [ HB.dFlex, HB.overflowHidden ]
        , HP.style
            ( "height: calc(100vh - " <> show navbarHeight <>
                "px); max-height: 100%; min-height: 0;" <> dragStyle
            )
        , HP.ref (H.RefLabel "splitview")
        ]
        ( -- TOC Sidebar
          renderSidebar state
            <>
              [ -- Editor
                HH.div
                  [ HP.style $ "position: relative; flex: 0 0 "
                      <> show (absoluteEditorRatio * 100.0)
                      <> "%;"
                  ]
                  [ -- The actual editor area
                    HH.div
                      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow0 ]
                      , HP.style
                          "height: 100%; box-sizing: border-box; min-height: 0; overflow: hidden;"
                      ]
                      [ HH.slot _editor 0 Editor.editor
                          { docID: state.docID, elementData: Nothing }
                          HandleEditor
                      ]
                  ]
              ]
            <>
              -- Preview Section
              case state.mSelectedTocEntry of
                Just (SelLeaf tocID) ->
                  let
                    versionEntry = fromMaybe
                      { elementID: -1, versionID: Nothing, comparisonData: Nothing }
                      (findRootTree (\e -> e.elementID == tocID) state.versionMapping)
                  in
                    case versionEntry.comparisonData of
                      Nothing -> renderPreview state
                      Just cData -> renderSecondEditor state (Just cData)
                _ -> renderPreview state
        )

  -- Render both TOC and Comment but make them visible depending on the flags
  -- Always keep them loaded to not load them over and over again
  renderSidebar :: State -> Array (H.ComponentHTML Action Slots m)
  renderSidebar state =
    let
      contentWidth = state.resizeState.windowWidth - resizersTotalWidth
      ratioScaleFactor = contentWidth / state.resizeState.windowWidth
      absoluteSidebarRatio = state.resizeState.sidebarRatio * ratioScaleFactor
    in
      [ -- TOC
        HH.div
          [ HP.classes [ HB.overflowAuto, HB.p1 ]
          , HP.style $
              "flex: 0 0 " <> show (absoluteSidebarRatio * 100.0)
                <>
                  "%; box-sizing: border-box; min-width: 6ch; background:rgb(233, 233, 235); position: relative;"
                <>
                  if
                    not state.resizeState.sidebarClosed
                      && state.resizeState.commentOverviewClosed
                      && state.resizeState.commentClosed
                      && not state.resizeState.tocClosed then
                    ""
                  else
                    "display: none;"
          ]
          [ HH.slot _toc unit TOC.tocview state.docID HandleTOC ]
      -- Comment
      , HH.div
          [ HP.classes [ HB.overflowAuto, HB.p1 ]
          , HP.style $
              "flex: 0 0 " <> show (absoluteSidebarRatio * 100.0)
                <>
                  "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
                <>
                  if
                    not state.resizeState.sidebarClosed && not
                      state.resizeState.commentClosed then
                    ""
                  else
                    "display: none;"
          ]
          [ closeButton CloseComment
          , HH.h4
              [ HP.style
                  "margin-top: 0.5rem; margin-bottom: 1rem; margin-left: 0.5rem; font-weight: bold; color: black;"
              ]
              [ HH.text (translate (label :: _ "comment_comment") state.translator) ]
          , HH.slot _comment unit Comment.commentview unit HandleComment
          ]
      -- CommentOverview
      , HH.div
          [ HP.classes [ HB.overflowAuto, HB.p1 ]
          , HP.style $
              "flex: 0 0 " <> show (absoluteSidebarRatio * 100.0)
                <>
                  "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
                <>
                  if
                    not state.resizeState.sidebarClosed
                      && state.resizeState.commentClosed
                      && not state.resizeState.commentOverviewClosed then
                    ""
                  else
                    "display: none;"
          ]
          [ closeButton $ ToggleCommentOverview false
          , HH.h4
              [ HP.style
                  "margin-top: 0.5rem; margin-bottom: 1rem; margin-left: 0.5rem; font-weight: bold; color: black;"
              ]
              [ HH.text
                  (translate (label :: _ "comment_allComments") state.translator)
              ]
          , HH.slot _commentOverview unit CommentOverview.commentOverviewview unit
              HandleCommentOverview
          ]
      -- Left Resizer
      , HH.div
          [ HE.onMouseDown (StartResize LeftResizer)
          , HP.style
              "width: 8px; \
              \cursor: col-resize; \
              \background:rgba(0, 0, 0, 0.3); \
              \display: flex; \
              \align-items: center; \
              \justify-content: center; \
              \position: relative;"
          ]
          [ HH.button
              [ HP.style
                  "background:rgba(255, 255, 255, 0.8); \
                  \border: 0.2px solid #aaa; \
                  \padding: 0.1rem 0.1rem; \
                  \font-size: 8px; \
                  \font-weight: bold; \
                  \line-height: 1; \
                  \color:rgba(0, 0, 0, 0.7); \
                  \border-radius: 3px; \
                  \cursor: pointer; \
                  \height: 40px; \
                  \width: 8px;"
              -- To prevent the resizer event under the button
              , HE.handler' (EventType "mousedown") \ev ->
                  unsafePerformEffect do
                    stopPropagation ev
                    pure Nothing -- Do not trigger the mouse down event under the button
              , HE.onClick \_ -> ToggleSidebar
              ]
              [ HH.text if not state.resizeState.sidebarClosed then "⟨" else "⟩" ]
          ]
      ]

  rightResizer :: State -> H.ComponentHTML Action Slots m
  rightResizer state =
    HH.div
      [ HE.onMouseDown (StartResize RightResizer)
      , HP.style
          "width: 8px; \
          \cursor: col-resize; \
          \background:rgba(0, 0, 0, 0.3); \
          \display: flex; \
          \align-items: center; \
          \justify-content: center; \
          \position: relative;"
      ]
      [ HH.button
          [ HP.style
              "background:rgba(255, 255, 255, 0.8); \
              \border: 0.2px solid #aaa; \
              \padding: 0.1rem 0.1rem; \
              \font-size: 8px; \
              \font-weight: bold; \
              \line-height: 1; \
              \color:rgba(0, 0, 0, 0.7); \
              \border-radius: 3px; \
              \cursor: pointer; \
              \height: 40px; \
              \width: 8px;"
          -- To prevent the resizer event under the button
          , HE.handler' (EventType "mousedown") \ev ->
              unsafePerformEffect do
                stopPropagation ev
                pure Nothing -- Do not trigger the mouse down event under the button
          , HE.onClick \_ -> TogglePreview
          ]
          [ HH.text if not state.resizeState.previewClosed then "⟩" else "⟨" ]
      ]

  renderPreview :: State -> Array (H.ComponentHTML Action Slots m)
  renderPreview state =
    let
      contentWidth = state.resizeState.windowWidth - resizersTotalWidth
      ratioScaleFactor = contentWidth / state.resizeState.windowWidth
      absolutePreviewRatio = state.resizeState.previewRatio * ratioScaleFactor
    in
      [ -- Right Resizer
        rightResizer state

      -- Preview
      , if not state.resizeState.previewClosed then
          HH.div
            [ HP.classes [ HB.dFlex, HB.flexColumn ]
            , HP.style $
                "flex: 1 1 "
                  <> show (absolutePreviewRatio * 100.0)
                  <>
                    "%; box-sizing: border-box; min-height: 0; overflow: auto; min-width: 6ch; position: relative;"
            ]
            [ HH.div
                [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
                , HP.style "padding-right: 0.5rem;"
                ]
                [ closeButton SwitchPreview ]
            , HH.slot _preview unit Preview.preview
                { renderedHtml: state.renderedHtml
                , isDragging: state.mDragTarget /= Nothing
                }
                HandlePreview
            ]
        else
          HH.text ""
      ]

  renderSecondEditor :: State -> ElementData -> Array (H.ComponentHTML Action Slots m)
  renderSecondEditor state cData =
    let
      contentWidth = state.resizeState.windowWidth - resizersTotalWidth
      ratioScaleFactor = contentWidth / state.resizeState.windowWidth
      absolutePreviewRatio = state.resizeState.previewRatio * ratioScaleFactor
    in
      [ -- Right Resizer
        rightResizer state
      ,
        -- Preview
        HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 1 1 "
                <> show (absolutePreviewRatio * 100.0)
                <>
                  "%; box-sizing: border-box; min-height: 0; overflow: auto; min-width: 6ch; position: relative;"
          ]
          [ HH.div
              [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
              , HP.style "padding-right: 0.5rem;"
              ]
              [ closeButton SwitchPreview ]
          , HH.slot_ _editor 1 Editor.editor
              { docID: state.docID, elementData: cData }
          ]
      ]

  closeButton :: Action -> H.ComponentHTML Action Slots m
  closeButton action =
    HH.button
      [ HP.classes [ HB.btn, HB.btnSm, HB.btnOutlineSecondary ]
      , HP.style
          "position: absolute; \
          \top: 0.5rem; \
          \right: 0.5rem; \
          \background-color: #fdecea; \
          \color: #b71c1c; \
          \padding: 0.2rem 0.4rem; \
          \font-size: 0.75rem; \
          \line-height: 1; \
          \border: 1px solid #f5c6cb; \
          \border-radius: 0.2rem; \
          \z-index: 10;"
      , HE.onClick \_ -> action
      ]
      [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-x" ] ] [] ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Init -> do
      let
        timeFormat = fromMaybe "" (head timeStampsVersions)
        timeFormatter = Just $ case parseFormatString timeFormat of
          Left _ -> defaultFormatter
          Right formatter -> formatter

      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let width = toNumber intWidth
      H.modify_ \st -> st
        { resizeState = st.resizeState { windowWidth = width }
        }
      H.tell _editor 0 (Editor.UpdateEditorSize (0.4 * width))

      H.modify_ \st -> do
        st { mTimeFormatter = timeFormatter }
      H.tell _comment unit (Comment.ReceiveTimeFormatter timeFormatter)
      H.tell _commentOverview unit
        (CommentOverview.ReceiveTimeFormatter timeFormatter)
      H.tell _toc unit (TOC.ReceiveTOCs Empty emptyMetaMap)
      -- Load the initial TOC entries into the editor
      handleAction GET
      handleAction UpdateMSelectedTocEntry
      H.tell _toc unit (TOC.SelectFirstEntry)

      -- Subscribe to resize events and store subscription for cleanup
      { emitter, listener } <- H.liftEffect HS.create
      subscription <- H.subscribe emitter

      H.modify_ _
        { mListener = Just listener
        , mResizeSubscriptionId = Just subscription
        }

      H.getHTMLElementRef (H.RefLabel "splitview") >>= traverse_ \el -> do
        -- Set up ResizeObserver to monitor size changes
        let
          callback _ _ = do
            -- Get the current width directly from the element
            width_ <- HTMLElement.offsetWidth el
            HS.notify listener (HandleWindowResize width_)

        observer <- H.liftEffect $ resizeObserver callback
        H.liftEffect $ observe (HTMLElement.toElement el) {} observer
        H.modify_ _ { mResizeObserver = Just observer }

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

    UpdateDirtyVersion -> do
      isDirty <- H.request _editor 0 Editor.RequestDirtyVersion
      case isDirty of
        Just dirty -> do
          H.modify_ _ { dirtyVersion = dirty }
        Nothing -> pure unit

    HideDirtyVersionModal -> do
      H.modify_ _ { modalData = Nothing }

    ModifyVersionFromModal elementID mVID -> do
      H.modify_ _ { modalData = Nothing }
      H.tell _editor 0 Editor.ResetDirtyVersion
      state <- H.get
      handleAction DeleteDraft
      handleAction (ModifyVersionMapping elementID (Just mVID) Nothing)
      case (findTOCEntry elementID state.tocEntries) of
        Nothing -> pure unit
        Just entry -> do
          mmTitle <- H.request _toc unit TOC.RequestFullTitle
          H.tell _editor 0 (Editor.ChangeSection entry mVID (join mmTitle))

    -- API Actions

    POST -> do
      state <- H.get
      let
        tree = tocTreeToDocumentTree state.tocEntries
        encodedTree = DT.encodeDocumentTree tree

      _ <- Request.postJson Right ("/docs/" <> show state.docID <> "/tree")
        encodedTree
      pure unit

    GET -> do
      s <- H.get
      maybeTree <- Request.getJson MM.decodeDocumentWithMetaMap
        ("/docs/" <> show s.docID <> "/tree/latest")
      case maybeTree of
        Left err -> Store.addError err
        Right (MM.DocumentTreeWithMetaMap { tree, metaMap }) -> do
          let
            finalTree = documentTreeToTOCTree tree
            -- Preserve existing version mapping data when updating the tree
            vMapping = map
              ( \elem ->
                  case
                    findRootTree (\v -> v.elementID == elem.id) s.versionMapping
                    of
                    Just existingEntry -> existingEntry -- Keep existing version and comparison data
                    Nothing ->
                      { elementID: elem.id
                      , versionID: Nothing
                      , comparisonData: Nothing
                      } -- New entries get defaults
              )
              finalTree
          H.modify_ _
            { tocEntries = finalTree
            , versionMapping = vMapping
            }
          H.tell _toc unit (TOC.ReceiveTOCs finalTree metaMap)

    -- Resizing as long as mouse is hold down on window
    -- (Or until the browser detects the mouse is released)
    StartResize dragTarget mouseEvent -> do
      startResizing <- case dragTarget of
        LeftResizer -> do
          sidebarClosed <- H.gets _.resizeState.sidebarClosed
          pure $ not sidebarClosed
        RightResizer -> do
          previewClosed <- H.gets _.resizeState.previewClosed
          pure $ not previewClosed
      when startResizing $ do
        H.liftEffect $ preventDefault (MouseEvent.toEvent mouseEvent)
        H.modify_ \st -> st
          { mDragTarget = Just dragTarget
          , mStartResizeState = Just st.resizeState
          }
        handleAction $ HandleMouseMove mouseEvent

    -- Stop resizing, when mouse is released (is detected by browser)
    -- the parameter cannot be deleted here because there is always a MouseEvent present, we just don't need it here
    StopResize _ -> do
      H.modify_ _ { mDragTarget = Nothing, mStartResizeState = Nothing }

    HandleWindowResize width -> do
      H.modify_ \st -> st
        { resizeState = st.resizeState
            { windowWidth = width
            }
        }
      newResizeState <- H.gets _.resizeState
      H.tell _editor 0 (Editor.UpdateEditorSize (newResizeState.editorRatio * width))
      H.tell _editor 1 (Editor.UpdateEditorSize (newResizeState.previewRatio * width))

    -- While mouse is hold down, resizer move to position of mouse
    -- (with certain rules)
    -- Mouse here is a HTML MouseEvent
    HandleMouseMove mouseEvent -> do
      state <- H.get
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        mouseXPos = toNumber $ clientX mouseEvent
        width = toNumber intWidth

      case state.mDragTarget, state.mStartResizeState of
        -- Resizing TOC or Comment Sidebar
        Just LeftResizer, Just startResizeState -> do
          let newResizeState = resizeFromLeft startResizeState mouseXPos

          H.modify_ \st -> st { resizeState = newResizeState }

          H.tell _editor 0
            (Editor.UpdateEditorSize (newResizeState.editorRatio * width))
          H.tell _editor 1
            (Editor.UpdateEditorSize (newResizeState.previewRatio * width))

        Just RightResizer, Just startResizeState -> do
          let
            mousePxFromRight = width - mouseXPos
            newResizeState = resizeFromRight startResizeState mousePxFromRight

          H.modify_ \st -> st { resizeState = newResizeState }

          H.tell _editor 0
            (Editor.UpdateEditorSize (newResizeState.editorRatio * width))
          H.tell _editor 1
            (Editor.UpdateEditorSize (newResizeState.previewRatio * width))

        _, _ -> pure unit

    UpdateMSelectedTocEntry -> do
      cToc <- H.request _toc unit TOC.RequestCurrentTocEntry
      H.modify_ _ { mSelectedTocEntry = join cToc }

    -- for when the tree updates.
    UpdateVersionMapping -> do
      state <- H.get
      let
        newVersionMapping =
          map
            ( \e ->
                case
                  (findRootTree (\v -> v.elementID == e.id) state.versionMapping)
                  of
                  Just entry -> entry
                  Nothing ->
                    { elementID: e.id, versionID: Nothing, comparisonData: Nothing }
            )
            state.tocEntries
      H.modify_ _ { versionMapping = newVersionMapping }

    CloseComment -> do
      H.modify_ \st -> st { resizeState = st.resizeState { commentClosed = true } }
      H.tell _editor 0 (Editor.UnselectCommentSection)

    ToggleCommentOverview shown -> do
      state <- H.get
      if shown then do
        if not state.resizeState.sidebarClosed then
          H.modify_ \st -> st
            { resizeState = st.resizeState
                { commentClosed = true, commentOverviewClosed = false }
            }
        else
          H.modify_ \st -> st
            { resizeState =
                st.resizeState
                  { sidebarRatio = st.resizeState.lastExpandedSidebarRatio
                  , sidebarClosed = false
                  , commentClosed = true
                  , commentOverviewClosed = false
                  }
            }
        H.tell _comment unit Comment.Overview
      else
        H.modify_ \st -> st
          { resizeState = st.resizeState { commentOverviewClosed = true } }

    ToggleSidebar -> do
      state <- H.get
      let newResizeState = toggleSidebar state.resizeState
      H.modify_ \st -> st { resizeState = newResizeState }

      H.tell _editor 0 $ Editor.UpdateEditorSize
        (newResizeState.editorRatio * newResizeState.windowWidth)
      H.tell _editor 1 $ Editor.UpdateEditorSize
        (newResizeState.previewRatio * newResizeState.windowWidth)

    DoNothing -> do
      pure unit

    -- Switch between CompareEditor, Preview and TogglePreview
    SwitchPreview -> do
      state <- H.get
      case state.mSelectedTocEntry of
        Just _ -> H.modify_ _ { mSelectedTocEntry = Nothing }
        Nothing -> handleAction TogglePreview

    -- Toggle the preview area
    TogglePreview -> do
      state <- H.get
      win <- H.liftEffect Web.HTML.window
      width <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        numberWidth = toNumber width
        newResizeState = togglePreview state.resizeState
      H.modify_ \st -> st { resizeState = newResizeState }
      H.tell _editor 0
        (Editor.UpdateEditorSize $ numberWidth * newResizeState.editorRatio)
      H.tell _editor 1
        (Editor.UpdateEditorSize $ numberWidth * newResizeState.previewRatio)

    ModifyVersionMapping tocID vID cData -> do
      previewClosed <- H.gets _.resizeState.previewClosed
      versionMapping <- H.gets _.versionMapping
      when previewClosed $
        H.modify_ \st -> st
          { resizeState = st.resizeState
              { previewRatio = st.resizeState.lastExpandedPreviewRatio
              , previewClosed = false
              }
          }
      handleAction UpdateMSelectedTocEntry
      let
        newVersionID = case vID of
          Just id -> const id
          Nothing -> (\v -> v)
        newComparisonData = case cData of
          Just d -> const d
          Nothing -> (\v -> v)
        newVersionMapping =
          modifyNodeRootTree
            (\v -> v.elementID == tocID)
            ( \v ->
                { elementID: v.elementID
                , versionID: newVersionID v.versionID
                , comparisonData: newComparisonData v.comparisonData
                }
            )
            versionMapping
      H.modify_ _ { versionMapping = newVersionMapping }

    SetComparison elementID mVID -> do
      state <- H.get
      let
        tocEntry = fromMaybe
          emptyTOCEntry
          (findTOCEntry elementID state.tocEntries)
        title = fromMaybe
          ""
          (findTitleTOCEntry elementID state.tocEntries)
      handleAction
        ( ModifyVersionMapping elementID Nothing
            (Just (Just { tocEntry: tocEntry, revID: mVID, title: title }))
        )
      mmTitle <- H.request _toc unit TOC.RequestFullTitle
      H.tell _editor 1
        (Editor.ChangeSection tocEntry mVID (join mmTitle))

    -- Query handler

    HandleComment output -> case output of

      Comment.CloseCommentSection -> do
        H.modify_ \st -> st { resizeState = st.resizeState { commentClosed = true } }

      -- behaviour for old versions still to discuss. for now will simply fail if old element version selected.
      Comment.ConfirmComment newCommentSection -> do
        H.tell _editor 0 (Editor.ConfirmComment newCommentSection)

      Comment.CommentOverview fs -> do
        inLatest <- H.gets _.inLatest
        H.tell _commentOverview unit
          (CommentOverview.ReceiveComments inLatest fs)

      Comment.SendAbstractedComments abstractCSs hasProblem -> do
        H.tell _editor 0 (Editor.ContinueChangeSection abstractCSs hasProblem)

      Comment.ToDeleteComment markerID commentProblem -> do
        H.tell _editor 0 (Editor.ToDeleteComment markerID commentProblem)

      Comment.UpdatedComments fs commentProblem -> do
        inLatest <- H.gets _.inLatest
        H.tell _commentOverview unit
          (CommentOverview.ReceiveComments inLatest fs)
        H.tell _editor 0 (Editor.UpdateCommentProblem commentProblem)

      Comment.SetReAnchor reAnchor -> do
        H.tell _editor 0 (Editor.SetReAnchor reAnchor)

    HandleCommentOverview output -> case output of

      CommentOverview.JumpToCommentSection markerID -> do
        H.modify_ \st -> st { resizeState = st.resizeState { commentClosed = false } }
        H.tell _comment unit
          (Comment.SelectedCommentSection markerID)
        H.tell _editor 0 (Editor.SelectCommentSection markerID)

    HandleEditor output -> case output of

      Editor.AddComment -> do
        state <- H.get
        if not state.resizeState.sidebarClosed then
          H.modify_ \st -> st
            { resizeState = st.resizeState { commentClosed = false } }
        else
          H.modify_ \st -> st
            { resizeState = st.resizeState
                { sidebarRatio = st.resizeState.lastExpandedSidebarRatio
                , sidebarClosed = false
                , commentClosed = false
                }
            }
        H.tell _comment unit Comment.AddComment

      Editor.ClickedQuery html -> do
        state <- H.get
        case state.mSelectedTocEntry of
          Just (SelLeaf tocID) -> do
            -- Only reset comparison data if we're not currently in comparison mode
            let
              versionEntry = fromMaybe
                { elementID: -1, versionID: Nothing, comparisonData: Nothing }
                (findRootTree (\e -> e.elementID == tocID) state.versionMapping)
            case versionEntry.comparisonData of
              Nothing -> handleAction
                (ModifyVersionMapping tocID Nothing (Just Nothing))
              Just _ -> pure unit -- Don't reset comparison data when in comparison mode
          _ -> pure unit
        -- Always set renderedHtml
        H.modify_ _
          { renderedHtml = Just (Loaded html)
          }
        -- Only update previewRatio and previewClosed if preview is not already shown
        when state.resizeState.previewClosed do
          H.modify_ \st -> st
            { resizeState = st.resizeState
                { previewRatio = state.resizeState.lastExpandedPreviewRatio
                , previewClosed = false
                }
            }

      Editor.PostPDF _ -> do
        state <- H.get
        upToDateVersion <- H.request _toc unit TOC.RequestUpToDateVersion
        currentTocEntry <- H.request _toc unit TOC.RequestCurrentTocEntry
        let
          textElementId :: Int
          textElementId = case join currentTocEntry of
            Just (SelLeaf id) -> id
            _ -> -1

          revisionId :: Int
          revisionId =
            case
              findRootTree (\e -> e.elementID == textElementId) state.versionMapping
              of
              Just revision -> fromMaybe (-1) revision.versionID
              Nothing -> case upToDateVersion of
                Just (Just version) -> fromMaybe (-1) version.identifier
                _ -> -1
        if textElementId == -1 then do
          updateStore $ Store.AddWarning "No section selected for PDF export"
        else do
          renderedPDF' <- Request.getBlobOrError
            ( "/docs/" <> show state.docID <> "/text/" <> show textElementId
                <> "/rev/"
                <> (if revisionId == -1 then "latest" else show revisionId)
                <> "/pdf"
            )
          tocTitleMaybeMaybe <- H.request _toc unit TOC.RequestCurrentTocEntryTitle
          let
            tocTitleMaybe = join tocTitleMaybeMaybe -- This flattens Maybe (Maybe String) to Maybe String
            tocTitleUgly = fromMaybe "document" tocTitleMaybe
            cleanTocTitle = stripHtmlTags tocTitleUgly
            filename = cleanTocTitle <> ".pdf"
          case renderedPDF' of
            Left _ -> pure unit
            Right blobOrError ->
              case blobOrError of
                Left errMsg -> H.modify_ _
                  { renderedHtml = Just
                      (Loaded ("<pre><code>" <> errMsg <> "</code></pre>"))
                  }
                Right body -> do
                  -- create blobl link
                  url <- H.liftEffect $ createObjectURL body
                  -- Create an invisible link and click it to download PDF
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

      Editor.RequestComments docID entryID markerIDs inLatest -> do
        H.modify_ _ { inLatest = inLatest }
        H.tell _comment unit
          (Comment.RequestComments docID entryID markerIDs inLatest)

      Editor.SelectedCommentSection markerID -> do
        state <- H.get
        if not state.resizeState.sidebarClosed then
          H.modify_ \st -> st
            { resizeState = st.resizeState { commentClosed = false } }
        else
          H.modify_ \st -> st
            { resizeState = st.resizeState
                { sidebarRatio = st.resizeState.lastExpandedSidebarRatio
                , sidebarClosed = false
                , commentClosed = false
                }
            }
        H.tell _comment unit
          (Comment.SelectedCommentSection markerID)

      Editor.RenamedNode newName path -> do
        s <- H.get
        updateTree $ changeNodeHeading path newName s.tocEntries

      Editor.ShowAllCommentsOutput -> do
        handleAction $ ToggleCommentOverview true

      Editor.RaiseDiscard -> do
        handleAction UpdateMSelectedTocEntry
        state <- H.get
        -- Only the SelLeaf case should ever occur
        case state.mSelectedTocEntry of
          Just (SelLeaf id) -> do
            _ <- Request.deleteIgnore
              ("/docs/" <> show state.docID <> "/text/" <> show id <> "/draft")
            handleAction (ModifyVersionMapping id (Just Nothing) (Just Nothing))
            let
              -- Nothing case should never occur
              entry = case (findTOCEntry id state.tocEntries) of
                Nothing -> emptyTOCEntry
                Just e -> e
            mmTitle <- H.request _toc unit TOC.RequestFullTitle
            H.tell _editor 0 (Editor.ChangeSection entry Nothing (join mmTitle))
          _ -> do
            pure unit

      Editor.RaiseMergeMode draft -> do
        handleAction UpdateMSelectedTocEntry
        state <- H.get
        upToDateVersion <- H.request _toc unit TOC.RequestUpToDateVersion
        case upToDateVersion of
          Just (Just version) -> do
            H.modify_ _ { upToDateVersion = Just version }
            H.tell _editor 0 (Editor.ReceiveUpToDateUpdate (Just version))
            case state.mSelectedTocEntry of
              Just (SelLeaf id) -> do
                let
                  tocEntry = fromMaybe
                    emptyTOCEntry
                    (findTOCEntry id state.tocEntries)
                  title = fromMaybe
                    ""
                    (findTitleTOCEntry id state.tocEntries)
                handleAction
                  ( ModifyVersionMapping id Nothing
                      ( Just
                          ( Just
                              { tocEntry: tocEntry
                              , revID: version.identifier
                              , title: title
                              }
                          )
                      )
                  )
                --handleAction (SetComparison id Nothing)
                H.tell _editor 1 (Editor.SetContent draft)
              _ -> pure unit
          _ -> do
            pure unit

      Editor.RaiseUpdateVersion mVID -> do
        state <- H.get
        let
          targetElementID =
            case state.pendingUpdateElementID of
              Just id -> Just id
              Nothing ->
                case state.mSelectedTocEntry of
                  Just (SelLeaf id) -> Just id
                  _ -> Nothing
        case targetElementID of
          Just elementID -> do
            H.modify_ _ { pendingUpdateElementID = Nothing }

            handleAction
              (ModifyVersionMapping elementID (Just mVID) Nothing)
            case (findTOCEntry elementID state.tocEntries) of
              Nothing -> pure unit
              Just entry -> do
                mmTitle <- H.request _toc unit TOC.RequestFullTitle
                H.tell _editor 0
                  (Editor.ChangeSection entry mVID (join mmTitle))
          _ -> pure unit

      Editor.Merged -> do
        handleAction UpdateMSelectedTocEntry
        state <- H.get
        case state.mSelectedTocEntry of
          Just (SelLeaf elementID) -> do
            H.modify_ _ { pendingUpdateElementID = Just elementID }
            handleAction DeleteDraft
            handleAction
              (ModifyVersionMapping elementID (Just Nothing) (Just Nothing))
            case (findTOCEntry elementID state.tocEntries) of
              Nothing -> pure unit
              Just entry -> do
                mmTitle <- H.request _toc unit TOC.RequestFullTitle
                H.tell _editor 0 (Editor.ChangeSection entry Nothing (join mmTitle))
          _ -> pure unit

      Editor.UpdateFullTitle -> do
        handleAction GET
        mmTitle <- H.request _toc unit TOC.RequestFullTitle
        H.tell _editor 0 $ Editor.ReceiveFullTitle (join mmTitle)

      Editor.UpdateComment markerIDs -> do
        H.tell _comment unit (Comment.UpdateComment markerIDs)

      Editor.ReaddedAnchor -> do
        H.tell _comment unit Comment.ReaddedAnchor

      Editor.ToUpdateCommentProblem markerID -> do
        H.tell _comment unit (Comment.UpdateCommentProblem markerID)

    DeleteDraft -> do
      handleAction UpdateMSelectedTocEntry
      state <- H.get
      -- Only the SelLeaf case should ever occur
      case state.mSelectedTocEntry of
        Just (SelLeaf id) -> do
          _ <- Request.deleteIgnore
            ("/docs/" <> show state.docID <> "/text/" <> show id <> "/draft")
          pure unit
        _ -> do
          pure unit

    HandlePreview _ -> pure unit

    HandleTOC output -> case output of

      TOC.ModifyVersion elementID mVID -> do
        handleAction UpdateDirtyVersion
        state <- H.get
        let
          currentVersion =
            case
              findRootTree (\e -> e.elementID == elementID) state.versionMapping
              of
              Nothing -> Nothing
              Just version -> version.versionID
        if state.dirtyVersion && currentVersion /= Nothing then do
          H.modify_ _ { modalData = Just { elementID: elementID, versionID: mVID } }
        else do
          handleAction (ModifyVersionMapping elementID (Just mVID) Nothing)
          case (findTOCEntry elementID state.tocEntries) of
            Nothing -> pure unit
            Just entry -> do
              mmTitle <- H.request _toc unit TOC.RequestFullTitle
              H.tell _editor 0 (Editor.ChangeSection entry mVID (join mmTitle))

      TOC.CompareTo elementID vID -> do
        handleAction (SetComparison elementID vID)

      TOC.ChangeToLeaf selectedId mTitle -> do
        stateBefore <- H.get
        let
          currentElementID =
            case stateBefore.mSelectedTocEntry of
              Just (SelLeaf id) -> Just id
              _ -> Nothing
        -- check to avoid weird merge/save race conditions
        isOnMerge <- H.request _editor 0 Editor.IsOnMerge
        if (fromMaybe false isOnMerge) then
          H.tell _editor 0 Editor.PreventChangeSection
        else do
          H.modify_ _ { pendingUpdateElementID = currentElementID }
          H.tell _editor 0 Editor.SaveSection
          handleAction UpdateMSelectedTocEntry
          state <- H.get
          let
            entry = case (findTOCEntry selectedId state.tocEntries) of
              Nothing -> emptyTOCEntry
              Just e -> e
            ev =
              case
                findRootTree (\e -> e.elementID == selectedId) state.versionMapping
                of
                Nothing ->
                  { elementID: -1, versionID: Nothing, comparisonData: Nothing }
                Just elem -> elem
          H.tell _editor 0 (Editor.ChangeSection entry ev.versionID mTitle)
          H.tell _toc unit $ TOC.UpdateMSelectedTocEntry (SelLeaf selectedId) mTitle
          case ev.comparisonData of
            Nothing -> do
              -- this case should be covered by mSelectedTocEntry being set to Nothing
              pure unit
            Just d ->
              H.tell _editor 1 (Editor.ChangeSection d.tocEntry d.revID mTitle)

      TOC.UpdateNodePosition path -> do
        H.tell _editor 0 (Editor.UpdateNodePosition path)

      TOC.ChangeToNode path heading mTitle -> do
        isOnMerge <- H.request _editor 0 Editor.IsOnMerge
        if (fromMaybe false isOnMerge) then
          H.tell _editor 0 Editor.PreventChangeSection
        else do
          H.tell _editor 0 (Editor.ChangeToNode heading path mTitle)
          H.tell _toc unit $ TOC.UpdateMSelectedTocEntry (SelNode path heading)
            (Just heading)

      TOC.AddNode path node -> do
        s <- H.get
        updateTree $ addRootNode path node s.tocEntries

      TOC.DeleteNode path -> do
        s <- H.get
        updateTree $ deleteRootNode path s.tocEntries

      TOC.ReorderItems { from, to } -> do
        s <- H.get
        updateTree $ reorderTocEntries from to s.tocEntries
        H.tell _editor 0 Editor.SetDirtyFlag
        H.tell _editor 0 Editor.SaveSection
        mmTitle <- H.request _toc unit TOC.RequestFullTitle
        H.tell _editor 0 $ Editor.ReceiveFullTitle (join mmTitle)

      TOC.RenameNode _ -> do
        -- s <- H.get
        -- updateTree $ changeNodeName path newName s.tocEntries
        pure unit

    Finalize -> do
      -- Clean up ResizeObserver
      mObserver <- H.gets _.mResizeObserver
      case mObserver of
        Just observer -> H.liftEffect $ disconnect observer
        Nothing -> pure unit

    where
    -- Communicates tree changes to the server and TOC component.
    updateTree newTree = do
      state <- H.get
      let
        doctTree = tocTreeToDocumentTree newTree
        encodedTree = DT.encodeDocumentTree doctTree

      -- TODO: This transaction actually returns an almost good-enough tree,
      --       but unfortunately the leaves have as content just the id,
      --       not the full TOCEntry. So we do not use the returned tree ...
      _ <- Request.postJson Right ("/docs/" <> show state.docID <> "/tree")
        encodedTree

      -- TODO: ... but instead fetch the latest tree again. A bit inefficient,
      --       but good enough for now. We should probably change the server
      --       endpoint to return the full tree with TOCEntries, given that this
      --       wouldn't cause trouble elsewhere.
      maybeTree <- Request.getJson MM.decodeDocumentWithMetaMap
        ("/docs/" <> show state.docID <> "/tree/latest")

      -- TODO: After changing the TOC (e.g., adding a node in the TOC),
      --       we receive a new tree from the server. This code was just
      --       yoinked from above, and we should implement just one function
      --       or action to handle receiving a new tree and meta map from
      --       the server.
      case maybeTree of
        Left err -> Store.addError err
        Right (MM.DocumentTreeWithMetaMap { tree, metaMap }) -> do
          let
            finalTree = documentTreeToTOCTree tree
          H.modify_ _
            { tocEntries = finalTree
            }
          H.tell _toc unit (TOC.ReceiveTOCs finalTree metaMap)

      handleAction UpdateVersionMapping

{- ------------------ Tree traversal and mutation function ------------------ -}
{- --------------------- TODO: Move to seperate module  --------------------- -}
-- TODO(lasse): Clean up redundant cases. Update `changeNodeName` regarding headings.

-- Add a node in TOC tree
addRootNode
  :: Path
  -> Tree TOCEntry
  -> TOCTree
  -> TOCTree
addRootNode [] entry (RootTree { children, header }) =
  RootTree { children: snoc children (Edge entry), header }
addRootNode _ entry Empty =
  RootTree
    { children: [ Edge entry ]
    , header: TreeHeader { headerKind: "root", headerType: "root", heading: "" }
    }
addRootNode path entry (RootTree { children, header }) =
  case uncons path of
    Nothing ->
      RootTree { children: snoc children (Edge entry), header }
    Just { head, tail } ->
      let
        child =
          fromMaybe
            ( Edge
                ( Leaf
                    { meta: errorMeta
                    , node: emptyTOCEntry
                    }
                )
            )
            (children !! head)
        newChildren =
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        RootTree { children: newChildren, header }

addNode
  :: Path
  -> Tree TOCEntry
  -> Edge TOCEntry
  -> Edge TOCEntry
addNode _ _ (Edge (Leaf { meta, node })) =
  Edge (Leaf { meta, node }) -- Cannot add to a leaf
addNode [] entry (Edge (Node { meta, children, header })) =
  Edge (Node { meta, children: snoc children (Edge entry), header })
addNode path entry (Edge (Node { meta, children, header })) =
  case uncons path of
    Nothing ->
      Edge (Node { meta, children: snoc children (Edge entry), header })
    Just { head, tail } ->
      let
        child =
          fromMaybe
            ( Edge
                ( Leaf
                    { meta: errorMeta
                    , node: emptyTOCEntry
                    }
                )
            )
            (children !! head)
        newChildren' =
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        Edge (Node { meta, children: newChildren', header })

deleteRootNode
  :: Array Int
  -> TOCTree
  -> TOCTree
deleteRootNode _ Empty = Empty
deleteRootNode [] _ = Empty
deleteRootNode path (RootTree { children, header }) =
  case uncons path of
    Nothing ->
      RootTree { children, header } -- no path, do nothing
    Just { head, tail } ->
      if null tail then
        -- Delete the child at index `head`
        case deleteAt head children of
          Nothing -> RootTree { children, header }
          Just newChildren -> RootTree { children: newChildren, header }
      else
        let
          child =
            fromMaybe
              ( Edge
                  ( Leaf
                      { meta: errorMeta
                      , node: emptyTOCEntry
                      }
                  )
              )
              (children !! head)
          newChildren =
            case updateAt head (deleteNode tail child) children of
              Nothing -> children
              Just res -> res
        in
          RootTree { children: newChildren, header }

deleteNode
  :: Array Int
  -> Edge TOCEntry
  -> Edge TOCEntry
deleteNode _ edge@(Edge (Leaf _)) =
  edge -- Cannot delete deeper inside a leaf
deleteNode [] e =
  -- If path is empty, delete this node entirely is handled by parent
  -- so this case should not normally be reached.
  e
deleteNode path (Edge (Node { meta, children, header })) =
  case uncons path of
    Nothing ->
      Edge (Node { meta, children, header })
    Just { head, tail } ->
      if null tail then
        case deleteAt head children of
          Nothing -> Edge (Node { meta, children, header })
          Just newChildren -> Edge (Node { meta, children: newChildren, header })
      else
        let
          child =
            fromMaybe
              ( Edge
                  ( Leaf
                      { meta: errorMeta
                      , node: emptyTOCEntry
                      }
                  )
              )
              (children !! head)
          newChildren' =
            case updateAt head (deleteNode tail child) children of
              Nothing -> children
              Just res -> res
        in
          Edge (Node { meta, children: newChildren', header })

-- Reorder TOC entries by moving a node from `sourcePath` to `targetPath`.
-- The node at sourcePath takes the place of the node at targetPath,
-- and everything shifts accordingly.
reorderTocEntries :: Array Int -> Array Int -> TOCTree -> TOCTree
reorderTocEntries sourcePath targetPath tree
  | sourcePath == targetPath = tree
  | otherwise = case extractNodeAtPath sourcePath tree of
      Nothing -> tree
      Just extractedNode ->
        let
          treeWithoutSource = deleteRootNode sourcePath tree
          adjustedTargetPath = adjustPathAfterDeletion sourcePath targetPath
        in
          insertNodeAtPosition adjustedTargetPath extractedNode treeWithoutSource

-- Adjust target path after source deletion to account for index shifts
adjustPathAfterDeletion :: Array Int -> Array Int -> Array Int
adjustPathAfterDeletion sourcePath targetPath =
  adjustPathRecursive sourcePath targetPath

adjustPathRecursive :: Array Int -> Array Int -> Array Int
adjustPathRecursive sourcePath targetPath =
  case uncons sourcePath, uncons targetPath of
    Just { head: srcHead, tail: srcTail }, Just { head: tgtHead, tail: tgtTail } ->
      if null srcTail && null tgtTail then
        -- Both are at the same level (siblings)
        if tgtHead > srcHead then
          -- Target is after source, so decrement target index since source was removed
          [ tgtHead - 1 ]
        else
          -- Target is before or at source position, stays the same
          targetPath
      else if srcHead == tgtHead then
        -- Same parent, continue recursively
        cons tgtHead (adjustPathRecursive srcTail tgtTail)
      else
        -- Different branches at this level
        if null srcTail then
        -- Source is being deleted at this level
        if tgtHead > srcHead then
          cons (tgtHead - 1) tgtTail
        else
          targetPath
      else
        -- Source deletion is deeper, no adjustment needed
        targetPath
    _, _ -> targetPath

-- Extract a node at a given path without deleting it
extractNodeAtPath :: Path -> TOCTree -> Maybe (Tree TOCEntry)
extractNodeAtPath _ Empty = Nothing
extractNodeAtPath [] _ = Nothing -- Cannot extract root
extractNodeAtPath path (RootTree { children }) =
  case uncons path of
    Nothing -> Nothing
    Just { head, tail } ->
      case children !! head of
        Nothing -> Nothing
        Just (Edge node) ->
          if null tail then
            Just node
          else
            extractNodeFromTree tail node

extractNodeFromTree :: Path -> Tree TOCEntry -> Maybe (Tree TOCEntry)
extractNodeFromTree _ (Leaf _) = Nothing -- Cannot go deeper in leaf
extractNodeFromTree [] node = Just node
extractNodeFromTree path (Node { children }) =
  case uncons path of
    Nothing -> Nothing
    Just { head, tail } ->
      case children !! head of
        Nothing -> Nothing
        Just (Edge node) ->
          if null tail then
            Just node
          else
            extractNodeFromTree tail node

-- Insert node at the exact target position (pushing existing nodes down)
insertNodeAtPosition :: Path -> Tree TOCEntry -> TOCTree -> TOCTree
insertNodeAtPosition [] node tree =
  -- Insert at root level (append to end)
  case tree of
    Empty -> RootTree
      { children: [ Edge node ]
      , header: TreeHeader { headerKind: "root", headerType: "root", heading: "" }
      }
    RootTree { children, header } ->
      RootTree { children: snoc children (Edge node), header }

insertNodeAtPosition _ node Empty =
  RootTree
    { children: [ Edge node ]
    , header: TreeHeader { headerKind: "root", headerType: "root", heading: "" }
    }

insertNodeAtPosition path node (RootTree { children, header }) =
  case uncons path of
    Nothing -> RootTree { children: snoc children (Edge node), header }
    Just { head, tail } ->
      if null tail then
        -- Insert exactly at position `head`, pushing existing elements down
        case insertAt head (Edge node) children of
          Nothing ->
            -- If insertion fails (index out of bounds), append to end
            RootTree { children: snoc children (Edge node), header }
          Just result ->
            RootTree { children: result, header }
      else
        -- Navigate deeper into the tree
        case children !! head of
          Nothing ->
            -- Path doesn't exist, cannot insert deeper
            RootTree { children, header }
          Just childEdge ->
            let
              newChild = insertNodeIntoEdgeAtPosition tail node childEdge
              newChildren = case updateAt head newChild children of
                Nothing -> children
                Just res -> res
            in
              RootTree { children: newChildren, header }

insertNodeIntoEdgeAtPosition
  :: Path -> Tree TOCEntry -> Edge TOCEntry -> Edge TOCEntry
insertNodeIntoEdgeAtPosition _ _ edge@(Edge (Leaf _)) = edge -- Cannot insert into leaf
insertNodeIntoEdgeAtPosition [] node (Edge (Node { meta, children, header })) =
  -- Insert at end of children
  Edge (Node { meta, children: snoc children (Edge node), header })
insertNodeIntoEdgeAtPosition path node (Edge (Node { meta, children, header })) =
  case uncons path of
    Nothing -> Edge (Node { meta, children: snoc children (Edge node), header })
    Just { head, tail } ->
      if null tail then
        -- Insert exactly at position `head`, pushing existing elements down
        case insertAt head (Edge node) children of
          Nothing ->
            -- If insertion fails (index out of bounds), append to end
            Edge (Node { meta, children: snoc children (Edge node), header })
          Just result ->
            Edge (Node { meta, children: result, header })
      else
        -- Navigate deeper
        case children !! head of
          Nothing -> Edge (Node { meta, children, header })
          Just childEdge ->
            let
              newChild = insertNodeIntoEdgeAtPosition tail node childEdge
              newChildren = case updateAt head newChild children of
                Nothing -> children
                Just res -> res
            in
              Edge (Node { meta, children: newChildren, header })

-- Changes the heading of a node in the TOC root tree.
changeNodeHeading
  :: Path -> String -> TOCTree -> TOCTree
changeNodeHeading _ _ Empty = Empty
changeNodeHeading path newName (RootTree { children, header }) =
  let
    newChildren = mapWithIndex
      ( \ix (Edge child) ->
          case uncons path of
            Just { head, tail } | ix == head ->
              Edge $ changeNodeHeading' tail newName child
            _ -> Edge child
      )
      children
  in
    RootTree { children: newChildren, header }

changeNodeHeading' :: Path -> String -> Tree TOCEntry -> Tree TOCEntry
changeNodeHeading' path newName tree = case path of
  [] -> case tree of
    Node r -> Node r { header = updateHeading newName r.header }
    leaf -> leaf
  _ -> case tree of
    Node { meta, children, header } ->
      case uncons path of
        Just { head: index, tail } ->
          let
            newChildren = mapWithIndex
              ( \ix (Edge child) ->
                  if ix == index then Edge $ changeNodeHeading' tail newName child
                  else Edge child
              )
              children
          in
            Node { meta, children: newChildren, header }
        Nothing -> Node { meta, children, header }
    leaf -> leaf

stripHtmlTags :: String -> String
stripHtmlTags input =
  case Regex.regex "<[^>]*>" (RegexFlags.global <> RegexFlags.ignoreCase) of
    Left _ -> input -- If regex compilation fails, return original string
    Right htmlRegex -> Regex.replace htmlRegex "" input
