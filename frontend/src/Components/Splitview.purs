-- | It has 3 split views: a sidebar, an editor, and a preview area.
-- | Between each of the views, there are resizers that allow the user to adjust the width
-- | of each section. The sidebar contains a table of contents (TOC) with clickable entries
-- | that jump to specific sections in the editor. The editor allows users to edit content,
-- | and the preview area displays the output based on the editor's content.

module FPO.Component.Splitview where

import Prelude

import Ace.Range as Range
import Data.Array (head, intercalate, range)
import Data.Formatter.DateTime (Formatter)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Now (nowDateTime)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Comment as Comment
import FPO.Components.Editor as Editor
import FPO.Components.Preview as Preview
import FPO.Components.TOC as TOC
import FPO.Types (AnnotatedMarker, Comment, CommentSection, TOCEntry, findTOCEntry, timeStampsVersions)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.MouseEvent (MouseEvent, clientX)


data DragTarget = ResizeLeft | ResizeRight

derive instance eqDragTarget :: Eq DragTarget

type Output = Unit
type Input = Unit
data Query a = UnitQuery a

data Action
  = Init
  -- Resizing Actions
  | StartResize DragTarget MouseEvent
  | StopResize MouseEvent
  | HandleMouseMove MouseEvent
  -- Toolbar buttons
  | ClickedHTTPRequest
  | SaveSection
  | QueryEditor
  | ClickLoadPdf
  | ShowWarning
  -- Toggle buttons
  | ToggleSidebar
  | TogglePreview
  -- Query Output
  | HandleComment Comment.Output
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output
  | HandleTOC TOC.Output

type State =
  { dragTarget :: Maybe DragTarget

  -- Store the width values as ratios of the total width
  -- TODO: Using the ratios to keep the ratio, when resizing the window
  --      But how do we get the event of window resize?

  -- Instead of setting the width directly to mouse position, calculate a delta
  -- for a smoother and correct resize experience with the start positions
  , startMouseRatio :: Number
  , startSidebarRatio :: Number
  , startMiddleRatio :: Number

  -- The current widths of the sidebar and middle content (as percentage ratios)
  , sidebarRatio :: Number
  , middleRatio :: Number

  -- The last expanded sidebar width, used to restore the sidebar when toggling
  , lastExpandedSidebarRatio :: Number
  , lastExpandedMiddleRatio :: Number

  -- There are 2 ways to send content to preview:
  -- 1. This editorContent is sent through the slot in renderPreview
  -- 2. Throuth QueryEditor, where the editor collects its content and sends it
  --   to the preview component.
  -- TODO: Which one to use?
  , editorContent :: Maybe (Array String)

  -- Store tocEntries and send some parts to its children components
  -- TODO load/upload from/to backend
  , tocEntries :: Array TOCEntry

  -- How the timestamp has to be formatted
  , timeFormatter:: Maybe Formatter

  -- Boolean flags for UI state
  , sidebarShown :: Boolean
  , tocShown :: Boolean
  , previewShown :: Boolean
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type Slots =
  ( comment :: H.Slot Comment.Query Comment.Output Unit
  , editor :: H.Slot Editor.Query Editor.Output Unit
  , preview :: H.Slot Preview.Query Preview.Output Unit
  , toc :: H.Slot TOC.Query TOC.Output Unit
  )

_comment = Proxy :: Proxy "comment"
_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"
_toc = Proxy :: Proxy "toc"

splitview :: forall query m. MonadAff m => H.Component query Input Output m
splitview = H.mkComponent
  { initialState: \_ ->
      { dragTarget: Nothing
      , startMouseRatio: 0.0
      , startSidebarRatio: 0.0
      , startMiddleRatio: 0.0
      , sidebarRatio: 0.2
      , middleRatio: 0.4
      , lastExpandedSidebarRatio: 0.2
      , lastExpandedMiddleRatio: 0.4
      , editorContent: Nothing
      , tocEntries: []
      , timeFormatter: Nothing
      , sidebarShown: true
      , tocShown: true
      , previewShown: true
      , pdfWarningAvailable: false
      , pdfWarningIsShown: false
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init, handleAction = handleAction }
  }
  where

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ renderToolbar state, renderSplit state ]

  renderToolbar :: State -> H.ComponentHTML Action Slots m
  renderToolbar state =
    -- First Toolbar
    HH.div
      [ HP.classes [ HB.bgDark, HB.overflowAuto, HB.dFlex, HB.flexRow ] ]
      [ HH.button
          [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
          , HE.onClick $ const ToggleSidebar
          ]
          [ HH.text "[≡]" ]
      , HH.span [ HP.classes [ HB.textWhite, HB.px2 ] ] [ HH.text "Toolbar" ]
      , HH.button
          [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
          , HE.onClick $ const ClickedHTTPRequest
          ]
          [ HH.text "Click Me for HTTP request" ]
      , HH.button
          [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
          , HE.onClick $ const SaveSection
          ]
          [ HH.text "Save" ]
      , HH.button
          [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
          , HE.onClick $ const QueryEditor
          ]
          [ HH.text "Query Editor" ]
      , HH.button
          [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
          , HE.onClick $ const ClickLoadPdf
          ]
          [ HH.text "Load PDF" ]
      , if not state.pdfWarningAvailable then HH.div_ []
        else HH.button
          [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
          , HE.onClick $ const ShowWarning
          ]
          [ HH.text
              ((if state.pdfWarningIsShown then "Hide" else "Show") <> " Warning")
          ]
      ]

  renderSplit :: State -> H.ComponentHTML Action Slots m
  renderSplit state =
    HH.div
      [ HE.onMouseMove HandleMouseMove
      , HE.onMouseUp StopResize
      , HP.classes [ HB.dFlex ]
      , HP.style "height: 100vh; position: relative; user-select: none;"
      ]
      ( -- TOC Sidebar
        renderSidebar state
          <>
            [ -- Editor
              HH.div
                [ HP.style $ "position: relative; flex: 0 0 "
                    <> show (state.middleRatio * 100.0)
                    <> "%;"
                ]
                [ -- Floating Button outside to the right of editor container to toggle preview
                  HH.div
                    [ HP.style
                        "position: absolute; top: 50%; right: 0px; margin = outside transform: translateY(-50%); z-index: 10;"
                    ]
                    [ HH.button
                        [ HP.classes [ HB.btn, HB.btnLight, HB.btnSm ]
                        , HE.onClick \_ -> TogglePreview
                        ]
                        [ HH.text if state.previewShown then ">" else "<" ]
                    ]

                -- The actual editor area
                , HH.div
                    [ HP.classes [ HB.dFlex, HB.flexColumn ]
                    , HP.style
                        "height: 100%; box-sizing: border-box; min-height: 0; overflow: hidden;"
                    ]
                    [ HH.slot _editor unit Editor.editor unit HandleEditor ]
                ]
            ]
          <>
            -- Preview Sectioin
            ( if state.previewShown then renderPreview state
              else []
            )
      )

  renderSidebar :: State -> Array (H.ComponentHTML Action Slots m)
  renderSidebar state =
    [ -- Sidebar
      HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0) <>
              "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248);"<>
              if state.sidebarShown && state.tocShown then "" else "display: none;"
        ]
        [ HH.slot _toc unit TOC.tocview unit HandleTOC ]
    , HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0) <>
              "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248);"<>
              if state.sidebarShown && not state.tocShown then "" else "display: none;"
        ]
        [ HH.slot _comment unit Comment.commentview unit HandleComment ]
    -- Left Resizer
    , if state.sidebarShown then
        HH.div
          [ HE.onMouseDown (StartResize ResizeLeft)
          , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
          ]
          []
      else HH.text ""
    ]

  renderPreview :: State -> Array (H.ComponentHTML Action Slots m)
  renderPreview state =
    [ -- Right Resizer
      HH.div
        [ HE.onMouseDown (StartResize ResizeRight)
        , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
        ]
        []

    -- Preview
    , HH.div
        [ HP.classes [ HB.dFlex, HB.flexColumn ]
        , HP.style $
            "flex: 1 1 "
              <> show ((1.0 - state.sidebarRatio - state.middleRatio) * 100.0)
              <>
                "%; box-sizing: border-box; min-height: 0; overflow: hidden; min-width: 6ch;"
        ]
        [ HH.slot _preview unit Preview.preview
            { editorContent: state.editorContent }
            HandlePreview
        ]
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of

    Init -> do
      exampleTOCEntries <- createExampleTOCEntries
      -- -- Comment it out for now, to let the other text show up first in editor
      -- -- head has to be imported from Data.Array
      -- -- Put first entry in editor
      -- --   firstEntry = case head entries of
      -- --     Nothing -> { id: -1, name: "No Entry", content: Just [ "" ] }
      -- --     Just entry -> entry
      -- -- H.tell _editor unit (Editor.ChangeSection firstEntry)
      let timeFormatter = head timeStampsVersions
      H.modify_ \st -> do st { tocEntries = exampleTOCEntries, timeFormatter = timeFormatter }
      H.tell _comment unit (Comment.ReceiveTimeFormatter timeFormatter)
      H.tell _toc unit (TOC.ReceiveTOCs exampleTOCEntries)

    -- Resizing as long as mouse is hold down on window
    -- (Or until the browser detects the mouse is released)
    StartResize which mouse -> do
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width
      H.modify_ \st -> st
        { dragTarget = Just which
        , startMouseRatio = ratioX
        , startSidebarRatio = st.sidebarRatio
        , startMiddleRatio = st.middleRatio
        }

    -- Stop resizing, when mouse is released (is detected by browser)
    StopResize _ ->
      H.modify_ \st -> st { dragTarget = Nothing }

    -- While mouse is hold down, resizer move to position of mouse
    -- (with certain rules)
    HandleMouseMove mouse -> do
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width

        minRatio = 0.05 -- 5%
        maxRatio = 0.7 -- 70%

        clamp :: Number -> Number -> Number -> Number
        clamp minVal maxVal xval = max minVal (min maxVal xval)

      mt <- H.gets _.dragTarget
      mx <- H.gets _.startMouseRatio

      case mt of
        Just ResizeLeft -> do
          s <- H.gets _.startSidebarRatio
          m <- H.gets _.startMiddleRatio
          let
            total = s + m
            rawSidebarRatio = s + (ratioX - mx)
            newSidebar = clamp minRatio 0.2 rawSidebarRatio
            newMiddle = total - newSidebar
          when
            ( newSidebar >= minRatio && newMiddle >= minRatio && newSidebar <=
                maxRatio
            )
            do
              H.modify_ \st -> st
                { sidebarRatio = newSidebar
                , middleRatio = newMiddle
                , lastExpandedSidebarRatio =
                    if newSidebar > minRatio then newSidebar
                    else st.lastExpandedSidebarRatio
                }

        Just ResizeRight -> do
          s <- H.gets _.startSidebarRatio
          m <- H.gets _.startMiddleRatio
          let
            total = 1.0 - s
            rawMiddleRatio = m + (ratioX - mx)
            newMiddle = clamp minRatio 0.7 rawMiddleRatio
            newPreview = total - newMiddle
          when
            (newMiddle >= minRatio && newMiddle <= maxRatio && newPreview >= minRatio)
            do
              H.modify_ \st -> st { middleRatio = newMiddle }

        _ -> pure unit

    -- Toolbar button actions

    ClickedHTTPRequest -> H.tell _preview unit Preview.TellClickedHttpRequest

    SaveSection -> H.tell _editor unit Editor.SaveSection

    QueryEditor -> do
      H.tell _editor unit Editor.SaveSection
      H.tell _editor unit Editor.QueryEditor

    ShowWarning -> do
      H.modify_ \st -> st { pdfWarningIsShown = not st.pdfWarningIsShown }
      H.tell _preview unit Preview.TellShowOrHideWarning

    ClickLoadPdf -> do
      H.modify_ \st -> st { pdfWarningAvailable = true }
      H.tell _editor unit Editor.LoadPdf
      H.tell _preview unit Preview.TellLoadPdf

    -- Toggle actions

    -- Toggle the sidebar
    -- Add logic in calculating the middle ratio
    -- to restore the last expanded middle ratio, when toggling preview back on
    ToggleSidebar -> do
      state <- H.get
      -- close sidebar
      if state.sidebarShown then
        H.modify_ \st -> st
          { sidebarRatio = 0.0
          , middleRatio = st.middleRatio + st.sidebarRatio
          , lastExpandedSidebarRatio = st.sidebarRatio
          , sidebarShown = false
          }
      -- open sidebar
      else do
        H.modify_ \st -> st
          { sidebarRatio = st.lastExpandedSidebarRatio
          , middleRatio = st.middleRatio - st.lastExpandedSidebarRatio
          , sidebarShown = true
          }

    -- Toggle the preview area
    TogglePreview -> do
      state <- H.get
      -- all this, in order for not overlapping the left resizer (to not make it disappear)
      win <- H.liftEffect Web.HTML.window
      totalWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        w = toNumber totalWidth
        resizerWidth = 5.0
        resizerRatio = resizerWidth / w
      -- close preview
      if state.previewShown then
        H.modify_ \st -> st
          { middleRatio = 1.0 - st.sidebarRatio - resizerRatio
          , previewShown = false
          }
      -- open preview
      else do
        -- restore the last expanded middle ratio, when toggling preview back on
        H.modify_ \st -> st
          { middleRatio = st.lastExpandedMiddleRatio
          , previewShown = true
          }

    -- Query handler

    HandleComment output ->  case output of

      Comment.UpdateComment tocID markerID newCommentSection -> do
        state <- H.get

        let
          updatedTOCEntries =
            map (\entry ->
              if entry.id /= tocID then entry
              else let
                newMarkers = case entry.markers of
                  Just markers ->
                    Just (map (\marker ->
                      if marker.id /= markerID then marker
                      else marker { commentSection = Just newCommentSection }
                    ) markers)
                  Nothing -> Nothing
                in
                  entry { markers = newMarkers }
            ) state.tocEntries

        H.modify_ \s -> s { tocEntries = updatedTOCEntries }
        
    HandleEditor output -> case output of

      Editor.ClickedQuery response -> H.tell _preview unit
        (Preview.GotEditorQuery response)

      Editor.SavedSection entry -> 
        H.modify_ \st -> 
          st 
            { tocEntries =
              map (\e -> if e.id == entry.id then entry else e) st.tocEntries
            }

      Editor.SelectedCommentSection tocID markerID commentSection -> do
        H.modify_ \st -> st { tocShown = false }
        H.tell _comment unit (Comment.SelectedCommentSection tocID markerID commentSection)

    HandlePreview _ -> pure unit

    HandleTOC output -> case output of

      TOC.ChangeSection selectEntry -> do
        H.tell _editor unit Editor.SaveSection
        state <- H.get
        let entry = case (findTOCEntry selectEntry.id state.tocEntries) of
              Nothing -> { id: -1, name: "No Entry", content: Nothing, markers: Nothing }
              Just e  -> e
        H.tell _editor unit (Editor.ChangeSection entry)

-- Create example TOC entries for testing purposes in Init

createExampleTOCEntries :: forall m. MonadAff m => H.HalogenM State Action Slots Output m (Array TOCEntry)
createExampleTOCEntries = do
  -- Since all example entries are similar, we create the same markers for all
  exampleMarkers <- createExampleMarkers
  let
    -- Create initial TOC entries
    entries = map
      ( \n ->
          { id: n
          , name: "§" <> show n <> " This is Paragraph " <> show n
          , content: Just $ createExampleTOCText n
          , markers: Just exampleMarkers
          }
      ) (range 1 11)
  pure entries

createExampleTOCText :: Int -> String
createExampleTOCText n =
  intercalate "\n" $
    [ "# This is content of §" <> show n
    , ""
    , "-- This is a developer comment."
    , ""
    , "## To-Do List"
    , ""
    , "1. Document initial setup."
    , "2. <*Define the API*>                        % LTML: bold"
    , "3. <_Underline important interface items_>   % LTML: underline"
    , "4. </Emphasize optional features/>           % LTML: italic"
    , ""
    , "/* Note: Nested styles are allowed,"
    , "   but not transitively within the same tag type!"
    , "   Written in a code block."
    , "*/"
    , ""
    , "<*This is </allowed/>*>                      % valid nesting"
    , "<*This is <*not allowed*>*>                  % invalid, but still highlighted"
    , ""
    , "## Status"
    , ""
    , "Errors can no longer be marked as such, see error!"
    , "Comment this section out of the code."
    , ""
    , "TODO: Write the README file."
    , "FIXME: The parser fails on nested blocks."
    , "NOTE: We're using this style as a placeholder."
    ]

createExampleMarkers :: forall m. MonadAff m => H.HalogenM State Action Slots Output m (Array AnnotatedMarker)
createExampleMarkers = do
  mark <- H.liftEffect $ Range.create 7 3 7 26
  commentSection <- createExampleCommentSection
  let entry = 
        { id: 1
        , type: "info"
        , range: mark
        , startRow: 7
        , startCol: 3
        -- TODO make this a real comment
        , commentSection: Just commentSection
        }
  pure [ entry ]

createExampleCommentSection :: forall m. MonadAff m => H.HalogenM State Action Slots Output m CommentSection
createExampleCommentSection = do
  comments <- createExampleComments
  let
    commentSection = 
      { -- Since in init, all markers have the same ID
        markerID: 1
      , comments: comments
      , resolved: false
      }
  pure commentSection

createExampleComments :: forall m. MonadAff m => H.HalogenM State Action Slots Output m (Array Comment)
createExampleComments = do
  now <- H.liftEffect nowDateTime
  let 
    comments = map
      ( \n ->
          { author: "Author " <> show (mod n 2)
          , timestamp: now
          , content: "This is comment number " <> show n
          }
      )
      (range 1 6)
  pure comments

