-- | It has 3 split views: a sidebar, an editor, and a preview area.
-- | Between each of the views, there are resizers that allow the user to adjust the width
-- | of each section. The sidebar contains a table of contents (TOC) with clickable entries
-- | that jump to specific sections in the editor. The editor allows users to edit content,
-- | and the preview area displays the output based on the editor's content.

module FPO.Component.Splitview where

import Prelude

import Ace.Range as Range
import Ace.Types as Types
import Data.Array (findIndex, intercalate, range, updateAt)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Editor as Editor
import FPO.Components.Preview as Preview
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

type AnnotatedMarker = 
  { id :: Int
  , type :: String
  , range :: Types.Range
  , startRow :: Int
  , startCol :: Int
  , endRow :: Int
  , endColumn :: Int
  }

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe String
  , markers :: Maybe (Array AnnotatedMarker)
  }

type Output = Unit
type Input = Unit
data Query a = UnitQuery a

data Action
  = Init
  -- Resizing Actions
  | StartResize DragTarget MouseEvent
  | StopResize MouseEvent
  | HandleMouseMove MouseEvent
  | JumpToSection TOCEntry
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
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output

type State =
  { dragTarget :: Maybe DragTarget

  -- TOC: Table of Contents
  , tocEntries :: Array TOCEntry
  , slectedTocEntry :: Maybe Int

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

  -- Boolean flags for UI state
  , tocEntriesShown :: Boolean
  , previewShown :: Boolean
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type Slots =
  ( editor :: H.Slot Editor.Query Editor.Output Unit
  , preview :: H.Slot Preview.Query Preview.Output Unit
  )

_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"

splitview :: forall query m. MonadAff m => H.Component query Input Output m
splitview = H.mkComponent
  { initialState: \_ ->
      { dragTarget: Nothing
      , tocEntries: []
      , slectedTocEntry: Nothing
      , startMouseRatio: 0.0
      , startSidebarRatio: 0.0
      , startMiddleRatio: 0.0
      , sidebarRatio: 0.2
      , middleRatio: 0.4
      , lastExpandedSidebarRatio: 0.2
      , lastExpandedMiddleRatio: 0.4
      , editorContent: Nothing
      , tocEntriesShown: true
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
        ( if state.tocEntriesShown then renderSidebar state
          else []
        )
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
              "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248);"
        ]
        [ HH.div_
            ( map
                ( \{ id, name, content, markers } ->
                    HH.div
                      [ HP.title ("Jump to section " <> name)
                      , HP.style
                          "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0;"
                      ]
                      [ HH.span
                          [ HE.onClick \_ -> JumpToSection { id, name, content, markers }
                          , HP.classes
                              ( [ HB.textTruncate ]
                                  <>
                                    if Just id == state.slectedTocEntry then
                                      [ HB.fwBold ]
                                    else []
                              )
                          , HP.style
                              "cursor: pointer; display: inline-block; min-width: 6ch;"
                          ]
                          [ HH.text name ]
                      ]
                )
                state.tocEntries
            )
        ]
    -- Left Resizer
    , HH.div
        [ HE.onMouseDown (StartResize ResizeLeft)
        , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
        ]
        []
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
      -- Since all example entries are similar, we create the same markers for all
      mark <- H.liftEffect $ Range.create 7 3 7 26
      let
        -- Create initial TOC entries
        entries = map
          ( \n ->
              { id: n
              , name: "§" <> show n <> " This is Paragraph " <> show n
              , content: Just
                ( intercalate "\n" $
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
                    , "Errors can already be marked as such, see error!"
                    , ""
                    , "TODO: Write the README file."
                    , "FIXME: The parser fails on nested blocks."
                    , "NOTE: We're using this style as a placeholder."
                    ]
                )
              , markers : Just
                [ { id: 1
                , type: "info"
                , range: mark
                , startRow: 7
                , startCol: 3
                , endRow: 7
                , endColumn: 26 } ]
              }
          )
          (range 1 11)
      -- Comment it out for now, to let the other text show up first in editor
      -- head has to be imported from Data.Array
      -- Put first entry in editor
      --   firstEntry = case head entries of
      --     Nothing -> { id: -1, name: "No Entry", content: Just [ "" ] }
      --     Just entry -> entry
      -- H.tell _editor unit (Editor.ChangeSection firstEntry)
      H.modify_ \st -> do
        st
          { tocEntries = entries
          -- Have not sent the first entry to editor yet. See comment above
          -- , slectedTocEntry = Just firstEntry.id
          , editorContent = Just [ "This is the initial content of the editor." ]
          }

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

    -- Change the content of current § section in the editor
    JumpToSection section -> do
      H.tell _editor unit Editor.SaveSection
      H.tell _editor unit (Editor.ChangeSection section)
      -- TODO add markers
      H.modify_ \st -> st
        { slectedTocEntry = Just section.id
        -- maybe add in later, to automatically update preview, when selecting section
        -- , editorContent = section.content
        }

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
      if state.tocEntriesShown then
        H.modify_ \st -> st
          { sidebarRatio = 0.0
          , middleRatio = st.middleRatio + st.sidebarRatio
          , lastExpandedSidebarRatio = st.sidebarRatio
          , tocEntriesShown = false
          }
      -- open sidebar
      else do
        H.modify_ \st -> st
          { sidebarRatio = st.lastExpandedSidebarRatio
          , middleRatio = st.middleRatio - st.lastExpandedSidebarRatio
          , tocEntriesShown = true
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

    HandleEditor output -> case output of
      Editor.ClickedQuery response -> H.tell _preview unit
        (Preview.GotEditorQuery response)
      Editor.SavedSection section -> do
        state <- H.get
        -- update the TOC entry received from the editor in state
        case findIndex (\e -> e.id == section.id) state.tocEntries of
          Nothing -> pure unit
          Just idx ->
            case updateAt idx section state.tocEntries of
              Nothing -> pure unit
              Just updatedEntries ->
                H.modify_ \st -> st { tocEntries = updatedEntries }

    HandlePreview _ -> pure unit

