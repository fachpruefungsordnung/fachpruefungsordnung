module FPO.Component.Splitview where

import Prelude

import Data.Array (head, range)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Editor as Editor
import Components.Preview (Output, Query(TellClickedHttpRequest, GotEditorQuery, TellLoadPdf, TellLoadUploadedPdf, TellShowOrHideWarning), preview) as Preview
import Web.UIEvent.MouseEvent (MouseEvent, clientX)
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))

data DragTarget = ResizeLeft | ResizeRight
derive instance eqDragTarget :: Eq DragTarget

type TOCEntry = 
  { id :: Int
  , name :: String
  , content :: String
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
  | ToggleSidebar
  | JumpToSection String
  -- Toolbar buttons
  | ClickedHTTPRequest
  | QueryEditor
  | ClickLoadPdf
  | ShowWarning
  -- Query Output
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output

type State =
  { dragTarget :: Maybe DragTarget

  -- TOC: Table of Contents
  , tocEntries :: Array TOCEntry

  -- Store the width values as ratios of the total width
  -- TODO: Using the ratios to keep the ratio, when resizing the window

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

  , editorText :: String
  , editorContent :: Maybe (Array String)
  , hasResizedSidebar :: Boolean
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
      , startMouseRatio: 0.0
      , startSidebarRatio: 0.0
      , startMiddleRatio: 0.0
      , sidebarRatio: 0.2
      , middleRatio: 0.4
      , lastExpandedSidebarRatio: 0.2
      , editorText: "Nothing."
      , editorContent: Nothing
      , hasResizedSidebar: false
      , pdfWarningAvailable: false
      , pdfWarningIsShown: false
      }
  , render
  , eval: H.mkEval $ H.defaultEval { initialize = Just Init, handleAction = handleAction }
  }
  where

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      [ 
        HH.div 
          [ HP.classes [ HB.bgDark, HB.overflowAuto, HB.dFlex, HB.flexRow ] ]
          [ HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const ToggleSidebar ] [ HH.text "[≡]" ]
          , HH.span [ HP.classes [ HB.textWhite, HB.px2 ] ] [ HH.text "Toolbar" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const ClickedHTTPRequest ] [ HH.text "Click Me for HTTP request" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const QueryEditor ] [ HH.text "Query Editor" ]
          , HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const ClickLoadPdf ] [ HH.text "Load PDF" ]
          , if not state.pdfWarningAvailable then HH.div_ []
            else HH.button [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ], HE.onClick $ const ShowWarning ]
              [ HH.text ((if state.pdfWarningIsShown then "Hide" else "Show") <> " Warning") ]
          ]
      ,renderSplit state
      ]

  renderSplit :: State -> H.ComponentHTML Action Slots m
  renderSplit state =
    HH.div
      [ HE.onMouseMove HandleMouseMove
      , HE.onMouseUp StopResize
      , HP.classes [ HB.dFlex ]
      , HP.style "height: 100vh; position: relative; user-select: none;"
      ]
      [ -- Sidebar
        HH.div
          [ HP.classes [ HB.overflowAuto, HB.p1 ]
          , HP.style $
              "flex: 0 0 " <> show (state.sidebarRatio * 100.0) <> "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248);"
          ]
          [ HH.div_
              ( map
                  (\{ id, name, content } ->
                    HH.div
                      [ HP.title ("Jump to section " <> name)
                      , HP.style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0;"
                      ]
                      [ HH.span
                          [ HE.onClick \_ -> JumpToSection content
                          , HP.classes [ HB.textTruncate ]
                          , HP.style "cursor: pointer; display: inline-block; min-width: 6ch;"
                          ]
                          [ HH.text name ]
                      ]
                  )
                  state.tocEntries
              )
          ]

      -- Resizer
      , HH.div
          [ HE.onMouseDown (StartResize ResizeLeft)
          , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
          ]
          []

      -- Editor
      , HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 0 0 " <> show (state.middleRatio * 100.0) <> "%; box-sizing: border-box; min-height: 0; overflow: hidden;"
          ]
          [ HH.slot _editor unit Editor.editor { editorText: state.editorText } HandleEditor ]

      -- Resizer
      , HH.div
          [ HE.onMouseDown (StartResize ResizeRight)
          , HP.style "width: 5px; cursor: col-resize; background: #ccc;"
          ]
          []

      -- Preview
      , HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 1 1 " <> show ((1.0 - state.sidebarRatio - state.middleRatio) * 100.0) <> "%; box-sizing: border-box; min-height: 0; overflow: hidden;" 
          ]
          [ HH.slot _preview unit Preview.preview { editorContent: state.editorContent } HandlePreview ]
      ]



  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of

    Init -> do
      let entries = map (\n -> {id: n, name: "§" <> show n <> " This is Paragraph " <> show n, content: "This is the content of §" <> show n} ) (range 1 11)
      H.modify_ \st -> do
        st  { tocEntries = entries
            , editorText = case head entries of
                Just entry -> entry.content
                Nothing -> "Nothing"
            , editorContent = Just ["This is the initial content of the editor."]
            }

    ToggleSidebar -> do
      win <- H.liftEffect Web.HTML.window
      width <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let w = toNumber width
      state <- H.get
      if state.sidebarRatio * w <= 85.0 then do
        let
          target = state.lastExpandedSidebarRatio
          delta = target - (85.0 / w)
        H.modify_ \st -> st
          { sidebarRatio = target
          , middleRatio = max 0.1 (st.middleRatio - delta)
          }
      else do
        let
          newSidebar = 85.0 / w
          delta = state.sidebarRatio - newSidebar
        H.modify_ \st -> st
          { sidebarRatio = newSidebar
          , middleRatio = max 0.1 (st.middleRatio + delta)
          , lastExpandedSidebarRatio = if st.hasResizedSidebar then st.lastExpandedSidebarRatio else st.sidebarRatio
          }

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

    StopResize _ ->
      H.modify_ \st -> st
        { dragTarget = Nothing
        , hasResizedSidebar = true
        }

    HandleMouseMove mouse -> do
      win <- H.liftEffect Web.HTML.window
      intWidth <- H.liftEffect $ Web.HTML.Window.innerWidth win
      let
        x = toNumber $ clientX mouse
        width = toNumber intWidth
        ratioX = x / width
      mt <- H.gets _.dragTarget
      mx <- H.gets _.startMouseRatio
      case mt of
        Just ResizeLeft -> do
          s <- H.gets _.startSidebarRatio
          m <- H.gets _.startMiddleRatio
          let
            rawSidebarRatio = s + (ratioX - mx)
            maxSidebarRatio = s + m - (85.0 / width)
            -- enforce minimum width to show at least ~6 characters
            clampedSidebarRatio = max (70.0 / width) (min maxSidebarRatio rawSidebarRatio)
            newSidebar = clampedSidebarRatio
            newMiddle = s + m - clampedSidebarRatio
          when (newMiddle * width >= 85.0) do
            H.modify_ \st -> st
              { sidebarRatio = newSidebar
              , middleRatio = newMiddle
              , lastExpandedSidebarRatio = if newSidebar * width > 85.0 then newSidebar else st.lastExpandedSidebarRatio
              , hasResizedSidebar = true
              }

        Just ResizeRight -> do
          m <- H.gets _.startMiddleRatio
          let
            newMiddleRatio = max 0.1 (m + (ratioX - mx))
          H.modify_ \st -> st { middleRatio = newMiddleRatio }

        _ -> pure unit

    JumpToSection section -> H.tell _editor unit (Editor.ChangeSection section)

    -- Toolbar button actions

    ClickedHTTPRequest -> H.tell _preview unit Preview.TellClickedHttpRequest

    QueryEditor -> H.tell _editor unit Editor.QueryEditor

    ShowWarning -> do
      H.modify_ \st -> st { pdfWarningIsShown = not st.pdfWarningIsShown }
      H.tell _preview unit Preview.TellShowOrHideWarning

    ClickLoadPdf -> do
      H.modify_ \st -> st { pdfWarningAvailable = true }
      H.tell _editor unit Editor.LoadPdf
      H.tell _preview unit Preview.TellLoadPdf

    -- Query handler

    HandleEditor output -> case output of
      Editor.ClickedQuery response -> H.tell _preview unit (Preview.GotEditorQuery response)
      Editor.SendPDF mURL -> H.tell _preview unit (Preview.TellLoadUploadedPdf mURL)

    HandlePreview _ -> pure unit


