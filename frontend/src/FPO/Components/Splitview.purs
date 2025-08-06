-- | It has 3 split views: a sidebar, an editor, and a preview area.
-- | Between each of the views, there are resizers that allow the user to adjust the width
-- | of each section. The sidebar contains a table of contents (TOC) with clickable entries
-- | that jump to specific sections in the editor. The editor allows users to edit content,
-- | and the preview area displays the output based on the editor's content.

module FPO.Component.Splitview where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (find, head, snoc, uncons, updateAt, (!!))
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Comment as Comment
import FPO.Components.CommentOverview as CommentOverview
import FPO.Components.Editor as Editor
import FPO.Components.Preview as Preview
import FPO.Components.TOC as TOC
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.DocumentDto.DocumentTree as DT
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..), findRootTree)
import FPO.Types
  ( CommentSection
  , TOCEntry
  , TOCTree
  , documentTreeToTOCTree
  , emptyTOCEntry
  , findTOCEntry
  , timeStampsVersions
  , tocTreeToDocumentTree
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
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
  | SaveSection
  | RenderHTML
  -- Toggle buttons
  | ToggleComment
  | ToggleCommentOverview Boolean
  | ToggleSidebar
  | TogglePreview
  -- Query Output
  | HandleComment Comment.Output
  | HandleCommentOverview CommentOverview.Output
  | HandleEditor Editor.Output
  | HandlePreview Preview.Output
  | HandleTOC TOC.Output
  | ForceGET
  | GET
  | POST

type State =
  { mDragTarget :: Maybe DragTarget

  -- Store the width values as ratios of the total width
  -- TODO: Using the ratios to keep the ratio, when resizing the window
  --      But how do we get the event of window resize?

  -- Instead of setting the width directly to mouse position, calculate a delta
  -- for a smoother and correct resize experience with the start positions
  , startMouseRatio :: Number
  , startSidebarRatio :: Number
  , startPreviewRatio :: Number

  -- The current widths of the sidebar and middle content (as percentage ratios)
  , sidebarRatio :: Number
  , previewRatio :: Number

  -- The last expanded sidebar width, used to restore the sidebar when toggling
  , lastExpandedSidebarRatio :: Number
  , lastExpandedPreviewRatio :: Number

  -- There are 2 ways to send content to preview:
  -- 1. This editorContent is sent through the slot in renderPreview
  -- 2. Throuth QueryEditor of the Editor, where the editor collects its content and sends it
  --   to the preview component.
  -- TODO: Which one to use?
  , renderedHtml :: Maybe String

  -- Store tocEntries and send some parts to its children components
  , tocEntries :: TOCTree

  -- How the timestamp has to be formatted
  , mTimeFormatter :: Maybe Formatter

  -- Boolean flags for UI state
  , sidebarShown :: Boolean
  , tocShown :: Boolean
  , commentOverviewShown :: Boolean
  , commentShown :: Boolean
  , previewShown :: Boolean
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type Slots =
  ( comment :: H.Slot Comment.Query Comment.Output Unit
  , commentOverview :: H.Slot CommentOverview.Query CommentOverview.Output Unit
  , editor :: H.Slot Editor.Query Editor.Output Unit
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
  => MonadStore Store.Action Store.Store m
  => DocumentID
  -> H.Component query Input Output m
splitview docID = H.mkComponent
  { initialState: \_ ->
      { mDragTarget: Nothing
      , startMouseRatio: 0.0
      , startSidebarRatio: 0.0
      , startPreviewRatio: 0.0
      , sidebarRatio: 0.2
      , previewRatio: 0.4
      , lastExpandedSidebarRatio: 0.2
      , lastExpandedPreviewRatio: 0.4
      , renderedHtml: Nothing
      , tocEntries: Empty
      , mTimeFormatter: Nothing
      , sidebarShown: true
      , tocShown: true
      , commentOverviewShown: false
      , commentShown: false
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
      [ renderToolbar, renderSplit state ]

  renderToolbar :: H.ComponentHTML Action Slots m
  renderToolbar =
    -- First Toolbar
    HH.div
      [ HP.classes [ HB.bgDark, HB.overflowAuto, HB.dFlex, HB.flexRow ] ]
      [ toolbarButton "[=]" ToggleSidebar
      , HH.span [ HP.classes [ HB.textWhite, HB.px2 ] ] [ HH.text "Toolbar" ]
      , toolbarButton "ForceGETP" ForceGET
      , toolbarButton "GET" GET
      , toolbarButton "POST" POST
      , toolbarButton "All Comments" (ToggleCommentOverview true)
      , toolbarButton "Save" SaveSection
      , toolbarButton "Render HTML" RenderHTML
      ]
    where
    toolbarButton label act = HH.button
      [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
      , HE.onClick $ const act
      ]
      [ HH.text label ]

  renderSplit :: State -> H.ComponentHTML Action Slots m
  renderSplit state =
    -- We have to manually shrink the size of those elements, otherwise if they overflow the bottom
    -- of the elements wont be visible anymore
    let
      navbarHeight :: Int
      navbarHeight = 56 -- px, height of the navbar

      toolbarHeight :: Int
      toolbarHeight = 31 -- px, height of the toolbar
    in
      HH.div
        [ HE.onMouseMove HandleMouseMove
        , HE.onMouseUp StopResize
        , HP.classes [ HB.dFlex, HB.overflowHidden ]
        , HP.style
            ( "height: calc(100vh - " <> show (navbarHeight + toolbarHeight) <>
                "px); max-height: 100%; user-select: none"
            )
        ]
        ( -- TOC Sidebar
          renderSidebar state
            <>
              [ -- Editor
                HH.div
                  [ HP.style $ "position: relative; flex: 0 0 "
                      <> show
                        ((1.0 - state.sidebarRatio - state.previewRatio) * 100.0)
                      <> "%;"
                  ]
                  [ -- The actual editor area
                    HH.div
                      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow0 ]
                      , HP.style
                          "height: 100%; box-sizing: border-box; min-height: 0; overflow: hidden;"
                      ]
                      [ HH.slot _editor unit (Editor.editor docID) unit HandleEditor ]
                  ]
              ]
            <>
              -- Preview Sectioin
              renderPreview state
        )

  -- Render both TOC and Comment but make them visable depending of the flags
  -- Always keep them load to not load them over and over again
  renderSidebar :: State -> Array (H.ComponentHTML Action Slots m)
  renderSidebar state =
    [ -- TOC
      HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0)
              <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
              <>
                if
                  state.sidebarShown
                    && not state.commentOverviewShown
                    && not state.commentShown
                    && state.tocShown then
                  ""
                else
                  "display: none;"
        ]
        [ HH.button
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
            , HE.onClick \_ -> ToggleSidebar
            ]
            [ HH.text "×" ]
        , HH.slot _toc unit (TOC.tocview docID) unit HandleTOC
        ]
    -- Comment
    , HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0)
              <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
              <>
                if state.sidebarShown && state.commentShown then
                  ""
                else
                  "display: none;"
        ]
        [ HH.button
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
            , HE.onClick \_ -> ToggleComment
            ]
            [ HH.text "×" ]
        , HH.h4
            [ HP.style
                "margin-top: 0.5rem; margin-bottom: 1rem; margin-left: 0.5rem; font-weight: bold; color: black;"
            ]
            [ HH.text "Conversation" ]
        , HH.slot _comment unit Comment.commentview unit HandleComment
        ]
    -- CommentOverview
    , HH.div
        [ HP.classes [ HB.overflowAuto, HB.p1 ]
        , HP.style $
            "flex: 0 0 " <> show (state.sidebarRatio * 100.0)
              <>
                "%; box-sizing: border-box; min-width: 6ch; background:rgb(229, 241, 248); position: relative;"
              <>
                if
                  state.sidebarShown
                    && not state.commentShown
                    && state.commentOverviewShown then
                  ""
                else
                  "display: none;"
        ]
        [ HH.button
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
            , HE.onClick \_ -> ToggleCommentOverview false
            ]
            [ HH.text "×" ]
        , HH.h4
            [ HP.style
                "margin-top: 0.5rem; margin-bottom: 1rem; margin-left: 0.5rem; font-weight: bold; color: black;"
            ]
            [ HH.text "All comments" ]
        , HH.slot _commentOverview unit CommentOverview.commentOverviewview unit
            HandleCommentOverview
        ]
    -- Left Resizer
    , HH.div
        [ HE.onMouseDown (StartResize ResizeLeft)
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
            , HE.onClick \_ -> ToggleSidebar
            ]
            [ HH.text if state.sidebarShown then "⟨" else "⟩" ]
        ]
    ]

  renderPreview :: State -> Array (H.ComponentHTML Action Slots m)
  renderPreview state =
    [ -- Right Resizer
      HH.div
        [ HE.onMouseDown (StartResize ResizeRight)
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
            , HE.onClick \_ -> TogglePreview
            ]
            [ HH.text if state.previewShown then "⟩" else "⟨" ]
        ]

    -- Preview
    , if state.previewShown then
        HH.div
          [ HP.classes [ HB.dFlex, HB.flexColumn ]
          , HP.style $
              "flex: 1 1 "
                <> show (state.previewRatio * 100.0)
                <>
                  "%; box-sizing: border-box; min-height: 0; overflow: hidden; min-width: 6ch; position: relative;"
          ]
          [ HH.div
              [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
              , HP.style "padding-right: 0.5rem;"
              ]
              [ HH.button
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
                  , HE.onClick \_ -> TogglePreview
                  ]
                  [ HH.text "×" ]
              ]
          , HH.slot _preview unit Preview.preview
              { renderedHtml: state.renderedHtml }
              HandlePreview
          ]
      else
        HH.text ""
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of

    POST -> do
      state <- H.get
      let
        tree = tocTreeToDocumentTree state.tocEntries
        encodedTree = DT.encodeDocumentTree tree

      rep <- H.liftAff $
        Request.postJson ("/docs/" <> show docID <> "/tree") encodedTree
      -- debugging logs in
      case rep of
        Left _ -> pure unit -- H.liftEffect $ Console.log $ Request.printError "post" err
        Right _ -> pure unit
    -- H.liftEffect $ Console.log "Successfully posted TOC to server"
    ForceGET -> do
      -- Forces a GET request to fetch the latest document tree of commit #1.
      fetchedTree <- H.liftAff
        $ Request.getFromJSONEndpoint DT.decodeDocument
        $ "/docs/" <> show docID <> "/tree/latest"
      let
        tree = case fetchedTree of
          Nothing -> Empty
          Just t -> documentTreeToTOCTree t
      H.modify_ \st -> do
        st { tocEntries = tree }
      H.tell _toc unit (TOC.ReceiveTOCs tree)

    GET -> do
      -- TODO: As of now, the editor page and splitview are parametrized by the document ID
      --       as given by the route. We could also handle the docID as an input to the component,
      --       instead, but parameters are more convenient and also there is no existence issue;
      --       in other words, the editor cannot exist with no document ID.
      --
      --       Here, we can simply fetch the latest commit (head commit) of the document and
      --       write the content into the editor. Because requests like these are very common,
      --       we should think of a way to have a uniform and clean request handling system, especially
      --       regarding authentification and error handling. Right now, the editor page is simply empty
      --       if the document retrieval fails in any way.
      finalTree <- fromMaybe Empty <$> runMaybeT do
        fetchedTree <- MaybeT $ H.liftAff
          $ Request.getFromJSONEndpoint DT.decodeDocument
          $ "/docs/" <> show docID <> "/tree/latest"
        pure $ documentTreeToTOCTree fetchedTree

      H.modify_ _ { tocEntries = finalTree }
      H.tell _toc unit (TOC.ReceiveTOCs finalTree)
    Init -> do
      -- exampleTOCEntries <- createExampleTOCEntries
      -- -- Comment it out for now, to let the other text show up first in editor
      -- -- head has to be imported from Data.Array
      -- -- Put first entry in editor
      -- --   firstEntry = case head entries of
      -- --     Nothing -> { id: -1, name: "No Entry", content: Just [ "" ] }
      -- --     Just entry -> entry
      -- -- H.tell _editor unit (Editor.ChangeSection firstEntry)
      let timeFormatter = head timeStampsVersions
      H.modify_ \st -> do
        st { mTimeFormatter = timeFormatter }
      H.tell _comment unit (Comment.ReceiveTimeFormatter timeFormatter)
      H.tell _commentOverview unit
        (CommentOverview.ReceiveTimeFormatter timeFormatter)
      H.tell _toc unit (TOC.ReceiveTOCs Empty)
      -- Load the initial TOC entries into the editor
      -- TODO: Shoult use Get instead, but I (Eddy) don't understand GET
      -- or rather, we don't use commit anymore in the API
      handleAction ForceGET

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
        { mDragTarget = Just which
        , startMouseRatio = ratioX
        , startSidebarRatio = st.sidebarRatio
        , startPreviewRatio = st.previewRatio
        }

    -- Stop resizing, when mouse is released (is detected by browser)
    StopResize _ ->
      H.modify_ \st -> st { mDragTarget = Nothing }

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

      mt <- H.gets _.mDragTarget
      mx <- H.gets _.startMouseRatio

      case mt of
        Just ResizeLeft -> do
          s <- H.gets _.startSidebarRatio
          let
            rawSidebarRatio = s + (ratioX - mx)
            newSidebar = clamp minRatio 0.2 rawSidebarRatio
          when (newSidebar >= minRatio && newSidebar <= maxRatio) do
            H.modify_ \st -> st
              { sidebarRatio = newSidebar
              , lastExpandedSidebarRatio =
                  if newSidebar > minRatio then newSidebar
                  else st.lastExpandedSidebarRatio
              }

        Just ResizeRight -> do
          p <- H.gets _.startPreviewRatio
          s <- H.gets _.sidebarRatio

          let
            delta = ratioX - mx
            rawPreview = p - delta
            maxPreview = 1.0 - s - minRatio
            newPreview = clamp minRatio maxPreview rawPreview

          when (newPreview >= minRatio && newPreview <= maxPreview) do
            H.modify_ \st -> st
              { previewRatio = newPreview
              , lastExpandedPreviewRatio =
                  if newPreview > minRatio then newPreview
                  else st.lastExpandedPreviewRatio
              }

        _ -> pure unit

    -- Toolbar button actions

    SaveSection -> H.tell _editor unit Editor.SaveSection

    RenderHTML -> do
      H.tell _editor unit Editor.SaveSection
      H.tell _editor unit Editor.QueryEditor

    -- Toggle actions

    ToggleComment -> H.modify_ \st -> st { commentShown = false }

    ToggleCommentOverview shown ->
      if shown then do
        H.tell _editor unit Editor.SendCommentSections
        H.modify_ \st -> st { commentShown = false, commentOverviewShown = shown }
      else
        H.modify_ \st -> st { commentOverviewShown = shown }

    -- Toggle the sidebar
    -- Add logic in calculating the middle ratio
    -- to restore the last expanded middle ratio, when toggling preview back on
    ToggleSidebar -> do
      state <- H.get
      -- close sidebar
      if state.sidebarShown then
        H.modify_ \st -> st
          { sidebarRatio = 0.0
          , lastExpandedSidebarRatio = st.sidebarRatio
          , sidebarShown = false
          }
      -- open sidebar
      else do
        H.modify_ \st -> st
          { sidebarRatio = st.lastExpandedSidebarRatio
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
        -- resizer size is 8, but there are 2 resizers.
        -- Also resizer size is not in sidebarRatio
        resizerWidth = 16.0
        resizerRatio = resizerWidth / w
      -- close preview
      if state.previewShown then
        H.modify_ \st -> st
          { previewRatio = resizerRatio
          , lastExpandedPreviewRatio = st.previewRatio
          , previewShown = false
          }
      -- open preview
      else do
        -- restore the last expanded middle ratio, when toggling preview back on
        H.modify_ \st -> st
          { previewRatio = st.lastExpandedPreviewRatio
          , previewShown = true
          }

    -- Query handler

    HandleComment output -> case output of

      Comment.CloseCommentSection -> do
        H.modify_ \st -> st { commentShown = false }

      Comment.UpdateComment tocID markerID newCommentSection -> do
        H.tell _editor unit Editor.SaveSection
        state <- H.get
        let
          updatedTOCEntries = map
            ( \entry ->
                if entry.id /= tocID then entry
                else
                  let
                    newMarkers =
                      ( map
                          ( \marker ->
                              if marker.id /= markerID then marker
                              else marker { mCommentSection = Just newCommentSection }
                          )
                          entry.markers
                      )
                  in
                    entry { markers = newMarkers }
            )
            state.tocEntries
          updateTOCEntry = fromMaybe
            emptyTOCEntry
            (findRootTree (\e -> e.id == tocID) updatedTOCEntries)
        H.modify_ \s -> s { tocEntries = updatedTOCEntries }
        H.tell _editor unit (Editor.ChangeSection updateTOCEntry)

    HandleCommentOverview output -> case output of

      CommentOverview.JumpToCommentSection tocID markerID commentSection -> do
        H.modify_ \st -> st { commentShown = true }
        H.tell _comment unit
          (Comment.SelectedCommentSection tocID markerID commentSection)

    HandleEditor output -> case output of

      Editor.ClickedQuery response -> do
        renderedHtml' <- H.liftAff $ Request.postRenderHtml (joinWith "" response)
        case renderedHtml' of
          Left _ -> pure unit -- Handle error
          Right { body } -> do
            H.modify_ \st -> st { renderedHtml = Just body }

      Editor.DeletedComment tocEntry deletedIDs -> do
        H.modify_ \st ->
          st
            { tocEntries =
                map (\e -> if e.id == tocEntry.id then tocEntry else e) st.tocEntries
            }
        H.tell _comment unit (Comment.DeletedComment tocEntry.id deletedIDs)

      Editor.SavedSection tocEntry ->
        H.modify_ \st ->
          st
            { tocEntries =
                map (\e -> if e.id == tocEntry.id then tocEntry else e) st.tocEntries
            }

      Editor.SelectedCommentSection tocID markerID -> do
        state <- H.get
        if state.sidebarShown then
          H.modify_ \st -> st { commentShown = true }
        else
          H.modify_ \st -> st
            { sidebarRatio = st.lastExpandedSidebarRatio
            , sidebarShown = true
            , commentShown = true
            }
        case (findCommentSection state.tocEntries tocID markerID) of
          Nothing -> pure unit
          Just commentSection -> do
            H.tell _comment unit
              (Comment.SelectedCommentSection tocID markerID commentSection)

      Editor.SendingTOC tocEntry -> do
        H.tell _commentOverview unit (CommentOverview.ReceiveTOC tocEntry)
    HandlePreview _ -> pure unit

    HandleTOC output -> case output of

      TOC.ChangeSection selectedId -> do
        H.tell _editor unit Editor.SaveSection
        state <- H.get
        let
          entry = case (findTOCEntry selectedId state.tocEntries) of
            Nothing -> emptyTOCEntry
            Just e -> e
        H.tell _editor unit (Editor.ChangeSection entry)

      TOC.AddNode path node -> do
        state <- H.get
        let
          newTree = addRootNode path node state.tocEntries
          docTree = tocTreeToDocumentTree newTree
          encodeTree = DT.encodeDocumentTree docTree
        _ <- H.liftAff $
          Request.postJson ("/docs/" <> show docID <> "/tree") encodeTree
        H.modify_ \st -> st { tocEntries = newTree }
        H.tell _toc unit (TOC.ReceiveTOCs newTree)

        pure unit

findCommentSection :: TOCTree -> Int -> Int -> Maybe CommentSection
findCommentSection tocEntries tocID markerID = do
  tocEntry <- findRootTree (\entry -> entry.id == tocID) tocEntries
  marker <- find (\m -> m.id == markerID) tocEntry.markers
  marker.mCommentSection

-- Add a node in TOC tree
addRootNode
  :: Array Int
  -> Tree TOCEntry
  -> TOCTree
  -> TOCTree
addRootNode [] entry (RootTree { children, header }) =
  RootTree { children: snoc children (Edge entry), header }
addRootNode _ entry Empty =
  RootTree
    { children: [ Edge entry ], header: { headerKind: "root", headerType: "root" } }
addRootNode path entry (RootTree { children, header }) =
  case uncons path of
    Nothing ->
      RootTree { children: snoc children (Edge entry), header }
    Just { head, tail } ->
      let
        child =
          fromMaybe
            (Edge (Leaf { title: "Error", node: emptyTOCEntry }))
            (children !! head)
        newChildren =
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        RootTree { children: newChildren, header }

addNode
  :: Array Int
  -> Tree TOCEntry
  -> Edge TOCEntry
  -> Edge TOCEntry
addNode _ _ (Edge (Leaf { title, node })) =
  Edge (Leaf { title, node }) -- Cannot add to a leaf
addNode [] entry (Edge (Node { title, children, header })) =
  Edge (Node { title, children: snoc children (Edge entry), header })
addNode path entry (Edge (Node { title, children, header })) =
  case uncons path of
    Nothing ->
      Edge (Node { title, children: snoc children (Edge entry), header })
    Just { head, tail } ->
      let
        child =
          fromMaybe
            (Edge (Leaf { title: "Error", node: emptyTOCEntry }))
            (children !! head)
        newChildren' =
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        Edge (Node { title, children: newChildren', header })
