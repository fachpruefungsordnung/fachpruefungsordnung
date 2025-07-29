module FPO.Components.Editor
  ( Action(..)
  , Output(..)
  , Query(..)
  , State
  , LiveMarker
  , _pdfSlideBar
  , addAnnotation
  , addChangeListener
  , editor
  , findAllIndicesOf
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
import Data.Array (catMaybes, filter, intercalate, uncons, (:))
import Data.Foldable (find, for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FPO.Components.Editor.Keybindings
  ( keyBinding
  , makeBold
  , makeItalic
  , underscore
  )
import FPO.Data.Request (getUser)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types (AnnotatedMarker, TOCEntry, markerToAnnotation, sortMarkers)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, ref, style, title) as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(Proxy))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (toEventTarget)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLElement (toElement)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type State = FPOState
  ( mEditor :: Maybe Types.Editor
  , mTocEntry :: Maybe TOCEntry
  , liveMarkers :: Array LiveMarker
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  , fontSize :: Int
  )

-- For tracking the comment markers live
-- Only store the values in save
type LiveMarker =
  { annotedMarkerID :: Int
  , startAnchor :: Types.Anchor
  , endAnchor :: Types.Anchor
  , ref :: Ref Int
  }

_pdfSlideBar = Proxy :: Proxy "pdfSlideBar"

data Output
  = ClickedQuery (Maybe (Array String))
  | DeletedComment TOCEntry (Array Int)
  | SavedSection TOCEntry
  | SelectedCommentSection Int Int
  | SendingTOC TOCEntry

data Action
  = Init
  | Comment
  | DeleteComment
  | ShowWarning
  | SelectComment
  | Bold
  | Italic
  | Underline
  | FontSizeUp
  | FontSizeDown
  | Undo
  | Redo
  | Receive (Connected FPOTranslator Unit)

-- We use a query to get the content of the editor
data Query a
  -- = RequestContent (Array String -> a)
  = QueryEditor a
  -- save the current content and send it to splitview
  | SaveSection a
  | LoadPdf a
  -- receive the selected TOC and put its content into the editor
  | ChangeSection TOCEntry a
  | SendCommentSections a

editor
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Unit Output m
editor = connect selectTranslator $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { translator: fromFpoTranslator context
    , mEditor: Nothing
    , liveMarkers: []
    , mTocEntry: Nothing
    , pdfWarningAvailable: false
    , pdfWarningIsShown: false
    , fontSize: 12
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HE.onClick $ const SelectComment
      , HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ]
      ]
      [ HH.div -- Second toolbar

          [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title (translate (label :: _ "editor_textBold") state.translator)
              , HE.onClick \_ -> Bold
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-type-bold" ] ] [] ]
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title (translate (label :: _ "editor_textItalic") state.translator)
              , HE.onClick \_ -> Italic
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-type-italic" ] ] [] ]
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_textUnderline") state.translator)
              , HE.onClick \_ -> Underline
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-type-underline" ] ] [] ]
          , HH.div
              [ HP.classes [ HB.vr, HB.mx1 ]
              , HP.style "height: 1.5rem"
              ]
              []
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_fontSizeUp") state.translator)
              , HE.onClick \_ -> FontSizeUp
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-plus-square" ] ] [] ]
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_fontSizeDown") state.translator)
              , HE.onClick \_ -> FontSizeDown
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-dash-square" ] ] [] ]
          , HH.div
              [ HP.classes [ HB.vr, HB.mx1 ]
              , HP.style "height: 1.5rem"
              ]
              []
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_undo") state.translator)
              , HE.onClick \_ -> Undo
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-arrow-counterclockwise" ] ]
                  []
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_redo") state.translator)
              , HE.onClick \_ -> Redo
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-arrow-clockwise" ] ] [] ]
          , HH.div
              [ HP.classes [ HB.vr, HB.mx1 ]
              , HP.style "height: 1.5rem"
              ]
              []
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_comment") state.translator)
              , HE.onClick \_ -> Comment
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-chat-square-text" ] ]
                  []
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
              , HP.title
                  (translate (label :: _ "editor_deleteComment") state.translator)
              , HE.onClick \_ -> DeleteComment
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-chat-square-text-fill" ] ]
                  []
              ]
          ]
      , HH.div -- Editor container

          [ HP.ref (H.RefLabel "container")
          , HP.classes [ HB.flexGrow1 ]
          , HP.style "min-height: 0"
          ]
          []
      ]

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Init -> do
      state <- H.get
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
        editor_ <- H.liftEffect $ Ace.editNode el Ace.ace

        H.modify_ _ { mEditor = Just editor_ }

        H.liftEffect $ do
          Editor.setFontSize (show state.fontSize <> "px") editor_
          eventListen <- eventListener (keyBinding editor_)
          container <- Editor.getContainer editor_
          addEventListener keydown eventListen true
            (toEventTarget $ toElement container)
          session <- Editor.getSession editor_
          document <- Session.getDocument session

          -- Set the editor's theme and mode
          Editor.setTheme "ace/theme/github" editor_
          Session.setMode "ace/mode/custom_mode" session
          Editor.setEnableLiveAutocompletion true editor_

          -- Add a change listener to the editor
          addChangeListener editor_

          -- Add some example text
          Document.setValue "" document

    Bold -> do
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ do
          makeBold ed
          Editor.focus ed

    Italic -> do
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ do
          makeItalic ed
          Editor.focus ed

    Underline -> do
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ do
          underscore ed
          Editor.focus ed

    FontSizeUp -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        let newSize = state.fontSize + 2
        H.modify_ \st -> st { fontSize = newSize }
        -- Set the new font size in the editor
        H.liftEffect $ do
          Editor.setFontSize (show newSize <> "px") ed
          Editor.focus ed

    FontSizeDown -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        let newSize = state.fontSize - 2
        H.modify_ \st -> st { fontSize = newSize }
        -- Set the new font size in the editor
        H.liftEffect $ do
          Editor.setFontSize (show newSize <> "px") ed
          Editor.focus ed

    Undo -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        H.liftEffect $ do
          Editor.undo ed
          Editor.focus ed

    Redo -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        H.liftEffect $ do
          Editor.redo ed
          Editor.focus ed

    Comment -> do
      user <- H.liftAff getUser
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        session <- H.liftEffect $ Editor.getSession ed
        range <- H.liftEffect $ Editor.getSelectionRange ed
        start <- H.liftEffect $ Range.getStart range
        end <- H.liftEffect $ Range.getEnd range

        let
          sRow = Types.getRow start
          sCol = Types.getColumn start
          eRow = Types.getRow end
          eCol = Types.getColumn end
          userName = maybe "Guest" _.userName user
          newMarkerID = case state.mTocEntry of
            Nothing -> 0
            Just tocEntry -> tocEntry.newMarkerNextID
          newCommentSection =
            { markerID: newMarkerID
            , comments: []
            , resolved: false
            }
          newMarker =
            { id: newMarkerID
            , type: "info"
            , startRow: sRow
            , startCol: sCol
            , endRow: eRow
            , endCol: eCol
            , markerText: userName
            , mCommentSection: Just newCommentSection
            }

        mLiveMarker <- H.liftEffect $ addAnchor newMarker session

        let
          newLiveMarkers = case mLiveMarker of
            Nothing -> state.liveMarkers
            Just lm -> lm : state.liveMarkers

        case state.mTocEntry of
          Just entry -> do
            let
              newEntry =
                { id: entry.id
                , name: entry.name
                , newMarkerNextID: entry.newMarkerNextID + 1
                , markers: sortMarkers (newMarker : entry.markers)
                }
            H.modify_ \st ->
              st
                { mTocEntry = Just newEntry
                , liveMarkers = newLiveMarkers
                }
            H.raise (SavedSection newEntry)
            H.raise
              (SelectedCommentSection entry.id newMarker.id)
          Nothing -> pure unit

    DeleteComment -> do
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        case state.mTocEntry of
          Nothing -> pure unit
          Just tocEntry -> do
            session <- H.liftEffect $ Editor.getSession ed
            cursor <- H.liftEffect $ Editor.getCursorPosition ed

            -- remove the marker at the cursor position and return the remaining markers
            foundLM <- H.liftEffect $ cursorInRange state.liveMarkers cursor
            case foundLM of
              Nothing -> pure unit
              Just lm -> do
                let
                  foundID = lm.annotedMarkerID
                  newMarkers = filter (\m -> not (m.id == foundID)) tocEntry.markers
                  newLiveMarkers =
                    filter (\l -> not (l.annotedMarkerID == foundID))
                      state.liveMarkers
                  newTOCEntry = tocEntry { markers = newMarkers }
                H.liftEffect $ removeLiveMarker lm session
                H.modify_ \st ->
                  st { mTocEntry = Just newTOCEntry, liveMarkers = newLiveMarkers }
                H.raise (DeletedComment newTOCEntry [ foundID ])

    ShowWarning -> do
      H.modify_ \state -> state { pdfWarningIsShown = not state.pdfWarningIsShown }

    SelectComment -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        cursor <- H.liftEffect $ Editor.getCursorPosition ed
        foundLM <- H.liftEffect $ cursorInRange state.liveMarkers cursor
        let
          foundID = case foundLM of
            Nothing -> -1
            Just lm -> lm.annotedMarkerID

          -- extract markers from the current TOC entry
          tocEntry = case state.mTocEntry of
            Nothing ->
              { id: -1
              , name: "No entry"
              , newMarkerNextID: -1
              , markers: []
              }
            Just e -> e
          markers = tocEntry.markers

        when (foundID >= 0)
          case (find (\m -> m.id == foundID) markers) of
            Nothing -> pure unit
            Just foundMarker -> H.raise
              (SelectedCommentSection tocEntry.id foundMarker.id)

    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ChangeSection entry a -> do
      H.modify_ \state -> state { mTocEntry = Just entry }

      -- Put the content of the section into the editor and update markers
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        newLiveMarkers <- H.liftEffect do
          session <- Editor.getSession ed
          document <- Session.getDocument session

          -- Set editor content
          let content = "TODO: get content from API"
          Document.setValue content document

          -- Reset Undo history
          undoMgr <- Session.getUndoManager session
          UndoMgr.reset undoMgr

          -- Remove existing markers
          for_ state.liveMarkers \lm -> do
            removeLiveMarker lm session
          -- existingMarkers <- Session.getMarkers session
          -- for_ existingMarkers \marker -> do
          --   id <- Marker.getId marker
          --   Session.removeMarker id session

          -- Clear annotations
          Session.clearAnnotations session

          -- Add annotations from marker
          tmp <- for (entry.markers) \marker -> do
            addAnchor marker session

          pure (catMaybes tmp)

        -- Update state with new marker IDs
        H.modify_ \st ->
          st { liveMarkers = newLiveMarkers }

      pure (Just a)

    LoadPdf a -> do
      H.modify_ _ { pdfWarningAvailable = true }
      pure (Just a)

    SaveSection a -> do
      state <- H.get
      allLines <- H.gets _.mEditor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines

      -- Save the current content of the editor
      let
        contentText = case allLines of
          Just ls -> intercalate "\n" ls
          Nothing -> ""

        -- extract the current TOC entry
        entry = case state.mTocEntry of
          Nothing ->
            { id: -1
            , name: "Section not found"
            , newMarkerNextID: -1
            , markers: []
            }
          Just e -> e

      -- Since the ids and postions in liveMarkers are changing constantly, 
      -- extract them now and store them 
      updatedMarkers <- H.liftEffect do
        for entry.markers \m -> do
          case find (\lm -> lm.annotedMarkerID == m.id) state.liveMarkers of
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
      let
        newEntry = entry
          { markers = updatedMarkers }
      
      --TODO send the content (contentText) as POST to the server

      H.modify_ \st -> st { mTocEntry = Just newEntry }
      H.raise (SavedSection newEntry)
      pure (Just a)

    -- Because Session does not provide a way to get all lines directly,
    -- we need to take another indirect route to get the lines.
    -- Notice that this extra step is not needed for all js calls.
    -- For example, `Session.getLine` can be called directly.
    QueryEditor a -> do
      allLines <- H.gets _.mEditor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines
      H.raise (ClickedQuery allLines)
      pure (Just a)

    SendCommentSections a -> do
      state <- H.get
      -- Send the current comment sections to the splitview
      case state.mTocEntry of
        Nothing -> pure unit
        Just tocEntry -> do
          H.raise (SendingTOC tocEntry)
      pure (Just a)

-- | Change listener for the editor.
--
--   This function should implement stuff like parsing and syntax analysis,
--   linting, code completion, etc.
--   For now, it puts "  " in front of "#", if it is placed at the
--   beginning of a line
addChangeListener :: Types.Editor -> Effect Unit
addChangeListener editor_ = do
  session <- Editor.getSession editor_
  -- Setup change listener to react to changes in the editor
  Session.onChange session \(Types.DocumentEvent { action, start, end: _, lines }) ->
    do
      -- Types.showDocumentEventType action does not work and using case of
      -- data DocumentEventType = Insert | Remove does also not work
      when ((unsafeCoerce action :: String) == "insert") do
        let
          sCol = Types.getColumn start
        when (sCol == 0 && lines == [ "#" ]) do
          let
            sRow = Types.getRow start
          range <- Range.create sRow sCol sRow (sCol + 1)
          Session.replace range "  #" session

-- | Helper function to find all indices of a substring in a string
--   in reverse order.
--
--   Naive and slow pattern matching, but it works for small strings and is
--   good enough for our marker placement experiments.
findAllIndicesOf :: String -> String -> Array Int
findAllIndicesOf needle haystack = go 0 []
  where
  go startIndex acc =
    case String.indexOf (String.Pattern needle) (String.drop startIndex haystack) of
      Just relativeIndex ->
        let
          absoluteIndex = startIndex + relativeIndex
        in
          go (absoluteIndex + 1) (absoluteIndex : acc)
      Nothing -> acc

-- | Adds an annotation to the editor session.
addAnnotation
  :: Types.Annotation
  -> Types.EditSession
  -> Effect Unit
addAnnotation annotation session = do
  anns <- Session.getAnnotations session
  Session.setAnnotations (annotation : anns) session

addAnchor :: AnnotatedMarker -> Types.EditSession -> Effect (Maybe LiveMarker)
addAnchor marker session =
  if (marker.startRow == marker.endRow && marker.startCol == marker.endCol) then
    pure Nothing -- No valid range, so no marker
  else do
    document <- Session.getDocument session
    startAnchor <- Document.createAnchor marker.startRow marker.startCol document
    endAnchor <- Document.createAnchor marker.endRow marker.endCol document
    Anchor.setInsertRight true endAnchor

    range <- createMarkerRange marker
    id <- Session.addMarker range "my-marker" "string" false session
    markerRef <- Ref.new id

    let
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
          "string"
          false
          session

        Ref.write newId markerRef
        pure unit

    Anchor.onChange startAnchor rerenderMarker
    Anchor.onChange endAnchor rerenderMarker
    addAnnotation (markerToAnnotation marker) session
    pure
      ( Just
          { annotedMarkerID: marker.id
          , startAnchor: startAnchor
          , endAnchor: endAnchor
          , ref: markerRef
          }
      )

removeLiveMarker :: LiveMarker -> Types.EditSession -> Effect Unit
removeLiveMarker lm session = do
  -- Marker entfernen
  markerId <- Ref.read lm.ref
  Session.removeMarker markerId session

  -- Anchors vom Dokument lösen
  Anchor.detach lm.startAnchor
  Anchor.detach lm.endAnchor

createMarkerRange :: AnnotatedMarker -> Effect Types.Range
createMarkerRange marker = do
  range <- Range.create marker.startRow marker.startCol marker.endRow marker.endCol
  pure range

-- Gets all markers from this session. Then check, if the Position is in 
-- range one of the markers. Because the markers are sorted by start Position
-- we can use the 
--findLocalMarkerID

cursorInRange :: Array LiveMarker -> Types.Position -> Effect (Maybe LiveMarker)
cursorInRange [] _ = pure Nothing
cursorInRange lms cursor =
  case uncons lms of
    Just { head: l, tail: ls } -> do
      start <- Anchor.getPosition l.startAnchor
      end <- Anchor.getPosition l.endAnchor
      range <- Range.create
        (Types.getRow start)
        (Types.getColumn start)
        (Types.getRow end)
        (Types.getColumn end)
      found <- Range.contains
        (Types.getRow cursor)
        (Types.getColumn cursor)
        range
      if found then
        pure (Just l)
      else
        cursorInRange ls cursor
    Nothing -> pure Nothing
