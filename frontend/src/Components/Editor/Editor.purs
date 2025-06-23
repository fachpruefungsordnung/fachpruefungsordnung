module FPO.Components.Editor
  ( Action(..)
  , Output(..)
  , Query(..)
  , State
  , _pdfSlideBar
  , addAnnotation
  , addChangeListener
  , editor
  , findAllIndicesOf
  ) where

import Prelude

import Ace (ace, editNode) as Ace
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Marker as Marker
import Ace.Range as Range
import Ace.Types as Types
import Components.Editor.Keybindings (keyBinding, makeBold, makeItalic, underscore)
import Data.Array (filter, filterA, head, intercalate, (..), (:))
import Data.Array as Array
import Data.Foldable (elem, for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( AnnotatedMarker
  , CommentSection
  , TOCEntry
  , markerToAnnotation
  , sortMarkers
  )
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
import Web.DOM.Element (toEventTarget)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLElement (toElement)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type State = FPOState
  ( mEditor :: Maybe Types.Editor
  , mTocEntry :: Maybe TOCEntry
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  )

_pdfSlideBar = Proxy :: Proxy "pdfSlideBar"

data Output
  = ClickedQuery (Maybe (Array String))
  | DeletedComment TOCEntry (Array Int)
  | SavedSection TOCEntry
  | SelectedCommentSection Int Int CommentSection

data Action
  = Init
  | Comment
  | DeleteComment
  | ShowWarning
  | SelectComment
  | Bold
  | Italic
  | Underline
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

editor
  :: forall m
   . MonadEffect m
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
    , mTocEntry: Nothing
    , pdfWarningAvailable: false
    , pdfWarningIsShown: false
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
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> Comment
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-x-lg" ] ] []
              , HH.text " Comment"
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> DeleteComment
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-x-lg" ] ] []
              , HH.text "Delete Comment"
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
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
        editor_ <- H.liftEffect $ Ace.editNode el Ace.ace
        H.modify_ _ { mEditor = Just editor_ }

        H.liftEffect $ do
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
          -- addChangeListener editor_

          -- Add some example text
          Document.setValue
            ( intercalate "\n" $
                [ "# Project Overview"
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
            document

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

    Comment -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        newMarker <- H.liftEffect do
          session <- Editor.getSession ed
          range <- Editor.getSelectionRange ed
          -- start is of type Types.Position = {row :: Int, column :: Int}
          -- Range.getStartRow does not work. Return undefined.
          start <- Range.getStart range
          let
            sRow = Types.getRow start
            sCol = Types.getColumn start
          newID <- Session.addMarker range "my-marker" "text" false session
          let
            newCommentSection =
              { markerID: newID
              , comments: []
              , resolved: false
              }
            newMarker =
              { id: newID
              , type: "info"
              , range: range
              , startRow: sRow
              , startCol: sCol
              , mCommentSection: Just newCommentSection
              }
          addAnnotation (markerToAnnotation newMarker) session
          pure newMarker
        H.modify_ \st ->
          st
            { mTocEntry = st.mTocEntry <#> \entry ->
                entry { markers = sortMarkers (newMarker : entry.markers) }
            }

    DeleteComment -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        case state.mTocEntry of
          Nothing -> pure unit
          Just tocEntry -> do
            session <- H.liftEffect $ Editor.getSession ed
            cursor <- H.liftEffect $ Editor.getCursorPosition ed
            let markers = tocEntry.markers

            -- remove the marker at the cursor position and return the remaining markers
            (Tuple newMarkers deletedIDs) <- H.liftEffect $ removeMarkerByPosition
              cursor
              markers
              session
            let newTOCEntry = tocEntry { markers = newMarkers }
            H.modify_ \st ->
              st { mTocEntry = Just newTOCEntry }
            H.raise (DeletedComment newTOCEntry deletedIDs)

    ShowWarning -> do
      H.modify_ \state -> state { pdfWarningIsShown = not state.pdfWarningIsShown }

    SelectComment -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        cursor <- H.liftEffect $ Editor.getCursorPosition ed
        state <- H.get
        -- extract markers from the current TOC entry
        let
          tocEntry = case state.mTocEntry of
            Nothing -> { id: -1, name: "No entry", content: "", markers: [] }
            Just e -> e
          markers = tocEntry.markers
          row = Types.getRow cursor
          col = Types.getColumn cursor

        -- remove the marker at the cursor position and return the remaining markers
        targetRange <- H.liftEffect $ Range.create row col row col
        matching <- H.liftEffect $ filterA
          (\m -> Range.containsRange targetRange m.range)
          markers
        case head matching of
          Nothing -> pure unit
          Just marker -> case marker.mCommentSection of
            Nothing -> pure unit
            Just commentSection -> H.raise
              (SelectedCommentSection tocEntry.id marker.id commentSection)

    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ChangeSection entry a -> do
      let
        comments = case head entry.markers of
          Nothing -> []
          Just marker -> case marker.mCommentSection of
            Nothing -> []
            Just commentSection -> commentSection.comments
      H.modify_ \state -> state { mTocEntry = Just entry }

      -- Put the content of the section into the editor and update markers
      H.gets _.mEditor >>= traverse_ \ed -> do
        updatedMarkers <- H.liftEffect do
          session <- Editor.getSession ed
          document <- Session.getDocument session

          -- Set editor content
          let content = entry.content
          Document.setValue content document

          -- Remove existing markers
          existingMarkers <- Session.getMarkers session
          for_ existingMarkers \marker -> do
            id <- Marker.getId marker
            Session.removeMarker id session

          -- Clear annotations
          Session.clearAnnotations session

          -- Reinsert markers with new IDs and annotations
          for (entry.markers) \marker -> do
            newID <- Session.addMarker marker.range "my-marker" "text" false session
            addAnnotation (markerToAnnotation marker) session
            pure marker { id = newID }

        -- Update state with new marker IDs
        H.modify_ \st ->
          st { mTocEntry = Just entry { markers = updatedMarkers } }

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

      let
        contentText = case allLines of
          Just ls -> intercalate "\n" ls
          Nothing -> "<No content>"

        entry = case state.mTocEntry of
          Nothing ->
            { id: -1, name: "Section not found", content: "", markers: [] }
          Just e -> e

        newEntry = entry { content = contentText }

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

-- | Change listener for the editor.
--
--   This function should implement stuff like parsing and syntax analysis,
--   linting, code completion, etc., but for now it just places markers
--   for occurrences of the word "error" in order to demonstrate how to use
--   the Ace editor API with markers.
addChangeListener :: Types.Editor -> Effect Unit
addChangeListener editor_ = do
  session <- Editor.getSession editor_
  -- Setup change listener to react to changes in the editor
  Session.onChange session \_ -> do
    lines <- Session.getDocument session >>= Document.getAllLines

    -- Remove all existing markers ...
    markers <- Session.getMarkers session
    for_ markers \marker -> do
      id <- Marker.getId marker
      Session.removeMarker id session
    -- ... and annotations
    Session.clearAnnotations session

    -- traverse all lines and insert a marker for each occurrence of "error"
    for_ (0 .. (Array.length lines - 1)) \row -> do
      case Array.index lines row of
        Just line -> do
          for_ (findAllIndicesOf "error" line) \col -> do
            r <- Range.create row col row (col + 5)
            _ <- Session.addMarker r "ace_error-marker" "text" false session
            addAnnotation
              { row: row
              , column: col
              , text: "Error found!"
              , type: "error"
              }
              session
        Nothing -> pure unit

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

-- Multiple marker removal functions
-- These functions remove markers by IDs, range, position, or row/column.

-- The base function for all other removeMarker functions
-- Remove marker based on the ids and return the remaining markers and the deleted ids
removeMarkerByIDs
  :: Array Int
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Tuple (Array AnnotatedMarker) (Array Int))
removeMarkerByIDs targetIDs markers session = do
  -- Remove all markers with the given IDs
  for_ targetIDs \targetID -> Session.removeMarker targetID session

  -- Create a new array with the remaining markers
  let remainingMarkers = filter (\m -> not (m.id `elem` targetIDs)) markers

  -- Set the remaining markers in the session
  let annotations = map markerToAnnotation remainingMarkers
  Session.setAnnotations annotations session

  pure (Tuple remainingMarkers targetIDs)

removeMarkerByID
  :: Int
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Tuple (Array AnnotatedMarker) (Array Int))
removeMarkerByID targetID = removeMarkerByIDs [ targetID ]

removeMarkerByRange
  :: Types.Range
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Tuple (Array AnnotatedMarker) (Array Int))
removeMarkerByRange targetRange markers session = do
  matching <- filterA (\m -> Range.containsRange targetRange m.range) markers
  let ids = map _.id matching
  removeMarkerByIDs ids markers session

removeMarkerByPosition
  :: Types.Position
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Tuple (Array AnnotatedMarker) (Array Int))
removeMarkerByPosition targetPos marker session = do
  let
    row = Types.getRow targetPos
    col = Types.getColumn targetPos
  targetRange <- Range.create row col row col
  removeMarkerByRange targetRange marker session

removeMarkerByRowCol
  :: Int
  -> Int
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Tuple (Array AnnotatedMarker) (Array Int))
removeMarkerByRowCol row col marker session = do
  targetRange <- Range.create row col row col
  removeMarkerByRange targetRange marker session
