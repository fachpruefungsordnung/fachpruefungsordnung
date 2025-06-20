module FPO.Components.Editor where

import Prelude

import Ace (ace, editNode) as Ace
import Ace.Document as Document
import Ace.EditSession as EditSession
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Marker as Marker
import Ace.Range as Range
import Ace.Types as Types
import Data.Array (filter, filterA, intercalate, sortBy, (..), (:))
import Data.Array as Array
import Data.Foldable (elem, for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse, for)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, ref, style) as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))

type AnnotatedMarker = 
  { id :: Int
  , type :: String
  , range :: Types.Range
  , startRow :: Int
  , startCol :: Int
  , endRow :: Int
  , endColumn :: Int
  }

sortMarkers :: Array AnnotatedMarker -> Array AnnotatedMarker
sortMarkers = sortBy (comparing _.startRow <> comparing _.startCol)

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe String
  , markers :: Maybe (Array AnnotatedMarker)
  }

type State =
  { editor :: Maybe Types.Editor
  , tocEntry :: Maybe TOCEntry
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

_pdfSlideBar = Proxy :: Proxy "pdfSlideBar"

data Output
  = ClickedQuery (Maybe (Array String))
  | SavedSection TOCEntry

data Action
  = Init
  | Paragraph
  | Delete
  | Comment
  | ShowWarning

-- We use a query to get the content of the editor
data Query a
  -- = RequestContent (Array String -> a)
  = QueryEditor a
  | SaveSection a
  | LoadPdf a
  | ChangeSection TOCEntry a

editor
  :: forall input m
   . MonadEffect m
  => H.Component Query input Output m
editor = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  initialState :: State
  initialState =
    { editor: Nothing
    , tocEntry: Nothing
    , pdfWarningAvailable: false
    , pdfWarningIsShown: false
    }

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render _ =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
      [ HH.div -- Second toolbar

          [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap2 ] ]
          [ HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> Paragraph
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-paragraph" ] ] []
              , HH.text " Paragraph"
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> Delete
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-x-lg" ] ] []
              , HH.text " Delete"
              ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
              , HE.onClick \_ -> Comment
              ]
              [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-x-lg" ] ] []
              , HH.text " Comment"
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
        H.modify_ _ { editor = Just editor_ }

        H.liftEffect $ do
          session <- Editor.getSession editor_
          document <- Session.getDocument session

          -- Set the editor's theme and mode
          Editor.setTheme "ace/theme/github" editor_
          EditSession.setMode "ace/mode/custom_mode" session
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

    Delete -> do
      H.gets _.editor >>= traverse_ \ed -> do
        H.liftEffect $ do
          row <- Types.getRow <$> Editor.getCursorPosition ed
          document <- Editor.getSession ed >>= Session.getDocument
          Document.removeLines row row document

    Paragraph -> do
      H.gets _.editor >>= traverse_ \ed -> do
        H.liftEffect $ do
          row <- Types.getRow <$> Editor.getCursorPosition ed
          document <- Editor.getSession ed >>= Session.getDocument
          Document.insertLines row [ "Paragraph", "=========" ] document

    Comment -> do
      H.gets _.editor >>= traverse_ \ed -> do
        newMarker <- H.liftEffect do
          session <- Editor.getSession ed
          range <- Editor.getSelectionRange ed
          -- start is of type Types.Position = {row :: Int, column :: Int}
          -- Range.getStartRow does not work. Return undefined.
          start <- Range.getStart range
          end <- Range.getEnd range
          let 
            sRow = Types.getRow start
            sCol = Types.getColumn start
            eRow = Types.getRow end
            eCol = Types.getColumn end
          newID <- Session.addMarker range "my-marker" "text" false session
          let newMarker =
                { id: newID
                , type: "info"
                , range: range
                , startRow: sRow
                , startCol: sCol
                , endRow: eRow
                , endColumn: eCol
                }
          addAnnotation (markerToAnnotation newMarker) session
          pure newMarker
        H.modify_ \st ->
          st 
            { tocEntry = st.tocEntry <#> \entry ->
              let updatedMarkers = sortMarkers case entry.markers of
                    Just ms -> newMarker : ms
                    Nothing -> [newMarker]
              in entry { markers = Just updatedMarkers }
            }

    ShowWarning -> do
      H.modify_ \state -> state { pdfWarningIsShown = not state.pdfWarningIsShown }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ChangeSection entry a -> do
      H.modify_ \state -> state { tocEntry = Just entry }

      -- Put the content of the section into the editor and update markers
      H.gets _.editor >>= traverse_ \ed -> do
        updatedMarkers <- H.liftEffect do
          session <- Editor.getSession ed
          document <- Session.getDocument session

          -- Set editor content
          let content = fromMaybe "" entry.content
          Document.setValue content document

          -- Remove existing markers
          existingMarkers <- Session.getMarkers session
          for_ existingMarkers \marker -> do
            id <- Marker.getId marker
            Session.removeMarker id session

          -- Clear annotations
          Session.clearAnnotations session

          -- Reinsert markers with new IDs and annotations
          for (fromMaybe [] entry.markers) \marker -> do
            newID <- Session.addMarker marker.range "my-marker" "text" false session
            addAnnotation (markerToAnnotation marker) session
            pure marker { id = newID }

        -- Update state with new marker IDs
        H.modify_ \st ->
          st { tocEntry = Just entry { markers = Just updatedMarkers } }

      pure (Just a)

    LoadPdf a -> do
      H.modify_ _ { pdfWarningAvailable = true }
      pure (Just a)

    SaveSection a -> do
      state <- H.get
      allLines <- H.gets _.editor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines

      let
        contentText = case allLines of
          Just ls -> intercalate "\n" ls
          Nothing -> "<No content>"

        entry = case state.tocEntry of
          Nothing -> { id: -1, name: "Section not found", content: Nothing , markers: Nothing }
          Just e -> e

        newEntry =
          { id: entry.id
          , name: entry.name
          , content: Just contentText
          , markers: entry.markers
          }

      H.modify_ \st -> st { tocEntry = Just newEntry }
      H.raise (SavedSection newEntry)
      pure (Just a)

    -- Because Session does not provide a way to get all lines directly,
    -- we need to take another indirect route to get the lines.
    -- Notice that this extra step is not needed for all js calls.
    -- For example, `Session.getLine` can be called directly.
    QueryEditor a -> do
      allLines <- H.gets _.editor >>= traverse \ed -> do
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

removeMarkerByIDs
  :: Array Int
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Array AnnotatedMarker)
removeMarkerByIDs targetIDs markers session = do
  -- Remove all markers with the given IDs
  for_ targetIDs \targetID -> Session.removeMarker targetID session

  -- Create a new array with the remaining markers
  let remainingMarkers = filter (\m -> not (m.id `elem` targetIDs)) markers

  -- Set the remaining markers in the session
  let annotations = map markerToAnnotation remainingMarkers
  Session.setAnnotations annotations session

  pure remainingMarkers

removeMarkerByID
  :: Int
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Array AnnotatedMarker)
removeMarkerByID targetID = removeMarkerByIDs [targetID]

removeMarkerByRange
  :: Types.Range
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Array AnnotatedMarker)
removeMarkerByRange targetRange markers session = do
  matching <- filterA (\m -> Range.containsRange m.range targetRange) markers
  let ids = map _.id matching
  removeMarkerByIDs ids markers session

removeMarkerByPosition
  :: Types.Position
  -> Array AnnotatedMarker
  -> Types.EditSession
  -> Effect (Array AnnotatedMarker)
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
  -> Effect (Array AnnotatedMarker)
removeMarkerByRowCol row col marker session = do 
  targetRange <- Range.create row col row col
  removeMarkerByRange targetRange marker session

markerToAnnotation :: AnnotatedMarker -> Types.Annotation
markerToAnnotation m =
  { row: m.startRow
  , column: m.startCol
  , text: "Comment found!"
  , type: m.type
  }