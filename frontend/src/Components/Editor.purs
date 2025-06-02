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
import Data.Array (intercalate, (..), (:))
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FPO.Components.FileSidebar as FileSidebar
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, ref, style) as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(Proxy))

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe (Array String)
  }

type State =
  { editor :: Maybe Types.Editor
  , tocEntry :: Maybe TOCEntry
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type Slots =
  ( pdfSlideBar :: H.Slot FileSidebar.Query FileSidebar.Output Unit
  )

_pdfSlideBar = Proxy :: Proxy "pdfSlideBar"

data Output
  = ClickedQuery (Maybe (Array String))
  | SavedSection TOCEntry
  | SendPDF (Maybe String)

data Action
  = Init
  | Paragraph
  | Delete
  | ShowWarning
  | HandleFileSidebar FileSidebar.Output

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

  render :: State -> H.ComponentHTML Action Slots m
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
          ]
      -- take out the file sidebar
      -- , HH.div -- FileSidebar

      --     [ HP.classes [ HB.bgLight, HB.dFlex, HB.flexRow, HB.g0, HB.overflowHidden ]
      --     , HP.style "min-height: 0"
      --     ]
      --     -- TODO: add input of dummy pdf file
      --     [ HH.slot _pdfSlideBar unit FileSidebar.fileSidebar unit HandleFileSidebar ]
      , HH.div -- Editor container

          [ HP.ref (H.RefLabel "container")
          , HP.classes [ HB.flexGrow1 ]
          , HP.style "min-height: 0"
          ]
          []
      ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
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
          EditSession.setMode "ace/mode/tex" session
          Editor.setEnableLiveAutocompletion true editor_

          -- Add a change listener to the editor
          addChangeListener editor_

          -- Add some example text
          Document.setValue
            ( intercalate "\n" $
                [ "The code will be written here later!"
                , ""
                , "Errors can already be marked as such, see error!"
                , ""
                , "% This editor is currently using TeX mode (and GitHub theme) for testing,"
                , "% so we can use TeX highlighting."
                , "   $\\dotsc$"
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

    ShowWarning -> do
      H.modify_ \state -> state { pdfWarningIsShown = not state.pdfWarningIsShown }

    HandleFileSidebar output -> case output of
      FileSidebar.SendPDF mURL -> do
        H.raise (SendPDF mURL)
        H.modify_ _ { pdfWarningAvailable = true }

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of

    ChangeSection entry a -> do
      H.modify_ \state -> state { tocEntry = Just entry }
      let
        content = case entry.content of
          Just lines -> lines
          Nothing -> []
      H.gets _.editor >>= traverse_ \ed -> do
        H.liftEffect $ do
          document <- Editor.getSession ed >>= Session.getDocument
          Document.setValue (intercalate "\n" content) document
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
        entry =
          case state.tocEntry of
            Nothing -> { id: -1, name: "Section not found", content: allLines }
            Just e -> e

        newEntry = { id: entry.id, name: entry.name, content: allLines }

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
