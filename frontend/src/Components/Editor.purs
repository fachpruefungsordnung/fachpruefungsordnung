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

type State =
  { editor :: Maybe Types.Editor
  , pdfWarningAvailable :: Boolean
  , pdfWarningIsShown :: Boolean
  }

type Slots =
  ( pdfSlideBar :: H.Slot FileSidebar.Query FileSidebar.Output Unit
  )

_pdfSlideBar = Proxy :: Proxy "pdfSlideBar"

data Output
  = ClickedHTTPRequest
  | ClickedQuery (Maybe (Array String))
  | LoadPdf
  | ClickedShowWarning
  | SendPDF (Maybe String)

data Action
  = Init
  | Paragraph
  | Delete
  | MakeRequest
  | QueryEditor
  | ClickLoadPdf
  | ShowWarning
  | HandleFileSidebar FileSidebar.Output

-- We use a query to get the content of the editor
data Query a = RequestContent (Array String -> a)

type Input = Unit

editor :: forall m. MonadEffect m => H.Component Query Input Output m
editor = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      }
  }
  where
  initialState :: State
  initialState =
    { editor: Nothing, pdfWarningAvailable: false, pdfWarningIsShown: false }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
      [ HH.div -- First toolbar

          [ HP.classes [ HB.bgDark, HB.overflowAuto, HB.dFlex, HB.flexRow ] ]
          [ HH.span [ HP.classes [ HB.textWhite ] ] [ HH.text "Toolbar" ]
          , HH.button
              [ HP.classes [ HB.btn, HB.btnSuccess, HB.btnSm ]
              , HE.onClick $ const MakeRequest
              ]
              [ HH.text "Click Me for HTTP request" ]
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
      , HH.div -- Second toolbar

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
      , HH.div -- FileSidebar

          [ HP.classes [ HB.bgLight, HB.dFlex, HB.flexRow, HB.g0, HB.overflowHidden ]
          , HP.style "min-height: 0"
          ]
          -- TODO: add input of dummy pdf file
          [ HH.slot _pdfSlideBar unit FileSidebar.fileSidebar unit HandleFileSidebar ]
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

          -- Add a change listener to the editor
          addChangeListener editor_

          -- Add some example text
          Document.setValue
            ( intercalate "\n" $
                [ "Hier wird später der Code geschrieben!"
                , ""
                , "Fehler können schon als solche markiert werden, siehe error!"
                , ""
                , "% Dieser Editor verwendet testweise den TeX-Mode (und GitHub-Theme),"
                , "% also können wir TeX-Highlighting verwenden."
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

    MakeRequest -> do
      H.raise ClickedHTTPRequest

    ClickLoadPdf -> do
      H.raise LoadPdf
      H.modify_ _ { pdfWarningAvailable = true }

    -- Because Session does not provide a way to get all lines directly,
    -- we need to take another indirect route to get the lines.
    -- Notice that this extra step is not needed for all js calls.
    -- For example, `Session.getLine` can be called directly.
    QueryEditor -> do
      allLines <- H.gets _.editor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines
      H.raise (ClickedQuery allLines)

    ShowWarning -> do
      H.modify_ \state -> state { pdfWarningIsShown = not state.pdfWarningIsShown }
      H.raise ClickedShowWarning

    HandleFileSidebar output -> case output of
      FileSidebar.SendPDF mURL -> do
        H.raise (SendPDF mURL)
        H.modify_ _ { pdfWarningAvailable = true }

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

    -- Remove all existing markers
    markers <- Session.getMarkers session
    for_ markers \marker -> do
      id <- Marker.getId marker
      Session.removeMarker id session

    -- traverse all lines and insert a marker for each occurrence of "error"
    for_ (0 .. (Array.length lines - 1)) \row -> do
      case Array.index lines row of
        Just line -> do
          for_ (findAllIndicesOf "error" line) \col -> do
            r <- Range.create row col row (col + 5)
            _ <- Session.addMarker r "ace_error-marker" "text" false session
            pure unit
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
