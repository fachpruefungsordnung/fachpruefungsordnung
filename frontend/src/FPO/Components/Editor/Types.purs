module FPO.Components.Editor.Types
  ( DragHandle(..)
  , ElementData
  , HandleBorder
  , HistoryOp(..)
  , LiveMarker
  , Path
  , RenderKind(..)
  , createMarkerRange
  , cursorInRange
  , failureLiveMarker
  , hideHandlesFrom
  , highlightSelection
  , near
  , removeLiveMarker
  , setAnnotations
  , setMarkerSelectedClass
  , showHandlesFor
  , updateMarkers
  ) where

import Prelude

import Ace.Anchor as Anchor
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types as Types
import Data.Array (mapMaybe, uncons)
import Data.Array as Array
import Data.Foldable (for_)
import Data.HashMap (HashMap, toArrayBy)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FPO.Types
  ( AnnotatedMarker
  , FirstComment
  , TOCEntry
  )
import Unsafe.Coerce (unsafeCoerce)

data DragHandle = DragStart | DragEnd
data HistoryOp = HUndo | HRedo
data RenderKind = RenderHTML | RenderPDF

type ElementData = Maybe
  { tocEntry :: TOCEntry
  , revID :: Maybe Int
  , title :: String
  }

type HandleBorder =
  { row :: Int
  , column :: Int
  , side :: DragHandle
  }

-- For tracking the comment markers live
-- Only store the values in save
type LiveMarker =
  { annotedMarkerID :: Int
  , startAnchor :: Types.Anchor
  , endAnchor :: Types.Anchor
  , markerText :: String
  , ref :: Ref Int
  }

type Path = Array Int

-- Help function for markerAnnoHS
addNames :: String -> Int -> String
addNames name occurence =
  if occurence == 1 then
    name
  else
    name <> "+" <> show occurence

-- Get the liveMarker, if the user cursor clicked on the comment
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

createMarkerRange :: AnnotatedMarker -> Effect Types.Range
createMarkerRange marker = do
  range <- Range.create marker.startRow marker.startCol marker.endRow marker.endCol
  pure range

failureLiveMarker :: LiveMarker
failureLiveMarker =
  { annotedMarkerID: -1
  , startAnchor: unsafeCoerce unit -- Fake Anchor
  , endAnchor: unsafeCoerce unit -- Fake Anchor
  , markerText: ""
  , ref: unsafeCoerce (-1 :: Int) -- Fake Ref
  }

hideHandlesFrom :: Types.EditSession -> Maybe Int -> Maybe Int -> Effect Unit
hideHandlesFrom session m1 m2 = do
  for_ m1 \i -> Session.removeMarker i session
  for_ m2 \i -> Session.removeMarker i session

highlightSelection
  :: Types.Editor
  -> Array LiveMarker
  -> LiveMarker -- ^ selectedLiveMarker
  -> Effect Unit
highlightSelection ed lms mSel = do
  session <- Editor.getSession ed
  for_ lms \lm ->
    setMarkerSelectedClass session lm (lm.annotedMarkerID == mSel.annotedMarkerID)

near :: Types.Position -> Types.Position -> Boolean
near a b =
  Types.getRow a == Types.getRow b
    && abs (Types.getColumn a - Types.getColumn b) <= 1
  where
  abs :: Int -> Int
  abs x = if x < 0 then (x * (-1)) else x

removeLiveMarker :: LiveMarker -> Types.EditSession -> Effect Unit
removeLiveMarker lm session = do
  -- Marker entfernen
  markerId <- Ref.read lm.ref
  Session.removeMarker markerId session

  -- Anchors vom Dokument lösen
  Anchor.detach lm.startAnchor
  Anchor.detach lm.endAnchor

setAnnotations
  :: HashMap Int (HashMap String Int)
  -> Maybe Types.Editor
  -> Effect Unit
setAnnotations markerAnnoHS mEditor = do
  for_ mEditor \ed -> do
    session <- Editor.getSession ed
    let
      -- extract information from markerAnnoHS
      tmp = toArrayBy
        (\k v -> { line: k, text: joinWith ", " (toArrayBy addNames v) })
        markerAnnoHS
      -- map it to correct Annotation type
      anns =
        map
          (\{ line, text } -> { row: line, column: 1, text: text, type: "info" })
          tmp
    Session.setAnnotations anns session

-- Give the marker the correct class depending on isSelected
setMarkerSelectedClass
  :: Types.EditSession
  -> LiveMarker
  -> Boolean -- ^ isSelected?
  -> Effect Unit
setMarkerSelectedClass session lm isSelected = do
  -- 1) remove old Ace-Marker entfernen
  oldId <- Ref.read lm.ref
  Session.removeMarker oldId session

  -- 2) calculate new Range from Anchors
  Types.Position { row: sRow, column: sCol } <- Anchor.getPosition lm.startAnchor
  Types.Position { row: eRow, column: eCol } <- Anchor.getPosition lm.endAnchor
  range <- Range.create sRow sCol eRow eCol

  -- 3) set new class
  let
    cls =
      if isSelected then "my-marker selected"
      else "my-marker"
  newId <- Session.addMarker range cls "text" false session

  -- 4) new Marker-ID
  Ref.write newId lm.ref

showHandlesFor
  :: Types.EditSession
  -> LiveMarker
  -> Effect { startId :: Maybe Int, endId :: Maybe Int }
showHandlesFor session lm = do
  -- Get Anchor-Positions
  Types.Position { row: sRow, column: sCol } <- Anchor.getPosition lm.startAnchor
  Types.Position { row: eRow, column: eCol } <- Anchor.getPosition lm.endAnchor

  -- Start Handle
  r1 <- Range.create sRow sCol sRow (sCol + 1)
  hid1 <- Session.addMarker r1 "fpo-handle-start" "text" false session

  -- End Handle
  r2 <- Range.create eRow eCol eRow (eCol + 1)
  hid2 <- Session.addMarker r2 "fpo-handle-end" "text" false session

  pure { startId: Just hid1, endId: Just hid2 }

updateMarkers :: Array FirstComment -> Array AnnotatedMarker -> Array AnnotatedMarker
updateMarkers firsts markers =
  mapMaybe
    ( \m ->
        case Array.find (\fs -> fs.markerID == m.id && not fs.resolved) firsts of
          Just fs -> Just (m { markerText = fs.comment.author })
          Nothing -> Nothing
    )
    markers