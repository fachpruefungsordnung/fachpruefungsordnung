module FPO.Types where

import Prelude

import Ace.Types as Types
import Data.Array (find, sortBy)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)

type AnnotatedMarker =
  { id :: Int
  , type :: String
  , range :: Types.Range
  , startRow :: Int
  , startCol :: Int
  , commentSection :: Maybe CommentSection
  }

type CommentSection =
  { markerID :: Int
  , comments :: Array Comment
  , resolved :: Boolean
  }

type Comment =
  { author :: String
  , timestamp :: DateTime
  , content :: String
  }

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe String
  , markers :: Maybe (Array AnnotatedMarker)
  }

-- shortend version for TOC component to not update its content
-- since it only uses id and name only
type ShortendTOCEntry =
  { id :: Int
  , name :: String
  }

findTOCEntry :: Int -> Array TOCEntry -> Maybe TOCEntry
findTOCEntry tocID tocEntries = find (\e -> e.id == tocID) tocEntries

findCommentSection :: Int -> Int -> Array TOCEntry -> Maybe CommentSection
findCommentSection tocID markerID tocEntries = do
  entry  <- findTOCEntry tocID tocEntries
  markers <- entry.markers
  marker <- find (\m -> m.id == markerID) markers
  marker.commentSection

sortMarkers :: Array AnnotatedMarker -> Array AnnotatedMarker
sortMarkers = sortBy (comparing _.startRow <> comparing _.startCol)

markerToAnnotation :: AnnotatedMarker -> Types.Annotation
markerToAnnotation m =
  { row: m.startRow
  , column: m.startCol
  , text: "Comment found!"
  , type: m.type
  }