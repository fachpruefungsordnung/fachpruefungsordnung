module FPO.Types where

import Prelude

import Ace.Types as Types
import Data.Array (sortBy)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)

type AnnotatedMarker =
  { id :: Int
  , type :: String
  , range :: Types.Range
  , startRow :: Int
  , startCol :: Int
  , comment :: Maybe Comment
  }

type Comment =
  { author :: String
  , timestamp :: DateTime
  , content :: String
  }

type CommentSection =
  { markerID :: Int
  , comments :: Array Comment
  , resolved :: Boolean
  }

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe String
  , markers :: Maybe (Array AnnotatedMarker)
  }

sortMarkers :: Array AnnotatedMarker -> Array AnnotatedMarker
sortMarkers = sortBy (comparing _.startRow <> comparing _.startCol)

markerToAnnotation :: AnnotatedMarker -> Types.Annotation
markerToAnnotation m =
  { row: m.startRow
  , column: m.startCol
  , text: "Comment found!"
  , type: m.type
  }