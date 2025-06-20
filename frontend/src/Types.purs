module FPO.Types where

import Prelude

import Ace.Types as Types
import Data.Array (sortBy)
import Data.Maybe (Maybe(..))

type AnnotatedMarker =
  { id :: Int
  , type :: String
  , range :: Types.Range
  , startRow :: Int
  , startCol :: Int
  , endRow :: Int
  , endColumn :: Int
  }

type TOCEntry =
  { id :: Int
  , name :: String
  , content :: Maybe String
  , markers :: Maybe (Array AnnotatedMarker)
  }

sortMarkers :: Array AnnotatedMarker -> Array AnnotatedMarker
sortMarkers = sortBy (comparing _.startRow <> comparing _.startCol)