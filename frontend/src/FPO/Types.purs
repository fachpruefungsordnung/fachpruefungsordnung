module FPO.Types where

import Prelude

import Ace.Types as Types
import Data.Array (find, sortBy)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import FPO.Dto.DocumentDto (DocumentTree, NodeWithRef(..))
import FPO.Dto.TreeDto (Tree, findTree)

-- TODO We can also store different markers, such as errors. But do we want to?
type AnnotatedMarker =
  { id :: Int
  , type :: String
  , startRow :: Int
  , startCol :: Int
  , endRow :: Int
  , endCol :: Int
  , markerText :: String
  , mCommentSection :: Maybe CommentSection
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
  , content :: String
  -- Is stored as 32bit Int = 2,147,483,647
  -- Schould not create so many markers, right?
  , newMarkerNextID :: Int
  , markers :: Array AnnotatedMarker
  }

-- shortend version for TOC component to not update its content
-- since it only uses id and name only
type ShortendTOCEntry =
  { id :: Int
  , name :: String
  }

type TOCTree = Tree TOCEntry

-- Empty TOCEntry in case of errors
emptyTOCEntry :: TOCEntry
emptyTOCEntry =
  { id: -1
  , name: "Error"
  , content: "Error"
  , newMarkerNextID: -1
  , markers: []
  }

findTOCEntry :: Int -> TOCTree -> Maybe TOCEntry
findTOCEntry tocID tocEntries = findTree (\e -> e.id == tocID) tocEntries

findCommentSection :: Int -> Int -> TOCTree -> Maybe CommentSection
findCommentSection tocID markerID tocEntries = do
  entry <- findTOCEntry tocID tocEntries
  marker <- find (\m -> m.id == markerID) entry.markers
  marker.mCommentSection

sortMarkers :: Array AnnotatedMarker -> Array AnnotatedMarker
sortMarkers = sortBy (comparing _.startRow <> comparing _.startCol)

markerToAnnotation :: AnnotatedMarker -> Types.Annotation
markerToAnnotation m =
  { row: m.startRow
  , column: m.startCol
  , text: m.markerText
  , type: m.type
  }

shortenTOC :: TOCEntry -> ShortendTOCEntry
shortenTOC { id, name } = { id, name }

-- TODO create more timestamps versions and discuss, where to store this
timeStampsVersions :: Array Formatter
timeStampsVersions =
  [ -- DD.MM.YY HH:mm
    ( DayOfMonthTwoDigits
        : Placeholder "."
        : MonthShort
        : Placeholder "."
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  -- DD/MM/YY HH:mm
  , ( DayOfMonthTwoDigits
        : Placeholder "/"
        : MonthShort
        : Placeholder "/"
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  -- MM/DD/YY HH:mm
  , ( MonthShort
        : Placeholder "/"
        : DayOfMonthTwoDigits
        : Placeholder "/"
        : YearTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  -- YY/MM/DD HH:mm
  , ( YearTwoDigits
        : Placeholder "/"
        : MonthShort
        : Placeholder "/"
        : DayOfMonthTwoDigits
        : Placeholder " "
        : Hours24
        : Placeholder ":"
        : MinutesTwoDigits
        : Nil
    )
  ]

-- Tree functions for TOC

nodeWithRefToTOCEntry :: NodeWithRef -> TOCEntry
nodeWithRefToTOCEntry (NodeWithRef { id, kind, content }) =
  { id: id
  , name: kind
  , content: fromMaybe "" content
  , newMarkerNextID: 0
  , markers: []
  }

tocEntryToNodeWithRef :: TOCEntry -> NodeWithRef
tocEntryToNodeWithRef { id, name, content } =
  NodeWithRef { id, kind: name, content: Just content }

documentTreeToTOCTree :: DocumentTree -> TOCTree
documentTreeToTOCTree = map nodeWithRefToTOCEntry

tocTreeToDocumentTree :: TOCTree -> DocumentTree
tocTreeToDocumentTree = map tocEntryToNodeWithRef