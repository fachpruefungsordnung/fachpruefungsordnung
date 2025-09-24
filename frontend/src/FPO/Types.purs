module FPO.Types
  ( AnnotatedMarker
  , Comment
  , CommentSection
  , FirstComment
  , TOCEntry
  , TOCTree
  , cdCommentToComment
  , documentTreeToTOCTree
  , emptyComment
  , emptyCommentSection
  , emptyTOCEntry
  , findTOCEntry
  , findTitleTOCEntry
  , markerToAnnotation
  , nodeHeaderToTOCEntry
  , replaceTOCEntry
  , sectionDtoToCS
  , tocEntryToNodeHeader
  , tocTreeToDocumentTree
  ) where

import Prelude

import Ace.Types as Types
import Data.Array (sortBy)
import Data.Date (canonicalDate)
import Data.Date.Component (Day, Month(..), Year)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time(..))
import Data.Time.Component (Hour, Millisecond, Minute, Second)
import FPO.Dto.CommentDto (CommentT(..), Section(..))
import FPO.Dto.CommentDto as CD
import FPO.Dto.DocumentDto.DocDate (docDateToDateTime)
import FPO.Dto.DocumentDto.DocumentTree as DT
import FPO.Dto.DocumentDto.NodeHeader as NH
import FPO.Dto.DocumentDto.TreeDto
  ( RootTree
  , findRootTree
  , findTitleRootTree
  , replaceNodeRootTree
  )
import Partial.Unsafe (unsafePartial)

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
  , first :: Maybe Comment
  , replies :: Array Comment
  , resolved :: Boolean
  }

type FirstComment =
  { markerID :: Int
  , resolved :: Boolean
  , first :: Comment
  }

type Comment =
  { author :: String
  , content :: String
  , timestamp :: DateTime
  }

type TOCEntry =
  { id :: Int
  , name :: String
  -- paragraph ID (§id) for later
  , paraID :: Int
  }

type TOCTree = RootTree TOCEntry

-- Empty TOCEntry in case of errors
emptyTOCEntry :: TOCEntry
emptyTOCEntry =
  { id: -1
  , name: "Error"
  , paraID: -1
  }

defaultDateTime :: DateTime
defaultDateTime =
  let
    y = unsafePartial $ fromJust (toEnum 1970 :: Maybe Year)
    m = January
    d = unsafePartial $ fromJust (toEnum 1 :: Maybe Day)
    date = (canonicalDate y m d)

    h = unsafePartial $ fromJust (toEnum 0 :: Maybe Hour)
    mi = unsafePartial $ fromJust (toEnum 0 :: Maybe Minute)
    s = unsafePartial $ fromJust (toEnum 0 :: Maybe Second)
    ms = unsafePartial $ fromJust (toEnum 0 :: Maybe Millisecond)
    time = Time h mi s ms
  in
    DateTime date time

emptyComment :: Comment
emptyComment =
  { author: "No author"
  , content: ""
  , timestamp: defaultDateTime
  }

emptyCommentSection :: CommentSection
emptyCommentSection =
  { markerID: -1
  , first: Nothing
  , replies: []
  , resolved: false
  }

findTOCEntry :: Int -> TOCTree -> Maybe TOCEntry
findTOCEntry tocID = findRootTree (\e -> e.id == tocID)

findTitleTOCEntry :: Int -> TOCTree -> Maybe String
findTitleTOCEntry tocID = findTitleRootTree (\e -> e.id == tocID)

replaceTOCEntry :: Int -> TOCEntry -> TOCTree -> TOCTree
replaceTOCEntry tocID = replaceNodeRootTree (\e -> e.id == tocID)

markerToAnnotation :: AnnotatedMarker -> Types.Annotation
markerToAnnotation m =
  { row: m.startRow
  , column: m.startCol
  , text: m.markerText
  , type: m.type
  }

-- Tree functions for TOC

nodeHeaderToTOCEntry :: NH.NodeHeader -> TOCEntry
nodeHeaderToTOCEntry nh =
  { id: NH.getId nh
  , name: NH.getKind nh
  , paraID: 0 -- implement it later
  }

tocEntryToNodeHeader :: TOCEntry -> NH.NodeHeader
tocEntryToNodeHeader { id, name } =
  NH.NodeHeader { identifier: id, textElementKind: name }

documentTreeToTOCTree :: DT.DocumentTreeTE -> TOCTree
documentTreeToTOCTree = map nodeHeaderToTOCEntry

tocTreeToDocumentTree :: TOCTree -> DT.DocumentTreeTE
tocTreeToDocumentTree = map tocEntryToNodeHeader

-- Comment functions

cdCommentToComment :: CD.CommentT -> Comment
cdCommentToComment (Comment { author, content, timestamp }) =
  let
    name = CD.getName author
    time = docDateToDateTime timestamp
  in
    { author: name, content: content, timestamp: time }

sectionDtoToCS :: CD.Section -> CommentSection
sectionDtoToCS (Section { id, firstComment, replies, status }) =
  let
    fst = cdCommentToComment firstComment
    rep = map cdCommentToComment replies
    -- comments = cons fst rep
    resolved = status == "Resolved"
  in
    { markerID: id, first: Just fst, replies: rep, resolved: resolved }
