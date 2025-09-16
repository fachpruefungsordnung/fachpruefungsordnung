module FPO.Dto.ContentDto where

import Prelude

import Data.Argonaut (Json, fromObject)
import Data.Argonaut.Decode
  ( class DecodeJson
  , JsonDecodeError
  , decodeJson
  , (.:)
  , (.:?)
  )
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import FPO.Types (AnnotatedMarker)

newtype CommentAnchor = CommentAnchor
  { id :: Int
  , startCol :: Int
  , startRow :: Int
  , endCol :: Int
  , endRow :: Int
  }

newtype Content = Content
  { content :: String
  , parent :: Int
  , draft :: Boolean
  }

newtype ContentWrapper = Wrapper
  { content :: Content
  , comments :: Array CommentAnchor
  }

derive instance newtypeCommentAnchor :: Newtype CommentAnchor _
derive instance newtypeContent :: Newtype Content _
derive instance newtypeContentWrapper :: Newtype ContentWrapper _

instance decodeJsonCommentAnchor :: DecodeJson CommentAnchor where
  decodeJson json = do
    obj <- decodeJson json
    comId <- obj .: "comment"
    anc <- obj .: "anchor"
    start <- anc .: "start"
    sCol <- start .: "col"
    sRow <- start .: "row"
    end <- anc .: "end"
    eCol <- end .: "col"
    eRow <- end .: "row"
    pure $
      CommentAnchor
        { id: comId
        , startCol: sCol
        , startRow: sRow
        , endCol: eCol
        , endRow: eRow
        }

instance decodeJsonContent :: DecodeJson Content where
  decodeJson json = do
    obj <- decodeJson json
    mCon <- obj .:? "content"
    case mCon of
      Just con -> do
        header <- obj .: "header"
        id <- header .: "identifier"
        pure $ Content { content: con, parent: id, draft: false }
      Nothing -> do
        con <- obj .: "draftContent"
        header <- obj .: "draftHeader"
        id <- header .: "draftIdentifier"
        pure $ Content { content: con, parent: id, draft: true }

instance decodeJsonContentWrapper :: DecodeJson ContentWrapper where
  decodeJson json = do
    obj <- decodeJson json
    ele <- obj .: "element"
    rev <- ele .: "revision"
    con <- decodeJson (fromObject rev)
    coms <- rev .: "commentAnchors"
    pure $ Wrapper { content: con, comments: coms }

instance encodeJsonCommentAnchor :: EncodeJson CommentAnchor where
  encodeJson (CommentAnchor { id, startCol, startRow, endCol, endRow }) =
    encodeJson
      { anchor:
          { start:
              { col: startCol
              , row: startRow
              }
          , end:
              { col: endCol
              , row: endRow
              }
          }
      , comment: id
      }

instance encodeJsonContent :: EncodeJson Content where
  encodeJson (Content { content, parent }) =
    encodeJson
      { content: content
      , parent: parent
      }

instance encodeJsonContentWrapper :: EncodeJson ContentWrapper where
  encodeJson (Wrapper { content: Content { content, parent }, comments }) =
    encodeJson
      { content: content
      , parent: parent
      , commentAnchors: map encodeJson comments
      }

instance showCommentAnchor :: Show CommentAnchor where
  show (CommentAnchor { id, startCol, startRow, endCol, endRow }) =
    "Comment { id: " <> show id <> ", startCol: " <> show startCol <> ", startRow: "
      <> show startRow
      <> ", endCol: "
      <> show endCol
      <> ", endRow: "
      <> show endRow
      <> " }"

instance showContent :: Show Content where
  show (Content { content, parent, draft }) = "Content { content: " <> content
    <> ", parent: "
    <> show parent
    <> ", "
    <> show draft
    <> " }"

instance showContentWrapper :: Show ContentWrapper where
  show (Wrapper { content, comments }) = "Content : { " <> show content <> ", "
    <> show comments
    <> " }"

decodeContent :: Json -> Either JsonDecodeError Content
decodeContent json = decodeJson json

decodeContentWrapper :: Json -> Either JsonDecodeError ContentWrapper
decodeContentWrapper json = decodeJson json

encodeContent :: Content -> Json
encodeContent content = encodeJson content

encodeWrapper :: ContentWrapper -> Json
encodeWrapper wrapper = encodeJson wrapper

getContentText :: Content -> String
getContentText (Content { content }) = content

getContentParent :: Content -> Int
getContentParent (Content { parent }) = parent

getContentDraft :: Content -> Boolean
getContentDraft (Content { draft }) = draft

-- Wrapper getter and setter

getWrapperContent :: ContentWrapper -> Content
getWrapperContent (Wrapper { content }) = content

getWrapperComments :: ContentWrapper -> Array CommentAnchor
getWrapperComments (Wrapper { comments }) = comments

setWrapper :: Content -> Array CommentAnchor -> ContentWrapper
setWrapper content comments = Wrapper { content, comments }

setWrapperContent :: Content -> ContentWrapper -> ContentWrapper
setWrapperContent content (Wrapper { comments }) = Wrapper { content, comments }

setContentText :: String -> Content -> Content
setContentText newText (Content con) = Content (con { content = newText })

setContentParent :: Int -> Content -> Content
setContentParent newParent (Content con) = Content (con { parent = newParent })

failureContent :: Content
failureContent = Content
  { content: "Error decoding content", parent: -1, draft: true }

failureContentWrapper :: ContentWrapper
failureContentWrapper = Wrapper { content: failureContent, comments: [] }

extractDraft
  :: Content -> Json -> Either JsonDecodeError { content :: Content, typ :: String }
extractDraft (Content cont) json = do
  obj <- decodeJson json
  ele <- obj .: "element"
  typ <- ele .: "type" :: Either JsonDecodeError String
  case typ of
    "noConflict" -> do
      newRev <- ele .: "newRevision"
      hdr <- newRev .: "header"
      pid <- hdr .: "identifier"
      pure $ { content: Content $ cont { parent = pid }, typ: "noConflict" }
    "draftCreated" -> do
      -- TODO update Commentmarkers
      draft <- ele .: "draft"
      newCon <- draft .: "draftContent"
      pure $ { content: Content $ cont { content = newCon }, typ: "draftCreated" }
    _ -> -- "conflict"

      pure { content: Content cont, typ: "conflict" }

convertToAnnotetedMarker
  :: CommentAnchor
  -> AnnotatedMarker
convertToAnnotetedMarker (CommentAnchor { id, startCol, startRow, endCol, endRow }) =
  { id: id
  , type: "info"
  , startRow: startRow
  , startCol: startCol
  , endRow: endRow
  , endCol: endCol
  , markerText: "tbc"
  , mCommentSection: Nothing
  }

convertToCommentAnchor
  :: AnnotatedMarker
  -> CommentAnchor
convertToCommentAnchor { id, startRow, startCol, endRow, endCol } =
  CommentAnchor { id, startCol, startRow, endCol, endRow }
