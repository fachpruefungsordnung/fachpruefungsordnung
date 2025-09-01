module FPO.Dto.CommentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Either (Either)
import Data.Newtype (class Newtype)
import FPO.Dto.DocumentDto.DocDate (DocDate)

newtype Author = Author
  { id :: String
  , name :: String
  }

newtype CommentT = Comment
  { author :: Author
  , content :: String
  , timestamp :: DocDate
  }

newtype Section = Section
  { id :: Int
  , firstComment :: CommentT
  , replies :: Array CommentT
  , status :: String
  }

newtype CommentSections = CommentSections
  { comments :: Array Section }

derive instance newtypeAuthor :: Newtype Author _
derive instance newtypeCommentT :: Newtype CommentT _
derive instance newtypeSection :: Newtype Section _
derive instance newtypeCommentSections :: Newtype CommentSections _

instance decodeJsonAuthor :: DecodeJson Author where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "identifier"
    name <- obj .: "name"
    pure $ Author { id: id, name: name }

instance decodeJsonCommentT :: DecodeJson CommentT where
  decodeJson json = do
    obj <- decodeJson json
    aut <- obj .: "author"
    con <- obj .: "content"
    time <- obj .: "timestamp"
    pure $ Comment { author: aut, content: con, timestamp: time }

instance decodeJsonSection :: DecodeJson Section where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "identifier"
    fst <- obj .: "message"
    replies <- obj .: "replies"
    status <- obj .: "status"
    tag <- status .: "tag"
    pure $ Section { id: id, firstComment: fst, replies: replies, status: tag }

instance decodeJsonCommentSections :: DecodeJson CommentSections where
  decodeJson json = do
    obj <- decodeJson json
    coms <- obj .: "comments"
    pure $ CommentSections { comments: coms }

derive newtype instance showAuthor :: Show Author
derive newtype instance showCommentT :: Show CommentT
derive newtype instance showSection :: Show Section
derive newtype instance showCommentSections :: Show CommentSections

decodeComment :: Json -> Either JsonDecodeError CommentT
decodeComment json = decodeJson json

decodeSection :: Json -> Either JsonDecodeError Section
decodeSection json = decodeJson json

decodeCommentSection :: Json -> Either JsonDecodeError CommentSections
decodeCommentSection json = decodeJson json

getName :: Author -> String
getName (Author { name }) = name

getSectionID :: Section -> Int
getSectionID (Section { id }) = id

getCommentSections :: CommentSections -> Array Section
getCommentSections (CommentSections { comments }) = comments
