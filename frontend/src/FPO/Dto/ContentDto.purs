module FPO.Dto.ContentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Newtype (class Newtype)

newtype Content = Content
  { content :: String
  , parent :: Int
  }

derive instance newtypeContent :: Newtype Content _

instance decodeJsonContent :: DecodeJson Content where
  decodeJson json = do
    obj <- decodeJson json
    rev <- obj .: "revision"
    con <- rev .: "content"
    header <- rev .: "header"
    id <- header .: "identifier"
    pure $ Content { content: con, parent: id }

instance encodeJsonContent :: EncodeJson Content where
  encodeJson (Content { content, parent }) =
    encodeJson { content: content, parent: parent }

instance showContent :: Show Content where
  show (Content { content, parent }) = "Content { content: " <> content
    <> ", parent: "
    <> show parent
    <> " }"

decodeContent :: Json -> Either JsonDecodeError Content
decodeContent json = decodeJson json

encodeContent :: Content -> Json
encodeContent content = encodeJson content

getContentText :: Content -> String
getContentText (Content { content }) = content

setContentText :: String -> Content -> Content
setContentText newText (Content { parent }) = Content { content: newText, parent }

setContentParent :: Int -> Content -> Content
setContentParent newParent (Content { content }) = Content
  { content, parent: newParent }

failureContent :: Content
failureContent = Content { content: "Error decoding content", parent: -1 }

extractNewParent :: Content -> Json -> Either JsonDecodeError Content
extractNewParent (Content cont) json = do
  obj <- decodeJson json
  newRev <- obj .: "newRevision"
  header <- newRev .: "header"
  newPar <- header .: "identifier"
  pure $ Content $ cont { parent = newPar }