module FPO.Dto.PostTextDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Newtype (class Newtype)

newtype PostTextDto = PostTextDto
  { identifier :: Int
  , kind :: String
  , type_ :: String
  }

derive instance newtypePostTextDto :: Newtype PostTextDto _

instance decodeJsonPostTextDto :: DecodeJson PostTextDto where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "identifier"
    kind <- obj .: "textElementKind"
    type_ <- obj .: "textElementType"
    pure $ PostTextDto { identifier: id, kind, type_ }

instance encodeJsonPostTextDto :: EncodeJson PostTextDto where
  encodeJson (PostTextDto { kind, type_ }) =
    encodeJson { kind: kind, type_: type_ }

instance showPostTextDto :: Show PostTextDto where
  show (PostTextDto { identifier, kind }) =
    "PostTextDto { identifier: " <> show identifier <> ", kind: " <> kind <> " }"

-- | Create a new PostTextDto where the identifier is not relevant.
-- | TODO: It would be smart to differentiate between a `PostTextDto` (without id)
-- |       and a `GetTextDto` (with id), but for simplicity we just use this one type.
-- |       The POST requests do not care about the identifier field.
createPostTextDto :: { kind :: String, type_ :: String } -> PostTextDto
createPostTextDto { kind, type_ } =
  PostTextDto { identifier: 0, kind, type_ }

decodePostTextDto :: Json -> Either JsonDecodeError PostTextDto
decodePostTextDto json = decodeJson json

encodePostTextDto :: PostTextDto -> Json
encodePostTextDto postTextDto = encodeJson postTextDto

getID :: PostTextDto -> Int
getID (PostTextDto { identifier }) = identifier
