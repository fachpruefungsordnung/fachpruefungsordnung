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
  }

derive instance newtypePostTextDto :: Newtype PostTextDto _

instance decodeJsonPostTextDto :: DecodeJson PostTextDto where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "identifier"
    kind <- obj .: "textElementKind"
    pure $ PostTextDto { identifier: id, kind }

instance encodeJsonPostTextDto :: EncodeJson PostTextDto where
  encodeJson (PostTextDto { kind }) =
    encodeJson { textElementKind: kind }

instance showPostTextDto :: Show PostTextDto where
  show (PostTextDto { identifier, kind }) =
    "PostTextDto { identifier: " <> show identifier <> ", kind: " <> kind <> " }"

decodePostTextDto :: Json -> Either JsonDecodeError PostTextDto
decodePostTextDto json = decodeJson json

encodePostTextDto :: PostTextDto -> Json
encodePostTextDto postTextDto = encodeJson postTextDto

getID :: PostTextDto -> Int
getID (PostTextDto { identifier }) = identifier