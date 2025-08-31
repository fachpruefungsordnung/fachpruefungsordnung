module FPO.Dto.DocumentDto.FullDocument where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, (.:))
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentHeader)
import FPO.Dto.DocumentDto.DocumentTree (DocumentTreeTER, decodeDocument)

newtype FullDocument = FullDocument
  { header :: DocumentHeader
  , body :: Maybe DocumentTreeTER
  }

getHeader :: FullDocument -> DocumentHeader
getHeader (FullDocument fd) = fd.header

getBody :: FullDocument -> Maybe DocumentTreeTER
getBody (FullDocument fd) = fd.body

decodeFullDocument :: Json -> Either JsonDecodeError FullDocument
decodeFullDocument json = do
  obj <- decodeJson json
  header <- obj .: "header" >>= decodeJson
  body <- obj .: "body" >>= traverse decodeDocument
  pure $ FullDocument { header, body }
