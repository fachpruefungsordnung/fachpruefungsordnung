module FPO.Dto.DocumentDto.FullDocument
  ( FullDocument(..)
  , decodeFullDocument
  , getBody
  , getHeader
  ) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, (.:))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import FPO.Dto.DocumentDto.DocumentHeader (DocumentHeader)
import FPO.Dto.DocumentDto.DocumentTree (DocumentTreeTER)

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
  -- body <- obj .: "body" >>= traverse decodeDocument 
  -- Body is not really relevant for decoding if creating a new document, so we ignore it for now
  pure $ FullDocument { header, body: Nothing }
