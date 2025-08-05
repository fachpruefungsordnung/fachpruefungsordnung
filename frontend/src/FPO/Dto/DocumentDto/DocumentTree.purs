module FPO.Dto.DocumentDto.DocumentTree where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson, (.:))
import Data.Either (Either)
import FPO.Dto.DocumentDto.NodeHeader as NH
import FPO.Dto.DocumentDto.TreeDto (RootTree)

type DocumentTree = RootTree NH.NodeHeader

decodeDocument :: Json -> Either JsonDecodeError DocumentTree
decodeDocument json = do
  obj <- decodeJson json
  root <- obj .: "root"
  decodeJson root

encodeDocumentTree :: DocumentTree -> Json
encodeDocumentTree = encodeJson <<< map NH.getId
