-- | This module defines the `DocumentTree` type, which represents
-- | the hierarchical structure of a document.

module FPO.Dto.DocumentDto.DocumentTree where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
  , encodeJson
  , (.:)
  )
import Data.Either (Either)
import FPO.Dto.DocumentDto.NodeHeader as NH
import FPO.Dto.DocumentDto.TextElementRevision (TextElementRevision)
import FPO.Dto.DocumentDto.TreeDto (RootTree)

-- | A DocumentTree with NodeHeader (alias TextElement) nodes, as given by
-- | the `GET /docs/{documentID}/tree/{treeRevision}` endpoint.
type DocumentTreeTE = DocumentTree NH.NodeHeader

-- | A DocumentTree with TextElementRevision nodes, as given by the
-- | `POST docs` endpoint.
type DocumentTreeTER = DocumentTree TextElementRevision

type DocumentTree a = RootTree a

decodeDocument
  :: forall a. DecodeJson a => Json -> Either JsonDecodeError (DocumentTree a)
decodeDocument json = do
  obj <- decodeJson json
  -- TODO: We are ignoring `header` for now, but we might need it later.
  root <- obj .: "root"
  decodeJson root

-- | Encodes a `DocumentTree NodeHeader` as a `DocumentTree TextElementID`.
encodeDocumentTree :: DocumentTree NH.NodeHeader -> Json
encodeDocumentTree = encodeJson <<< map NH.getId
