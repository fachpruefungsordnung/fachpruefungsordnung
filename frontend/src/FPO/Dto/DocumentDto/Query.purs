module FPO.Dto.DocumentDto.Query where

import Data.Argonaut (class DecodeJson)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentHeader)

newtype Query = Q
  { group :: Maybe Int, user :: Maybe String }

derive instance newtypeQuery :: Newtype Query _
derive newtype instance decodeJsonQuery :: DecodeJson Query

newtype DocumentQuery = DQ
  { documents :: Array DocumentHeader, query :: Query }

getDocuments :: DocumentQuery -> Array DocumentHeader
getDocuments (DQ dq) = dq.documents

derive newtype instance decodeJsonDocumentQuery :: DecodeJson DocumentQuery
