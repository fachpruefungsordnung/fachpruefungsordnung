module FPO.Dto.CreateDocumentDto where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

-- | According to `POST /documents` API endpoint.
newtype DocumentCreateDto = DocumentCreateDto
  { documentCreateGroupId :: Int
  , documentCreateName :: String
  }

derive instance newtypeDocumentCreateDto :: Newtype DocumentCreateDto _

derive newtype instance encodeJsonDocumentCreateDto :: EncodeJson DocumentCreateDto
derive newtype instance decodeJsonDocumentCreateDto :: DecodeJson DocumentCreateDto
