module FPO.Dto.DocumentDto.TextElementRevision where

import Data.Argonaut (class DecodeJson)
import Data.Newtype (class Newtype)
import FPO.Dto.DocumentDto.NodeHeader as NH

newtype TextElementRevision = TextElementRevision
  { textElement :: NH.NodeHeader
  -- TODO: Add text revision information
  -- , revision :: Maybe TextRevision
  }

derive instance newtypeTextElementRevision :: Newtype TextElementRevision _
derive newtype instance decodeJsonTextElementRevision ::
  DecodeJson TextElementRevision
