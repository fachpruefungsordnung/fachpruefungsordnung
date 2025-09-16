module FPO.Dto.DocumentDto.NodeHeader
  ( NodeHeader(..)
  , getId
  , getKind
  ) where

import Data.Argonaut (class DecodeJson, class EncodeJson)

-- This seems to be `TextElement` in the backend?
newtype NodeHeader = NodeHeader
  { identifier :: Int
  , textElementKind :: String
  }

getId :: NodeHeader -> Int
getId (NodeHeader nh) = nh.identifier

getKind :: NodeHeader -> String
getKind (NodeHeader nh) = nh.textElementKind

derive newtype instance decodeJsonNodeHeader :: DecodeJson NodeHeader
derive newtype instance encodeJsonNodeHeader :: EncodeJson NodeHeader
