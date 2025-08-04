module FPO.Dto.DocumentDto.NodeHeader
  ( NodeHeader(..)
  , getId
  , getKind
  )
  where

import Data.Argonaut (class DecodeJson, class EncodeJson)

newtype NodeHeader = NodeHeader
  { identifier :: Int
  , kind :: String
  }

getId :: NodeHeader -> Int
getId (NodeHeader nh) = nh.identifier

getKind :: NodeHeader -> String
getKind (NodeHeader nh) = nh.kind

derive newtype instance decodeJsonNodeHeader :: DecodeJson NodeHeader
derive newtype instance encodeJsonNodeHeader :: EncodeJson NodeHeader
