module Docs.Hasql.TreeEdge
    ( TreeEdge (..)
    , TreeEdgeChildRef (..)
    , TreeEdgeChild (..)
    ) where

import Data.Text (Text)
import GHC.Int (Int32)

import Docs.TextElement (TextElement, TextElementID)
import Docs.Tree (NodeHeader)
import DocumentManagement.Hash (Hash, Hashable (..))

data TreeEdgeChild
    = TreeEdgeToTextElement TextElement
    | TreeEdgeToNode Hash NodeHeader

data TreeEdgeChildRef
    = TreeEdgeRefToTextElement TextElementID
    | TreeEdgeRefToNode Hash

instance Hashable TreeEdgeChildRef where
    updateHash ctx (TreeEdgeRefToTextElement elementID) =
        updateHash ctx elementID
    updateHash ctx (TreeEdgeRefToNode nodeHash) = updateHash ctx nodeHash

data TreeEdge = TreeEdge
    { parentHash :: Hash
    , position :: Int32
    , title :: Text
    , child :: TreeEdgeChildRef
    }

instance Hashable TreeEdge where
    updateHash = flip updateHash'
      where
        updateHash' edge =
            flip updateHash (position edge)
                . flip updateHash (title edge)
                . flip updateHash (child edge)
