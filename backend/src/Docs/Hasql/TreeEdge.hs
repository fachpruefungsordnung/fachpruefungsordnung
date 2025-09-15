module Docs.Hasql.TreeEdge
    ( TreeEdge (..)
    , TreeEdgeChildRef (..)
    , TreeEdgeChild (..)
    ) where

import GHC.Int (Int64)

import Docs.Hash (Hash, Hashable (..))
import Docs.TextElement (TextElement, TextElementID)
import Docs.Tree (NodeHeader)

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
    , position :: Int64
    , child :: TreeEdgeChildRef
    }

instance Hashable TreeEdge where
    updateHash = flip updateHash'
      where
        updateHash' edge =
            flip updateHash (position edge)
                . flip updateHash (child edge)
