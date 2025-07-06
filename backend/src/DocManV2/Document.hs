{-# LANGUAGE GADTs #-}

module DocManV2.Document
    ( Document (..)
    , DocumentID (..)
    , Node (..)
    , NodeID (..)
    , NodeVersion (..)
    , NodeVersionID (..)
    ) where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Int (Int32)
import UserManagement.Group (GroupID)

newtype DocumentID = DocumentID
    { unDocumentID :: Int32
    }

data Document = Document
    { documentID :: DocumentID
    , documentName :: Text
    , documentGroup :: GroupID
    , documentRoot :: Maybe NodeID
    }

newtype NodeID = NodeID
    { unNodeID :: Int32
    }

type NodeKind = Text

data Node = Node
    { nodeID :: NodeID
    , nodeKind :: NodeKind
    }

newtype NodeVersionID = NodeVersionID
    { unNodeVersionID :: Int32
    }

data NodeVersion
    = NodeVersion
    { nodeVersionID :: NodeVersionID
    , nodeVersionNodeID :: NodeID
    , nodeVersionContent :: Text
    , nodeVersionAuthor :: UUID
    }
