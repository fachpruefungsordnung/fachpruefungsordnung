module DocManV2.Document
    ( Document (..)
    , DocumentID (..)
    , Node (..)
    , NodeID (..)
    , Version (..)
    , VersionID (..)
    )
where

-- Um ein Dokument über die API zu laden, kann ein Baum angefragt werden, der die
-- Struktur der einzelnen Paragraphen und Knoten darstellt. An einem anderen Endpunkt
-- können die Inhalte eines bestimmten Paragraphen oder Knoten abgerufen werden.
-- Mit einem POST auf den gleichen Endpunkt kann der Paragraph oder Knoten bearbeitet
-- werden. Dabei wird dieser Endpunkt sowohl genutzt, um die Inhalte zu verändern,
-- als auch um die Struktur zu ändern. Ist die `children` Eigenschaft gesetzt,
-- so werden die angegebenen Nodes in der gegebenen Reihenfolge fortan als Kindknoten
-- des bearbeiteten Knotens behandelt. Ist die `Content` Eigenschaft gesetzt,
-- so wird der Inhalt des Paragraphen (oder Knoten?) geändert.
-- TODO: Sollen Knoten sowohl Text als auch Kindknoten haben können?
--       -> Äußerst relevant für interne Darstellung oberhalb des Datalayers und
--          für die API.

import Data.Text (Text)
import Data.UUID (UUID)
import DocManV2.Hash (Hashed)
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

newtype VersionID = VersionID
    { unVersionID :: Int32
    }

data Version
    = Version
    { versionID :: VersionID
    , versionNode :: Node
    , versionAuthor :: UUID
    , versionContent :: Maybe (Hashed Text)
    , versionChildren :: [NodeID]
    }

data CreateVersion
    = CreateVersion
    { createVersionNodeID :: NodeID
    , createVersionAuthor :: UUID
    , createVersionContent :: Maybe (Hashed Text)
    , createVersionChildren :: Maybe [NodeID]
    }

data Tree = Tree
    { treeNode :: Node
    , treeVersionID :: VersionID
    , treeChildren :: [Tree]
    }
