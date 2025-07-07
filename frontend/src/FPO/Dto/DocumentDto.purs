module FPO.Dto.DocumentDto where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Newtype (class Newtype, unwrap)

newtype NodeWithRef = NodeWithRef
  { id :: Int
  , kind :: String
  , content :: Maybe String
  }

newtype Tree = Tree
  { node :: NodeWithRef
  , children :: Array Edge
  }

newtype Edge = Edge
  { title :: String
  , child :: Tree
  }

type DocumentTree = Tree

derive instance newtypeTree :: Newtype Tree _
derive instance newtypeEdge :: Newtype Edge _
derive instance newtypeNodeWithRef :: Newtype NodeWithRef _

instance decodeJsonNodeWithRef :: DecodeJson NodeWithRef where
  decodeJson json = do
    obj <- decodeJson json
    inner <- obj .: "content"  -- neu: extrahiere das innere Objekt
    id <- inner .: "id"
    kind <- inner .: "kind"
    content <- inner .: "content"
    pure $ NodeWithRef { id, kind, content }

instance decodeJsonEdge :: DecodeJson Edge where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    child <- obj .: "child"
    pure $ Edge { title, child }

instance decodeJsonTree :: DecodeJson Tree where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .: "node"
    children <- obj .: "children"
    pure $ Tree { node, children }

instance encodeJsonNodeWithRef :: EncodeJson NodeWithRef where
  encodeJson (NodeWithRef { id, kind, content }) =
    encodeJson
      { content:
          { id
          , kind
          , content
          }
      }

instance encodeJsonEdge :: EncodeJson Edge where
  encodeJson (Edge { title, child }) =
    encodeJson
      { title
      , child
      }

instance encodeJsonTree :: EncodeJson Tree where
  encodeJson (Tree { node, children }) =
    encodeJson
      { node
      , children
      }


-- show instances for debugging purposes
instance showNodeWithRef :: Show NodeWithRef where
  show (NodeWithRef { id, kind, content }) =
    "NodeWithRef { id: " <> show id <> ", kind: " <> show kind <> ", content: " <> show content <> " }"

instance showEdge :: Show Edge where
  show (Edge { title, child }) =
    "Edge { title: " <> show title <> ", child: " <> show child <> " }"

instance showTree :: Show Tree where
  show (Tree { node, children }) =
    "Tree { node: " <> show node <> ", children: " <> show children <> " }"


decodeDocument :: Json -> Either JsonDecodeError Tree
decodeDocument json = do
  obj <- decodeJson json
  body <- obj .: "body"
  root <- body .: "root"
  decodeJson root

-- Encode instances
-- | Erzeugt ein JSON-Objekt mit Dummy-Daten passend zur Upload-Schnittstelle
encodeCreateCommit :: Tree -> Json
encodeCreateCommit tree =
  encodeJson
    { info:
        { author: "00000000-0000-0000-0000-000000000000"
        , message: "Initial commit"
        , parents: [1]
        }
    , root: encodeTree tree
    }

encodeTree :: Tree -> Json
encodeTree (Tree { node, children }) =
  let { id, kind, content } = unwrap node in
  encodeJson
    { node:
        { id
        , kind
        , content: fromMaybe "" content
        }
    , edges: map encodeEdge children
    }

encodeEdge :: Edge -> Json
encodeEdge (Edge { title, child }) =
  encodeJson
    { title
    , child: encodeTree child
    }