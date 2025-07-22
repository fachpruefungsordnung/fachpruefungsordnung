module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import FPO.Dto.TreeDto (Edge(..), Tree(..))

newtype NodeWithRef = NodeWithRef
  { id :: Int
  , kind :: String
  , content :: Maybe String
  }

type DocumentTree = Tree NodeWithRef

type DocumentID = Int
type CommitID = Int

newtype DocumentHeader = DH
  { group :: Int, headCommit :: Maybe CommitID, id :: DocumentID, name :: String }

newtype DocumentHeaderPlusPermission = DHPP
  { document :: DocumentHeader, documentPermission :: String }

derive instance newtypeDocumentHeader :: Newtype DocumentHeader _
derive instance newtypeDocumentHeaderPlusPermission ::
  Newtype DocumentHeaderPlusPermission _

derive instance newtypeNodeWithRef :: Newtype NodeWithRef _

instance decodeJsonHeader :: DecodeJson DocumentHeader where
  decodeJson json = do
    obj <- decodeJson json
    g <- obj .: "group"
    h <- obj .: "headCommit"
    i <- obj .: "id"
    n <- obj .: "name"
    pure $ DH { group: g, headCommit: h, id: i, name: n }

instance decodeJsonHeaderPlusPermission :: DecodeJson DocumentHeaderPlusPermission where
  decodeJson json = do
    obj <- decodeJson json
    doc <- obj .: "document"
    docPerm <- obj .: "documentPermission"
    pure $ DHPP { document: doc, documentPermission: docPerm }

getDHName :: DocumentHeader -> String
getDHName (DH dh) = dh.name

getDHID :: DocumentHeader -> Int
getDHID (DH dh) = dh.id

getDHHeadCommit :: DocumentHeader -> Maybe CommitID
getDHHeadCommit (DH dh) = dh.headCommit

getDHPPName :: DocumentHeaderPlusPermission -> String
getDHPPName (DHPP dhpp) = getDHName dhpp.document

getDHPPID :: DocumentHeaderPlusPermission -> Int
getDHPPID (DHPP dhpp) = getDHID dhpp.document

instance decodeJsonNodeWithRef :: DecodeJson NodeWithRef where
  decodeJson json = do
    obj <- decodeJson json
    inner <- obj .: "content" -- neu: extrahiere das innere Objekt
    id <- inner .: "id"
    kind <- inner .: "kind"
    content <- inner .: "content"
    pure $ NodeWithRef { id, kind, content }

instance encodeJsonNodeWithRef :: EncodeJson NodeWithRef where
  encodeJson (NodeWithRef { id, kind, content }) =
    encodeJson
      { content:
          { id
          , kind
          , content
          }
      }

-- show instances for debugging purposes
instance showNodeWithRef :: Show NodeWithRef where
  show (NodeWithRef { id, kind, content }) =
    "NodeWithRef { id: " <> show id <> ", kind: " <> show kind <> ", content: "
      <> show content
      <> " }"

decodeDocument :: Json -> Either JsonDecodeError DocumentTree
decodeDocument json = do
  obj <- decodeJson json
  body <- obj .: "body"
  root <- body .: "root"
  decodeJson root

-- Encode instances
-- | Erzeugt ein JSON-Objekt mit Dummy-Daten passend zur Upload-Schnittstelle
encodeCreateCommit :: DocumentTree -> Json
encodeCreateCommit tree =
  encodeJson
    { info:
        { author: "00000000-0000-0000-0000-000000000000"
        , message: "Initial commit"
        , parents: [ 1 ]
        }
    , root: encodeTree tree
    }

encodeTree :: DocumentTree -> Json
encodeTree Empty = encodeJson {}
encodeTree (Node { node, children }) =
  let
    { id, kind, content } = unwrap node
  in
    encodeJson
      { node:
          { id
          , kind
          , content: fromMaybe "" content
          }
      , edges: map encodeEdge children
      }

encodeEdge :: Edge NodeWithRef -> Json
encodeEdge (Edge { title, child }) =
  encodeJson
    { title
    , child: encodeTree child
    }
