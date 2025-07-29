module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import FPO.Dto.TreeDto (Edge(..), RootTree(..), Tree(..))

type DocumentID = Int
type CommitID = Int

newtype NodeHeader = NodeHeader
  { id :: Int
  , kind :: String
  }

type DocumentTree = RootTree NodeHeader

newtype DocumentHeader = DH
  { group :: Int, headCommit :: Maybe CommitID, id :: DocumentID, name :: String }

newtype DocumentHeaderPlusPermission = DHPP
  { document :: DocumentHeader, documentPermission :: String }

derive instance newtypeDocumentHeader :: Newtype DocumentHeader _
derive instance newtypeDocumentHeaderPlusPermission ::
  Newtype DocumentHeaderPlusPermission _

derive instance newtypeNodeHeader :: Newtype NodeHeader _

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

instance decodeJsonNodeHeader :: DecodeJson NodeHeader where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "identifier"
    kind <- obj .: "kind"
    pure $ NodeHeader { id, kind }

instance encodeJsonNodeHeader :: EncodeJson NodeHeader where
  encodeJson (NodeHeader { id, kind }) =
    encodeJson
      { content:
          { id
          , kind
          }
      }

-- show instances for debugging purposes
instance showNodeHeader :: Show NodeHeader where
  show (NodeHeader { id, kind }) =
    "NodeHeader { id: " <> show id <> ", kind: " <> show kind <> " }"

decodeDocument :: Json -> Either JsonDecodeError DocumentTree
decodeDocument json = do
  obj <- decodeJson json
  root <- obj .: "root"
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
    , root: encodeRootTree tree
    }

encodeRootTree :: DocumentTree -> Json
encodeRootTree Empty = encodeJson {}
encodeRootTree (RootTree { children }) =
  encodeJson { children: map encodeEdge children }

encodeTree :: Tree NodeHeader -> Json
encodeTree (Node { children }) = encodeJson { edges: map encodeEdge children}
encodeTree (Leaf { node }) =
  let
    { id, kind } = unwrap node
  in
  encodeJson { node: encodeJson { identifier: id, kind } }

encodeEdge :: Edge NodeHeader -> Json
encodeEdge (Edge child) = encodeJson { child: encodeTree child }
