module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import FPO.Dto.TreeDto (RootTree)

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

encodeDocumentTree :: DocumentTree -> Json
encodeDocumentTree tree = encodeJson $ map (\(NodeHeader { id }) -> id) tree
