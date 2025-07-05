-- | Module for handling JSON data in the application.
module FPO.Data.JSON where

import Prelude

import Data.Argonaut (Json, decodeJson, encodeJson, (.:))
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either)
import FPO.Data.Store (Document, DocumentPlusPermission, Group, GroupCreate, User)

-- | TODO: It might be better to implement `instance DecodeJson User`, but
-- |       this forces us to use Data instead of Type..
-- | TODO: This is an incomplete representation of a user.
-- |
-- | Decodes a JSON object into a `User`.
decodeUser :: Json -> Either JsonDecodeError User
decodeUser json = do
  obj <- decodeJson json
  name <- obj .: "fullUserName"
  admin <- obj .: "fullUserIsSuperadmin"
  pure { userName: name, isAdmin: admin }

-- | Decodes a JSON object into a `Group`.
decodeGroup :: Json -> Either JsonDecodeError Group
decodeGroup json = do
  obj <- decodeJson json
  name <- obj .: "groupOverviewName"
  id <- obj .: "groupOverviewID"
  pure { groupOverviewName: name, groupOverviewId: id }

decodeDocument :: Json -> Either JsonDecodeError Document
decodeDocument json = do
  obj <- decodeJson json
  g <- obj .: "group"
  h <- obj .: "headCommit"
  i <- obj .: "id"
  n <- obj .: "name"
  pure { group: g, headCommit: h, id: i, name: n }

decodeDocumentWithPermission :: Json -> Either JsonDecodeError DocumentPlusPermission
decodeDocumentWithPermission json = do
  obj <- decodeJson json
  doc <- obj .: "document"
  docPerm <- obj .: "documentPermission"
  pure { document: doc, documentPermission: docPerm }

encodeGroupCreate :: GroupCreate -> Json
encodeGroupCreate = encodeJson
