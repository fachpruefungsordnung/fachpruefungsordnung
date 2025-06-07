-- | Module for handling JSON data in the application.
module Data.JSON where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, (.:))
import Data.Either (Either)
import FPO.Data.Store (User)

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
