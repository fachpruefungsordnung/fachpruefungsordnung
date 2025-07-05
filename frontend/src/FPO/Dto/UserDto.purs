module FPO.Dto.UserDto where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, (.:))
import Data.Either (Either)

-- | TODO: This is an incomplete representation of a user.
-- |
-- | Representation of a user in the application.
type User = { userName :: String, isAdmin :: Boolean }

-- | Decodes a JSON object into a `User`.
-- |
-- | TODO: It might be better to implement `instance DecodeJson User`, but
-- |       this forces us to use Data instead of Type..
decodeUser :: Json -> Either JsonDecodeError User
decodeUser json = do
  obj <- decodeJson json
  name <- obj .: "fullUserName"
  admin <- obj .: "fullUserIsSuperadmin"
  pure { userName: name, isAdmin: admin }
