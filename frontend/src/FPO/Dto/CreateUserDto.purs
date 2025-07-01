module FPO.Dto.CreateUserDto where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

newtype CreateUserDto = CreateUserDto
  { registerEmail :: String
  , registerName :: String
  , registerPassword :: String
  , groupID :: Int
  }

derive instance newtypeCreateUserDto :: Newtype CreateUserDto _

derive newtype instance encodeJsonCreateUserDto :: EncodeJson CreateUserDto
derive newtype instance decodeJsonCreateUserDto :: DecodeJson CreateUserDto

empty :: CreateUserDto
empty = CreateUserDto
  { registerEmail: "", registerName: "", registerPassword: "", groupID: 1 }

getName :: CreateUserDto -> String
getName (CreateUserDto { registerName }) = registerName

getEmail :: CreateUserDto -> String
getEmail (CreateUserDto { registerEmail }) = registerEmail

getPassword :: CreateUserDto -> String
getPassword (CreateUserDto { registerPassword }) = registerPassword

withName :: String -> CreateUserDto -> CreateUserDto
withName name (CreateUserDto { registerEmail, registerPassword, groupID }) =
  CreateUserDto { registerEmail, registerName: name, registerPassword, groupID }

withEmail :: String -> CreateUserDto -> CreateUserDto
withEmail email (CreateUserDto { registerName, registerPassword, groupID }) =
  CreateUserDto { registerEmail: email, registerName, registerPassword, groupID }

withPassword :: String -> CreateUserDto -> CreateUserDto
withPassword password (CreateUserDto { registerEmail, registerName, groupID }) =
  CreateUserDto { registerEmail, registerName, registerPassword: password, groupID }
