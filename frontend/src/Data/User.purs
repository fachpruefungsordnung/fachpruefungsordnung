module FPO.Data.User where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show (class Show)

newtype User = User { fullUserName :: String, fullUserIsSuperadmin :: Boolean }

derive instance newtypeAppUser :: Newtype User _

derive newtype instance encodeJsonAppUser :: EncodeJson User
derive newtype instance decodeJsonAppUser :: DecodeJson User

derive newtype instance showUser :: Show User

isAdmin :: User -> Boolean
isAdmin (User { fullUserIsSuperadmin }) = fullUserIsSuperadmin

isAdminMaybe :: Maybe User -> Boolean
isAdminMaybe (Just user) = isAdmin user
isAdminMaybe Nothing = false
