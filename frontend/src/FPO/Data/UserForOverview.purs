module FPO.Data.UserForOverview where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

newtype UserForOverview = UserForOverview
  { userEmail :: String
  , userID :: String
  , userName :: String
  }

getName :: UserForOverview -> String
getName (UserForOverview { userName }) = userName

getEmail :: UserForOverview -> String
getEmail (UserForOverview { userEmail }) = userEmail

derive instance newtypeAppUser :: Newtype UserForOverview _

derive newtype instance encodeJsonAppUser :: EncodeJson UserForOverview
derive newtype instance decodeJsonAppUser :: DecodeJson UserForOverview
