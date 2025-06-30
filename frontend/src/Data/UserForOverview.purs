module FPO.Data.UserForOverview where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

newtype UserForOverview = UserForOverview
  { userEmail :: String
  , userPwhash :: String
  , userName :: String
  }

getName :: UserForOverview -> String
getName (UserForOverview { userName }) = userName

derive instance newtypeAppUser :: Newtype UserForOverview _

derive newtype instance encodeJsonAppUser :: EncodeJson UserForOverview
derive newtype instance decodeJsonAppUser :: DecodeJson UserForOverview
