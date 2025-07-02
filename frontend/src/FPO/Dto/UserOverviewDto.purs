module FPO.Dto.UserOverviewDto where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

newtype UserOverviewDto = UserOverviewDto
  { userEmail :: String
  , userID :: String
  , userName :: String
  }

getName :: UserOverviewDto -> String
getName (UserOverviewDto { userName }) = userName

getEmail :: UserOverviewDto -> String
getEmail (UserOverviewDto { userEmail }) = userEmail

getID :: UserOverviewDto -> String
getID (UserOverviewDto { userID }) = userID

derive instance newtypeAppUser :: Newtype UserOverviewDto _

derive newtype instance encodeJsonAppUser :: EncodeJson UserOverviewDto
derive newtype instance decodeJsonAppUser :: DecodeJson UserOverviewDto
