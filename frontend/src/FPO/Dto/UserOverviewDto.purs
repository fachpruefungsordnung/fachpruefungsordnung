module FPO.Dto.UserOverviewDto where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)
import FPO.Dto.UserDto (UserID)

newtype UserOverviewDto = UserOverviewDto
  { userEmail :: String
  , userID :: UserID
  , userName :: String
  }

getName :: UserOverviewDto -> String
getName (UserOverviewDto { userName }) = userName

getEmail :: UserOverviewDto -> String
getEmail (UserOverviewDto { userEmail }) = userEmail

getID :: UserOverviewDto -> UserID
getID (UserOverviewDto { userID }) = userID

derive instance newtypeAppUser :: Newtype UserOverviewDto _

derive newtype instance encodeJsonAppUser :: EncodeJson UserOverviewDto
derive newtype instance decodeJsonAppUser :: DecodeJson UserOverviewDto
