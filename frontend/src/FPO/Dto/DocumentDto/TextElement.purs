module FPO.Dto.DocumentDto.TextElement where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import FPO.Dto.DocumentDto.DocDate (DocDate)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID, User)

type TextElementID = Int

newtype TextElementInfo = TEI
  { documentID :: DocumentID, textElementID :: TextElementID }

derive newtype instance decodeJsonTextElementInfo :: DecodeJson TextElementInfo

newtype TextElementHistory = TEH
  { author :: User, identifier :: Int, timestamp :: DocDate }

derive newtype instance decodeJsonTextElementHistory :: DecodeJson TextElementHistory

newtype FullTextElementHistory = FTEH
  { history :: Array TextElementHistory, info :: TextElementInfo }

-- derive newtype instance decodeJsonFullTextElementHistory :: DecodeJson FullTextElementHistory

instance decodeJsonFullTextElementHistory :: DecodeJson FullTextElementHistory where
  decodeJson json = do
    obj <- decodeJson json
    history <- obj .: "history"
    info <- obj .: "textElement"
    pure $ FTEH { history, info }

getHistoryElementID :: TextElementHistory -> Int
getHistoryElementID (TEH teh) = teh.identifier

getHistoryElementTimestamp :: TextElementHistory -> DocDate
getHistoryElementTimestamp (TEH teh) = teh.timestamp

getTEHsFromFTEH :: FullTextElementHistory -> Array TextElementHistory
getTEHsFromFTEH (FTEH fteh) = fteh.history

