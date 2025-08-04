module FPO.Dto.DocumentDto.DocDate where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Time (Time(..))
import Data.Tuple (fst)
import Parsing (ParserT, fail, runParserT)
import Parsing.String (anyTill, char, rest)

newtype DocDate = DocDate DateTime

docDateToDateTime :: DocDate -> DateTime
docDateToDateTime (DocDate date) = date

timeParser :: forall m a. BoundedEnum a => Monad m => Char -> ParserT String m a
timeParser c = do
  res <- anyTill (char c)
  h res
  where
  h string = case fromString $ fst (string) of
    Nothing -> fail "can't parse number"
    Just num -> case toEnum num of
      Nothing -> fail "not valid"
      Just a -> pure a

dateParser :: forall m. Monad m => ParserT String m DateTime
dateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeParser 'T'
  hour <- timeParser ':'
  minute <- timeParser ':'
  second <- timeParser '.'
  _ <- rest
  case (toEnum 0) of
    Nothing -> fail "not valid"
    Just g -> pure $ DateTime (canonicalDate year month day)
      (Time hour minute second g)

instance decodeJsonDateTime :: DecodeJson DocDate where
  decodeJson json = do
    obj <- decodeJson json
    result <- runParserT obj dateParser
    case result of
      Left _ -> Left (UnexpectedValue json)
      Right datetime -> Right $ DocDate datetime

derive newtype instance eqDocDate :: Eq DocDate
derive newtype instance ordDocDate :: Ord DocDate
