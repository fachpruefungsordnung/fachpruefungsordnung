module FPO.Dto.DocumentDto.DocDate where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Date (Date(..), canonicalDate)
import Data.DateTime
  ( DateTime(..)
  , date
  , day
  , hour
  , millisecond
  , minute
  , month
  , second
  , time
  , year
  )
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (length)
import Data.String.Utils (repeat)
import Data.Time (Time(..))
import Data.Tuple (fst)
import Parsing (ParserT, fail, runParserT)
import Parsing.String (anyTill, char, rest)

newtype DocDate = DocDate DateTime

-- Date format example: "2025-08-14T17:24:55.895359Z"
toStringFormat :: DocDate -> String
toStringFormat docDate =
  (show $ fromEnum yearComponent) <> "-"
    <>
      (fillToTwo $ show $ fromEnum monthComponent)
    <> "-"
    <>
      (fillToTwo $ show $ fromEnum dayComponent)
    <> "T"
    <>
      (fillToTwo $ show $ fromEnum hourComponent)
    <> ":"
    <>
      (fillToTwo $ show $ fromEnum minuteComponent)
    <> ":"
    <>
      (fillToTwo $ show $ fromEnum secondComponent)
    <> "."
    <>
      (fillMilli $ show $ fromEnum millisecondComponent)
    <> "999Z"
  where
  dateComponent = date (docDateToDateTime docDate)
  timeComponent = time (docDateToDateTime docDate)
  yearComponent = year dateComponent
  monthComponent = month dateComponent
  dayComponent = day dateComponent
  hourComponent = hour timeComponent
  minuteComponent = minute timeComponent
  secondComponent = second timeComponent
  millisecondComponent = millisecond timeComponent
  fillToTwo num = if (length num < 2) then ("0" <> num) else num
  fillMilli num = case repeat (3 - length num) "9" of
    Nothing -> "999999"
    Just res -> num <> res

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

timeEndParser :: forall m a. BoundedEnum a => Monad m => ParserT String m a
timeEndParser = do
  res <- rest
  h res
  where
  h string = case fromString $ string of
    Nothing -> fail "can't parse number"
    Just num -> case toEnum num of
      Nothing -> fail "not valid"
      Just a -> pure a

shortDateParser :: forall m. Monad m => ParserT String m Date
shortDateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeEndParser
  pure $ canonicalDate year month day

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

getYear :: DocDate -> Int
getYear d = fromEnum $ year $ date $ docDateToDateTime d

getMonth :: DocDate -> Int
getMonth d = fromEnum $ month $ date $ docDateToDateTime d

getDay :: DocDate -> Int
getDay d = fromEnum $ day $ date $ docDateToDateTime d

docYear :: DocDate -> Int
docYear d = fromEnum $ year $ date $ docDateToDateTime d

docMonth :: DocDate -> Int
docMonth d = fromEnum $ month $ date $ docDateToDateTime d

docDay :: DocDate -> Int
docDay d = fromEnum $ day $ date $ docDateToDateTime d

docHour :: DocDate -> Int
docHour d = fromEnum $ hour $ time $ docDateToDateTime d

docMinute :: DocDate -> Int
docMinute d = fromEnum $ minute $ time $ docDateToDateTime d

docSecond :: DocDate -> Int
docSecond d = fromEnum $ second $ time $ docDateToDateTime d

derive newtype instance eqDocDate :: Eq DocDate
derive newtype instance ordDocDate :: Ord DocDate
derive newtype instance showDocDate :: Show DocDate
