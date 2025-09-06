module FPO.Data.Time
    ( adjustDateTime
    , getEditTimestamp
    , formatAbsoluteTimeDetailed
    , formatRelativeTime
    ) where

import Prelude

import Data.DateTime (DateTime, adjust, date, day, diff, hour, minute, month, second, time, year)
import Data.Enum (fromEnum)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (class Duration, Seconds(..), negateDuration, toDuration)
import FPO.Dto.DocumentDto.DocDate as DocDate
import FPO.Dto.DocumentDto.DocumentHeader (DocumentHeader)
import FPO.Dto.DocumentDto.DocumentHeader as DocumentHeader

-- | Helper function to adjust a DateTime by a duration (subtract from current time)
adjustDateTime :: forall d. Duration d => d -> DateTime -> DateTime
adjustDateTime duration dt =
  fromMaybe dt $ adjust (negateDuration duration) dt

getEditTimestamp ∷ DocumentHeader → DateTime
getEditTimestamp = DocDate.docDateToDateTime <<< DocumentHeader.getLastEdited

formatAbsoluteTimeDetailed :: Maybe DateTime -> String 
formatAbsoluteTimeDetailed Nothing = "Unknown"
formatAbsoluteTimeDetailed (Just current) =
    let
        d' = date current
        t' = time current
        y = show $ fromEnum $ year d'
        m = padZero $ fromEnum $ month d'
        d = padZero $ fromEnum $ day d'
        h = padZero $ fromEnum $ hour t'
        min = padZero $ fromEnum $ minute t'
        s = padZero $ fromEnum $ second t'
    in
        y <> "." <> m <> "." <> d <> " " <> h <> ":" <> min <> ":" <> s

-- | Formats DateTime as relative time ("3 hours ago") or absolute date if > 1 week.
formatRelativeTime :: Maybe DateTime -> DateTime -> String
formatRelativeTime Nothing _ = "Unknown"
formatRelativeTime (Just current) updated =
  let
    timeDiff =
      if current > updated then diff current updated else diff updated current

    (Seconds seconds) = toDuration timeDiff :: Seconds
    totalMinutes = floor (seconds / 60.0)
    totalHours = floor (seconds / 3600.0)
    totalDays = floor (seconds / 86400.0)
  in
    if totalDays > 7 then
      formatAbsoluteDate updated
    else if totalDays >= 1 then
      show totalDays <> if totalDays == 1 then " day ago" else " days ago"
    else if totalHours >= 1 then
      show totalHours <> if totalHours == 1 then " hour ago" else " hours ago"
    else if totalMinutes >= 1 then
      show totalMinutes <>
        if totalMinutes == 1 then " minute ago" else " minutes ago"
    else
      "Just now"
  where
  -- Format DateTime as absolute date (YYYY-MM-DD)
  formatAbsoluteDate :: DateTime -> String
  formatAbsoluteDate dt =
    let
      d' = date dt
      y = show $ fromEnum $ year d'
      m = padZero $ fromEnum $ month d'
      d = padZero $ fromEnum $ day d'
    in
      d <> "." <> m <> "." <> y

padZero :: Int -> String
padZero n = if n < 10 then "0" <> show n else show n