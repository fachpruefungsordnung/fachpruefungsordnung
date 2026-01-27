module FPO.Data.Time
  ( adjustDateTime
  {-   , getEditTimestamp -}
  , dateToDatetime
  , formatAbsoluteTimeDetailed
  , formatRelativeTime
  , defaultFormatter
  , timeStampsVersions
  , genericDatetime
  ) where

import Prelude

import Data.DateTime
  ( Date
  , DateTime(..)
  , Time(..)
  , adjust
  , canonicalDate
  , diff
  )
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, format, formatDateTime)
import Data.Formatter.DateTime as FDT
import Data.Int (floor)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (class Duration, Seconds(..), negateDuration, toDuration)

{- import FPO.Dto.DocumentDto.DocDate as DocDate -}
{- import FPO.Dto.DocumentDto.DocumentHeader (DocumentHeader)
import FPO.Dto.DocumentDto.DocumentHeader as DocumentHeader -}

-- for cases that need to be handled even though one case cannot happen. This Data is a placeholder that can be used in
-- such places
genericDatetime :: DateTime
genericDatetime = DateTime genericDate genericTime

genericDate :: Date
genericDate = canonicalDate bottom bottom bottom

genericTime :: Time
genericTime = Time bottom bottom bottom bottom

dateToDatetime :: Date -> DateTime
dateToDatetime d = DateTime d (Time bottom bottom bottom bottom)

-- | Helper function to adjust a DateTime by a duration (subtract from current time)
adjustDateTime :: forall d. Duration d => d -> DateTime -> DateTime
adjustDateTime duration dt =
  fromMaybe dt $ adjust (negateDuration duration) dt

{- getEditTimestamp ∷ DocumentHeader → DateTime
getEditTimestamp = DocDate.docDateToDateTime <<< DocumentHeader.getLastEdited -}

formatAbsoluteTimeDetailed :: forall d. Duration d => Maybe d -> DateTime -> String
formatAbsoluteTimeDetailed offset dateTime =
  let
    dTime = fromMaybe dateTime case offset of
      Just oSet -> adjust (negateDuration oSet) dateTime
      Nothing -> Just dateTime
  in
    format defaultFormatter dTime

defaultFormatter :: Formatter
defaultFormatter =
  ( FDT.DayOfMonthTwoDigits
      : FDT.Placeholder ". "
      : FDT.MonthShort
      : FDT.Placeholder ". "
      : FDT.YearFull
      : FDT.Placeholder " "
      : FDT.Hours24
      : FDT.Placeholder ":"
      : FDT.MinutesTwoDigits
      : Nil
  )

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
      format formatAbsoluteDate updated
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
  formatAbsoluteDate :: Formatter
  formatAbsoluteDate =
    ( FDT.DayOfMonthTwoDigits
        : FDT.Placeholder ". "
        : FDT.MonthShort
        : FDT.Placeholder ". "
        : FDT.YearFull
        : Nil
    )

-- TODO create more timestamps versions and discuss, where to store this
timeStampsVersions :: Array String
timeStampsVersions =
  [ "DD.MMM.YY HH:mm"
  , "DD.MM.YY HH:mm"
  , "DD/MMM/YY HH:mm"
  , "DD/MM/YY HH:mm"
  , "MM/DD/YY HH:mm"
  , "MMM/DD/YY HH:mm"
  , "YY/MMM/DD HH:mm"
  , "YY/MM/DD HH:mm"
  ]
