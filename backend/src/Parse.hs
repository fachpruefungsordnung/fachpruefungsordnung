{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parseFlexibleTime
    , nonEmptyString
    , nonEmptyText
    ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- | Parse @UTCTime@ accepting different formats
parseFlexibleTime :: String -> Maybe UTCTime
parseFlexibleTime s =
    let fmts =
            [ "%Y-%m-%dT%H:%M:%SZ" -- 2025-08-24T12:34:56Z
            , "%Y-%m-%dT%H:%M:%S" -- 2025-08-24T12:34:56
            , "%Y-%m-%dT%H:%M" -- 2025-08-24T12:34
            , "%Y-%m-%d" -- 2025-08-24
            , "%H:%M:%S" -- 12:34:56
            , "%H:%M" -- 12:34
            ]
     in foldr
            (\fmt acc -> acc <|> parseTimeM True defaultTimeLocale fmt s)
            Nothing
            fmts

-- | Ensures input is not an empty string
nonEmptyString :: String -> Maybe String
nonEmptyString "" = Nothing
nonEmptyString x = Just x

-- | Ensures input is not an empty text
nonEmptyText :: Text -> Maybe Text
nonEmptyText "" = Nothing
nonEmptyText x = Just x
