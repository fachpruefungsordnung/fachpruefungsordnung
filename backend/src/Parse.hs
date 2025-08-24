{-# LANGUAGE OverloadedStrings #-}

module Parse (parseFlexibleTime) where

import Control.Applicative ((<|>))
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

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
