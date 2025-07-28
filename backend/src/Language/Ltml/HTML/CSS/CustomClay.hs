{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CustomClay
    ( Counter (..)
    , counter
    , counterNum
    , counterChar
    , stringCounter
    , counterReset
    , counterIncrement
    ) where

import Clay
import Data.Text (Text)

counterReset :: Text -> Css
counterReset t = "counter-reset" -: t

counterIncrement :: Text -> Css
counterIncrement t = "counter-increment" -: t

-- | Type for concatinating strings (e.g. "(") with counters (e.g. counter(item))
newtype Counter = Counter {unCounter :: Text}

-- | Translates Counter into actual CSS property
-- (uses CSS content)  
counter :: Counter -> Css
counter c = "content" -: unCounter c

counterNum :: Text -> Counter
counterNum t = Counter $ "counter(" <> t <> ")"

counterChar :: Text -> Counter
counterChar t = Counter $ "counter(" <> t <> ", lower-alpha)"

stringCounter :: Text -> Counter
stringCounter t = Counter $ "\"" <> t <> "\""

instance Semigroup Counter where
    Counter a <> Counter b = Counter (a <> b)