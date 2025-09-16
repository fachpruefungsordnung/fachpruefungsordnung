{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CustomClay
    ( renderStrict
    , Counter (..)
    , counter
    , counterNum
    , counterChar
    , counterCharCapital
    , stringCounter
    , counterReset
    , counterIncrement
    , alignLeft
    , alignRight
    , displayContents
    , gap
    ) where

import Clay hiding (a, b, s)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

renderStrict :: Css -> Text
renderStrict = toStrict . render

-------------------------------------------------------------------------------

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

counterCharCapital :: Text -> Counter
counterCharCapital t = Counter $ "counter(" <> t <> ", upper-alpha)"

stringCounter :: Text -> Counter
stringCounter t = Counter $ "\"" <> t <> "\""

instance Monoid Counter where
    mempty = Counter ""

instance Semigroup Counter where
    Counter a <> Counter b = Counter (a <> b)

-------------------------------------------------------------------------------

alignLeft :: TextAlign
alignLeft = other "left"

alignRight :: TextAlign
alignRight = other "right"

displayContents :: Display
displayContents = other "contents"

-------------------------------------------------------------------------------

gap :: Size LengthUnit -> Css
gap s = "gap" -: unPlain (unValue $ value s)
