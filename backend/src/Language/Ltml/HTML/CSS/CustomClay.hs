{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CustomClay
    ( -- * @Css@ Rendering
      renderStrict

      -- * EDSL for CSS Counters
    , Counter (..)
    , counter
    , counterNum
    , counterChar
    , counterCharCapital
    , stringCounter
    , counterReset
    , counterIncrement

      -- * Custom CSS Values
    , alignLeft
    , alignRight
    , displayContents
    , autoLayout

      -- * Custom CSS Properties
    , gap
    , scrollMarginTop
    , tableLayout
    , justifyItems
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

autoLayout :: Position
autoLayout = other "auto"

-------------------------------------------------------------------------------

gap :: Size LengthUnit -> Css
gap s = "gap" -: unPlain (unValue $ value s)

scrollMarginTop :: Size LengthUnit -> Css
scrollMarginTop s = "scroll-margin-top" -: unPlain (unValue $ value s)

tableLayout :: Position -> Css
tableLayout arg = "table-layout" -: unPlain (unValue $ value arg)

justifyItems :: JustifyContentValue -> Css
justifyItems arg = "justify-items" -: unPlain (unValue $ value arg)
