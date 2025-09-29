module FPO.UI.SmoothScroll
  ( smoothScrollToElement
  ) where

import Prelude

import Effect (Effect)

foreign import smoothScrollToElementImpl :: String -> Effect Unit

-- | Smoothly scrolls the page to the element with the given ID.
-- | To use, simply add some ID to an element and call this function with that ID.
-- | Example: `smoothScrollToElement "features"` will scroll to `<div id="features">...</div>`
-- |          (configured via `HP.id "features"`).
smoothScrollToElement :: String -> Effect Unit
smoothScrollToElement = smoothScrollToElementImpl
