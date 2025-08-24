module FPO.UI.SmoothScroll
  ( smoothScrollToElement
  ) where

import Prelude

import Effect (Effect)

foreign import smoothScrollToElementImpl :: String -> Effect Unit

smoothScrollToElement :: String -> Effect Unit
smoothScrollToElement = smoothScrollToElementImpl
