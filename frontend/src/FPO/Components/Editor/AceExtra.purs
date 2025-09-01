module FPO.Components.Editor.AceExtra
( addClass
, clearSelection
, removeClass
, screenToText
, textToScreen
, setAnchorPosition
) where

import Prelude
import Effect (Effect)
import Ace.Types as Types
import Web.HTML.HTMLElement (HTMLElement)

-- For CSS Styling
foreign import addClass    :: HTMLElement -> String -> Effect Unit
foreign import removeClass :: HTMLElement -> String -> Effect Unit

-- Comment Section dragger

foreign import screenToText
  :: Types.Editor -> Number -> Number -> Effect Types.Position


foreign import textToScreen
  :: Types.Editor -> Int -> Int -> Effect { pageX :: Number, pageY :: Number }


foreign import setAnchorPosition
  :: Types.Anchor -> Int -> Int -> Effect Unit

foreign import clearSelection :: Types.Editor -> Effect Unit