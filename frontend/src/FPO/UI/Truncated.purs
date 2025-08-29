module FPO.UI.Truncated where

import Prelude

import Effect (Effect)
import Web.DOM.Document as Document
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

foreign import handleMouseEnterImpl :: Event -> Effect Unit

-- | Handles mouse enter events (i.e., the cursor entering a specific
-- | element's bounds) to add or remove title attributes for elements
-- | with the "truncate-hover-title" class.
setupTruncationListener :: Effect Unit
setupTruncationListener = do
  win <- HTML.window
  doc <- Window.document win
  let docElement = HTMLDocument.toDocument doc
  let eventTarget = Document.toEventTarget docElement
  listener <- EventTarget.eventListener handleMouseEnter
  EventTarget.addEventListener (EventType "mouseenter") listener true eventTarget

handleMouseEnter :: Event -> Effect Unit
handleMouseEnter = handleMouseEnterImpl
