module FPO.UI.Truncated where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf)
import Effect (Effect)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

setupTruncationListener :: Effect Unit
setupTruncationListener = do
  win <- HTML.window
  doc <- Window.document win
  let docElement = HTMLDocument.toDocument doc
  let eventTarget = Document.toEventTarget docElement
  listener <- EventTarget.eventListener handleMouseEnter
  EventTarget.addEventListener (EventType "mouseenter") listener true eventTarget

handleMouseEnter :: Event -> Effect Unit
handleMouseEnter event = case Event.target event of
  Nothing -> pure unit
  Just target -> case Element.fromEventTarget target of
    Nothing -> pure unit
    Just element -> case HTMLElement.fromElement element of
      Nothing -> pure unit
      Just htmlElement -> do
        className <- Element.className element
        when (contains "text-truncate" className) do
          let el = HTMLElement.toElement htmlElement
          cw <- Element.clientWidth el
          sw <- Element.scrollWidth el
          if cw < sw then do
            text <- Node.textContent (Element.toNode element)
            Element.setAttribute "title" text element
          else
            Element.removeAttribute "title" element
  where
    -- TODO: just use something like `unwords`...
    contains needle haystack = 
      case indexOf (Pattern needle) haystack of
        Just _ -> true
        Nothing -> false
