module Components.Editor.Keybindings where

import Prelude

import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Selection as Selection
import Ace.Types as Types
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, ctrlKey, fromEvent, key)

makeBold :: Types.Editor -> Effect Unit
makeBold editor_ = surroundSelection "<*" ">" editor_

makeItalic :: Types.Editor -> Effect Unit
makeItalic editor_ = surroundSelection "</" ">" editor_

underscore :: Types.Editor -> Effect Unit
underscore editor_ = surroundSelection "<_" ">" editor_

keyBinding :: Types.Editor -> Event -> Effect Unit
keyBinding editor_ event = do
  let keyboardEvent = fromEvent event :: Maybe KeyboardEvent
  case keyboardEvent of
    Nothing -> pure unit
    Just keyEvent -> do
      let pressedKey = key keyEvent
      let ctrlKeyPressed = ctrlKey keyEvent
      if ctrlKeyPressed then
        case pressedKey of
          "b" -> makeBold editor_
          "i" -> makeItalic editor_
          _ -> pure unit
      else
        pure unit
      case pressedKey of
        "Enter" -> log "Enter" -- Placeholder for Enter key action
        "Escape" -> log "Escape" -- Placeholder for Escape key action
        _ -> pure unit
  pure unit

-- | Surrounds the selected text with the given left and right strings
-- | and positions the cursor after the inserted left text.
surroundSelection :: String -> String -> Types.Editor -> Effect Unit
surroundSelection left right ed = do
  session <- Editor.getSession ed
  selection <- Editor.getSelection ed
  range <- Selection.getRange selection
  selectedText <- Session.getTextRange range session

  -- Get the start position
  startPos <- Range.getStart range

  -- Insert the surrounded text
  let newText = left <> selectedText <> right
  Session.replace range newText session

  -- Calculate new cursor position (after the left part)
  let newColumn = (Types.getColumn startPos) + (String.length left)

  -- Move cursor to new position
  Editor.moveCursorTo (Types.getRow startPos) (Just newColumn) Nothing ed
  -- Create a new range that encompasses the entire surrounded text
  newRange <- Range.create
    (Types.getRow startPos)
    ((Types.getColumn startPos) + (String.length left))
    (Types.getRow startPos)
    ((Types.getColumn startPos) + (String.length newText) - (String.length right))

  -- Set the selection to this new range
  Selection.setSelectionRange newRange selection
