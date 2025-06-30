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
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, ctrlKey, fromEvent, key, shiftKey)

makeBold :: Types.Editor -> Effect Unit
makeBold editor_ = do
  isSurrounded <- isSelectionSurrounded "<*" ">" editor_
  if isSurrounded then
    -- If already surrounded, remove the surrounding tags
    deleteSurrounding "<*" ">" editor_
  else
    -- Otherwise, surround the selection with bold tags
    surroundSelection "<*" ">" editor_

makeItalic :: Types.Editor -> Effect Unit
makeItalic editor_ = do
  isSurrounded <- isSelectionSurrounded "</" ">" editor_
  if isSurrounded then
    -- If already surrounded, remove the surrounding tags
    deleteSurrounding "</" ">" editor_
  else
    -- Otherwise, surround the selection with bold tags
    surroundSelection "</" ">" editor_

underscore :: Types.Editor -> Effect Unit
underscore editor_ = do
  isSurrounded <- isSelectionSurrounded "<_" ">" editor_
  if isSurrounded then
    -- If already surrounded, remove the surrounding tags
    deleteSurrounding "<_" ">" editor_
  else
    -- Otherwise, surround the selection with bold tags
    surroundSelection "<_" ">" editor_

keyBinding :: Types.Editor -> Event -> Effect Unit
keyBinding editor_ event = do
  let keyboardEvent = fromEvent event :: Maybe KeyboardEvent
  case keyboardEvent of
    Nothing -> pure unit
    Just keyEvent -> do
      let pressedKey = key keyEvent
      let ctrlKeyPressed = ctrlKey keyEvent
      let shiftKeyPressed = shiftKey keyEvent

      if ctrlKeyPressed && not shiftKeyPressed then
        case pressedKey of
          "b" -> makeBold editor_
          "i" -> makeItalic editor_
          "z" -> do
            Editor.undo editor_
            Editor.focus editor_
          "Z" -> do
            Editor.undo editor_
            Editor.focus editor_
          _ -> pure unit
      else pure unit
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

-- | Checks if the selected text is surrounded by the given left and right strings.
isSelectionSurrounded :: String -> String -> Types.Editor -> Effect Boolean
isSelectionSurrounded left right ed = do
  session <- Editor.getSession ed
  selection <- Editor.getSelection ed
  range <- Selection.getRange selection
  selectedText <- Session.getTextRange range session

  let leftLength = String.length left
  let rightLength = String.length right

  let selectedTextLength = String.length selectedText
  let isLeftCorrect = String.take leftLength selectedText == left
  let
    isRightCorrect = String.lastIndexOf (String.Pattern right) selectedText == Just
      (selectedTextLength - rightLength)

  rangeStart <- Range.getStart range
  rangeEnd <- Range.getEnd range
  rangeWithSurrounding <- Range.create
    (Types.getRow rangeStart)
    ((Types.getColumn rangeStart) - String.length left)
    (Types.getRow rangeEnd)
    ((Types.getColumn rangeEnd) + String.length right)
  selectedTextSurr <- Session.getTextRange rangeWithSurrounding session
  let selectedTextLengthSurr = String.length selectedTextSurr
  let isLeftCorrectSurr = String.take leftLength selectedTextSurr == left
  let
    isRightCorrectSurr = String.lastIndexOf (String.Pattern right) selectedTextSurr ==
      Just (selectedTextLengthSurr - rightLength)

  pure
    ( isLeftCorrect && isRightCorrect && selectedTextLength >=
        (leftLength + rightLength)
        || isLeftCorrectSurr && isRightCorrectSurr && selectedTextLengthSurr >=
          (leftLength + rightLength)
    )

-- | Deletes the surrounding left and right strings from the selected text.
deleteSurrounding :: String -> String -> Types.Editor -> Effect Unit
deleteSurrounding left right ed = do
  session <- Editor.getSession ed
  selection <- Editor.getSelection ed
  range <- Selection.getRange selection
  selectedText <- Session.getTextRange range session

  rangeStart <- Range.getStart range
  rangeEnd <- Range.getEnd range

  let leftLength = String.length left
  let rightLength = String.length right

  if (String.take leftLength selectedText /= left) then do
    -- if the left part is not present, that means we highlighted the inner part of the surrounding
    -- so we need to delete the surrounding from a bigger range 
    rangeWithSurrounding <- Range.create
      (Types.getRow rangeStart)
      ((Types.getColumn rangeStart) - leftLength)
      (Types.getRow rangeEnd)
      ((Types.getColumn rangeEnd) + rightLength)
    selectedTextSurr <- Session.getTextRange rangeWithSurrounding session
    let selectedTextLength = String.length selectedTextSurr
    let
      newText = String.drop leftLength
        (String.take (selectedTextLength - rightLength) selectedTextSurr)

    Session.replace rangeWithSurrounding newText session

    newSelectionRange <- Range.create
      (Types.getRow rangeStart)
      ((Types.getColumn rangeStart) - leftLength)
      (Types.getRow rangeEnd)
      ((Types.getColumn rangeEnd) - leftLength)
    Selection.setSelectionRange newSelectionRange selection

  else do
    let selectedTextLength = String.length selectedText
    let
      newText = String.drop leftLength
        (String.take (selectedTextLength - rightLength) selectedText)

    Session.replace range newText session

    newSelectionRange <- Range.create
      (Types.getRow rangeStart)
      (Types.getColumn rangeStart)
      (Types.getRow rangeEnd)
      ((Types.getColumn rangeEnd) - leftLength - rightLength)
    Selection.setSelectionRange newSelectionRange selection
