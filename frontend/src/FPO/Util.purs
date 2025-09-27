-- | General utility functions.

module FPO.Util where

import Prelude

import Control.Alternative (guard)
import Data.Array (length, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect.Class (class MonadEffect)
import Halogen as H
import Web.HTML.HTMLElement as WHE
import Web.UIEvent.KeyboardEvent as KE

-- | Prepends an element to an array if the condition is true.
prependIf :: forall a. Boolean -> a -> Array a -> Array a
prependIf cond x xs = (guard cond $> x) <> xs

isPrefixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isPrefixOf prefix arr =
  take (length prefix) arr == prefix

isRealPrefixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isRealPrefixOf prefix arr =
  let
    len = length prefix
  in
    len > 0 && isPrefixOf prefix arr && length arr > len

-- | Creates a singleton array if the condition is true, otherwise an empty array.
singletonIf :: forall a. Boolean -> a -> Array a
singletonIf cond x = if cond then [ x ] else []

-- | Handles key down events for modals. In general, "Enter" confirms the action,
-- | "Escape" cancels it, and any other key does nothing.
handleKeyDown
  :: forall action
   . action
  -> action
  -> action
  -> KE.KeyboardEvent
  -> action
handleKeyDown cancel confirm doNothing event
  | KE.key event == "Enter" = confirm
  | KE.key event == "Escape" = cancel
  | otherwise = doNothing

-- | Similar to 'handleKeyDown', but without the confirm action. That is,
-- | this keydown handler can be used to cancel a modal with "Escape".
handleKeyDownEscape
  :: forall action
   . action
  -> action
  -> KE.KeyboardEvent
  -> action
handleKeyDownEscape cancel doNothing event
  | KE.key event == "Escape" = cancel
  | otherwise = doNothing

-- | Focuses the given reference label, if it exists.
focusRef
  :: ∀ state action slots output m
   . MonadEffect m
  => H.RefLabel
  -> H.HalogenM state action slots output m Unit
focusRef refLabel = do
  H.getHTMLElementRef refLabel >>= case _ of
    Just element -> do
      H.liftEffect $ WHE.focus element
    Nothing -> pure unit

-- | Stricter email validation that requires a proper domain with TLD.
-- | This validates that the email has:
-- | - A local part (before @)
-- | - A domain name (after @)
-- | - A top-level domain (at least 2 characters after the last dot)
isValidEmailStrict :: String -> Boolean
isValidEmailStrict email =
  case regex "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" noFlags of
    Right r -> test r email
    Left _ -> false

