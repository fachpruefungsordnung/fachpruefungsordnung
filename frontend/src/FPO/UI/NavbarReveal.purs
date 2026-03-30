module FPO.UI.NavbarReveal
  ( setupNavbarReveal
  , teardownNavbarReveal
  ) where

import Prelude

import Effect (Effect)

foreign import setupNavbarRevealImpl :: Int -> Effect Unit
foreign import teardownNavbarRevealImpl :: Effect Unit

-- | Sets up the navbar auto-hide/reveal behaviour.  The navbar is hidden
-- | initially and slides in once the user scrolls past `threshold` pixels.
-- | Call `teardownNavbarReveal` to undo all DOM changes.
setupNavbarReveal :: Int -> Effect Unit
setupNavbarReveal = setupNavbarRevealImpl

-- | Removes the scroll listener and restores the navbar to its normal
-- | (always-visible) state.  Safe to call even if `setupNavbarReveal` was
-- | never called.
teardownNavbarReveal :: Effect Unit
teardownNavbarReveal = teardownNavbarRevealImpl
