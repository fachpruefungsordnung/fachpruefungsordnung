-- | This module defines the Navigate type class and its instance for the Halogen monad.
-- | This allows us to navigate between different pages in the application.
-- |
-- | The `navigate` function is used to perform the actual navigation to a given route.
-- | This is useful for handling navigation in a clean and consistent manner across
-- | the application. The function `navigate` can be used in any monad that implements
-- | the `Navigate` type class. Particularly, it is useful in the context of Halogen
-- | components, where we can navigate to different routes based on user actions or
-- | application state changes.

module FPO.Data.Navigate where

import Prelude

import FPO.Data.Route (Route)
import Halogen (lift)
import Halogen as H

class Monad m <= Navigate m where
  -- | Navigate to a given route.
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (H.HalogenM st act slots msg m) where
  navigate = lift <<< navigate
