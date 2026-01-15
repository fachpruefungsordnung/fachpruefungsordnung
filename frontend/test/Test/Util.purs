module Test.Util
  ( shouldBeNear
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Number (abs)
import Effect.Exception (Error)
import Test.Spec.Assertions (shouldEqual)

shouldBeNear :: forall m. MonadThrow Error m => Number -> Number -> m Unit
shouldBeNear expected actual = (abs (expected - actual) < 0.0001) `shouldEqual` true
