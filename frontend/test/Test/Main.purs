module Test.Main where

import Prelude

import Effect (Effect)
import Test.Components.Splitview (resizeFromLeftTest, resizeFromRightTest)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] resizeFromLeftTest
  runSpecAndExitProcess [ consoleReporter ] resizeFromRightTest
