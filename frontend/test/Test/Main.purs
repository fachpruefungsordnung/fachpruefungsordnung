module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.UI.Resizing
  ( resizeFromLeftTest
  , resizeFromRightTest
  , togglePreviewTest
  )

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] resizeFromLeftTest
  runSpecAndExitProcess [ consoleReporter ] resizeFromRightTest
  runSpecAndExitProcess [ consoleReporter ] togglePreviewTest
