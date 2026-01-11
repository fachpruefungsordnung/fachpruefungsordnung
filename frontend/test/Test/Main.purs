module Test.Main where

import Prelude

import Effect (Effect)
import Test.Components.Splitview
  ( handleWindowResizeTest
  , resizeFromLeftTest
  , resizeFromRightTest
  , togglePreviewTest
  )
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] resizeFromLeftTest
  runSpecAndExitProcess [ consoleReporter ] resizeFromRightTest
  runSpecAndExitProcess [ consoleReporter ] togglePreviewTest
  runSpecAndExitProcess [ consoleReporter ] handleWindowResizeTest
