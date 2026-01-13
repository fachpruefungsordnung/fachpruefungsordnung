module Test.Main where

import Prelude

import Effect (Effect)
import Test.Components.Splitview
  ( togglePreviewTest
  )
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.UI.Resizing
  ( resizeFromLeftTest
  , resizeFromRightTest
  )

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] resizeFromLeftTest
  runSpecAndExitProcess [ consoleReporter ] resizeFromRightTest
  runSpecAndExitProcess [ consoleReporter ] togglePreviewTest
