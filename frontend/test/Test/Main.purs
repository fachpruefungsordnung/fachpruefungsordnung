module Test.Main where

import Prelude

import Effect (Effect)
import Test.Routing (routeCodecTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.UI.Resizing
  ( resizeFromLeftTest
  , resizeFromRightTest
  , togglePreviewTest
  , toggleSidebarTest
  )

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] routeCodecTests
  runSpecAndExitProcess [ consoleReporter ] resizeFromLeftTest
  runSpecAndExitProcess [ consoleReporter ] resizeFromRightTest
  runSpecAndExitProcess [ consoleReporter ] togglePreviewTest
  runSpecAndExitProcess [ consoleReporter ] toggleSidebarTest
