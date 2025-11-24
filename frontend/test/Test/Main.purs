module Test.Main where

import Prelude

import Effect (Effect)
import Test.Components.Splitview (spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec 
