module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Paths (pathTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  pathTests
