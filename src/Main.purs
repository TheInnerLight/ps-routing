module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (fromMaybe)
import Paths ((</>), constantR, numberR)
import Paths as R

testHandler :: ∀ e. Number -> Number -> Eff (console :: CONSOLE | e) Unit
testHandler x y = log $ "Did some shit " <> show x <> " " <> show y

main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main = do
  let combined = constantR "test" </> numberR </> numberR
  let result = R.runRoute (testHandler) "test/21.7e6/1" combined
  let result2 = R.runRoute (log "test") "test" (constantR "test")
  --let test = map (\f -> f 6) result
  fromMaybe (log "Nothing") $ result
