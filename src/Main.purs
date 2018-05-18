module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Express (ExpressHandler, listenHttp)
import Express.Effect (EXPRESS)
import Express.Response (ok, text)

endpoint :: ∀ e. ExpressHandler (console :: CONSOLE | e)
endpoint = do
  _ <- liftEff $ log "Received endpoint request"
  pure $ ok $ text "Hi test!" 

main :: ∀ e. Eff (console :: CONSOLE, express :: EXPRESS | e) Unit
main = launchAff_ do
  _ <- listenHttp endpoint 8056
  pure unit

