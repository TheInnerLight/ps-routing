module Main where

import Prelude

import Control.Monad.Aff ( launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Reader (runReaderT)
import Express (EXPRESS, listenHttp, makeApp)


main :: ∀ e. Eff (console :: CONSOLE, express :: EXPRESS | e) Unit
main = launchAff_ do
  app <- liftEff $ makeApp
  _ <- runReaderT (listenHttp 8056) app
  pure unit
  
  --_ <- runReaderT (listenHttp 8056) app
  --let test = map (\f -> f 6) result
