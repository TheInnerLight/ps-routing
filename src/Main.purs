module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Express (listenHttp)
import Express.Effect (EXPRESS)
import Express.Response (Content(..), Header(..), Response(..))

main :: ∀ e. Eff (console :: CONSOLE, express :: EXPRESS | e) Unit
main = launchAff_ do
  _ <- listenHttp (\_ -> Response { headers: [Other "Cheese" "Biscuits"], body : Text "Cheese!" } ) 8056
  pure unit
  
  --_ <- runReaderT (listenHttp 8056) app
  --let test = map (\f -> f 6) result
