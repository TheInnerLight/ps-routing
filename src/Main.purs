module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Express (ExpressHandler, listenHttp)
import Express.Effect (EXPRESS)
import Express.Endpoint (Endpoint, createHandler, mkEndpoint)
import Express.Response (badRequest, ok, text)
import Express.Http (OK, BAD_REQUEST)
import Paths (constantPath, intPath, numberPath, (</>))

endpointTest :: forall e s. Endpoint (console :: CONSOLE | e) (ok :: OK, badRequest :: BAD_REQUEST | s)
endpointTest = mkEndpoint 
  { path : constantPath "cheese" </> intPath </> numberPath </> constantPath "blob" 
  , handler : \x y -> do
    _ <- liftEff $ log "Received endpoint request"
    if 1 < 2 then 
        pure $ ok $ text "Hi test!" 
      else 
        pure $ badRequest $ text "Urgh"
  }

main :: ∀ e. Eff (console :: CONSOLE, express :: EXPRESS | e) Unit
main = launchAff_ do
  _ <- listenHttp (createHandler endpointTest) 8056
  pure unit

