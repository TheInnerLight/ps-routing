module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(..), fromFoldable)
import Express (listenHttp)
import Express.Application (ApplicationInternal(..), run)
import Express.Effect (EXPRESS)
import Express.Endpoint (Endpoint, createHandler, mkEndpoint)
import Express.Http (NOT_FOUND, OK)
import Express.Response (ok, text)
import Paths (constantPath, intPath, numberPath, (</>))

endpointTest :: forall e s. Endpoint (console :: CONSOLE | e) (ok :: OK, notFound :: NOT_FOUND | s)
endpointTest = mkEndpoint 
  { path : constantPath "your-number" </> intPath
  , handler : \x-> do
    _ <- liftEff $ log "Received number endpoint request"
    pure $ ok $ text $ "Your number is " <> show x
  }

endpointTest2 :: forall e s. Endpoint (console :: CONSOLE | e) (ok :: OK, notFound :: NOT_FOUND | s)
endpointTest2 = mkEndpoint 
  { path : constantPath "cheese"
  , handler : do
    _ <- liftEff $ log "Received cheese endpoint request"
    pure $ ok $ text "mimolette"
  }

--application :: forall e. ApplicationInternal (console :: CONSOLE | e)
application = ApplicationInternal (fromFoldable [endpointTest, endpointTest2])

main :: ∀ e. Eff (console :: CONSOLE, express :: EXPRESS | e) Unit
main = launchAff_ do
  _ <- run application
  pure unit

