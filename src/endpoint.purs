module Express.Endpoint(Endpoint, mkEndpoint, createHandler) where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Reader (ask)
import Data.Exists (Exists, mkExists, runExists)
import Express (ExpressHandler)
import Express.Http (NOT_FOUND, kind Status)
import Express.Request (Request(..), pathOf)
import Express.Response (notFound, ok, text)
import Paths (class Path)

newtype InternalEndpoint h p = InternalEndpoint
  { path :: p
  , handler :: h
  }

newtype EndpointIntermediate h = EndpointIntermediate (Exists (InternalEndpoint h))

newtype Endpoint (effects:: #Effect) (statuses :: #Status) = Endpoint (Exists EndpointIntermediate)

mkEndpoint :: forall p h eff r. Path p h (ExpressHandler eff r) => {path :: p, handler :: h}  -> Endpoint eff r
mkEndpoint {path : path, handler : handler} = Endpoint $ mkExists $ EndpointIntermediate $ mkExists $ InternalEndpoint
  { path : path
  , handler : handler
  }

createHandler :: forall eff r. Endpoint eff r -> (ExpressHandler eff (notFound :: NOT_FOUND | r))
createHandler (Endpoint ep) =
  runExists (\(EndpointIntermediate ei) -> runExists (f) ei) ep
  where 
    f :: forall h p. InternalEndpoint h p -> (ExpressHandler eff (notFound :: NOT_FOUND | r))
    f (InternalEndpoint {path : path, handler : handler}) = do
      req <- ask
      let path = pathOf req
      pure $ notFound $ text "Not found."



--endpoint :: forall p e r a. Path p a (Endpoint p (ExpressHandler e r)) => p -> a -> Endpoint p a
--endpoint path handler =
--    Endpoint {path : path, handler : handler}