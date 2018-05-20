module Express.Endpoint where

import Prelude

import Control.Monad.Eff (kind Effect)
import Data.Exists (Exists, mkExists)
import Express (ExpressHandler)
import Express.Status (kind Status)
import Paths (class Path)

newtype InternalEndpoint h p = InternalEndpoint
  { path :: p
  , handler :: h
  }

newtype Endpoint h = Endpoint (Exists (InternalEndpoint h))

mkEndpoint :: forall p h eff r. Path p h (ExpressHandler eff r) => {path :: p, handler :: h}  -> Endpoint h
mkEndpoint {path : path, handler : handler} = Endpoint $ mkExists $ InternalEndpoint
  { path : path
  , handler : handler
  }

--endpoint :: forall p e r a. Path p a (Endpoint p (ExpressHandler e r)) => p -> a -> Endpoint p a
--endpoint path handler =
--    Endpoint {path : path, handler : handler}