module Express.Endpoint(Endpoint, mkEndpoint, createHandler) where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Express (ExpressHandler)
import Express.Http (HttpError(..), NOT_FOUND, kind Status)
import Express.Request (pathOf)
import Express.Response (notFound, text)
import Paths (class Path, runRoute)

newtype Endpoint eff s = HiddenEndpoint (ExpressHandler eff s) 

mkEndpoint :: forall p h eff s. Path p h (ExpressHandler eff (notFound :: NOT_FOUND | s)) => {path :: p, handler :: h} -> Endpoint eff (notFound :: NOT_FOUND | s)
mkEndpoint {path : path, handler : handler} = HiddenEndpoint do
  req <- ask
  let pathStr = fixpath $ pathOf req
  case runRoute handler pathStr path of
    Just eh -> eh
    Nothing -> throwError $ UnknownPath
  where 
    fixpath str =
      fromMaybe str $ stripPrefix (Pattern "/") $ str
    

createHandler :: forall eff s. Endpoint eff s -> ExpressHandler eff s
createHandler (HiddenEndpoint endpoint) = endpoint