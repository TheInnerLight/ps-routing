module Express where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (class MonadReader, ask)

foreign import data Response :: Type
foreign import data Request :: Type
foreign import data ExpressApp :: Type

foreign import data EXPRESS :: Effect

foreign import _get ::  ∀ e . ExpressApp -> Request -> Eff (express :: EXPRESS | e) Response

get :: ∀ e m a. MonadReader ExpressApp m => MonadEff (express :: EXPRESS | e) m => Request -> m Response
get req = do
  app <- ask
  response <- liftEff $ _get app req
  pure response