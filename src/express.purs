module Express where

import Prelude

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader as MR
import Control.Monad.State (StateT(..))
import Control.Monad.State as MS

foreign import data Response :: Type
foreign import data Request :: Type
foreign import data ExpressApp :: Type
foreign import data EXPRESS :: Effect

foreign import makeApp :: ∀ e . Eff (express :: EXPRESS | e) ExpressApp

type ExpressMonad e m = StateT Request (ReaderT ExpressApp (Eff (express :: EXPRESS | e))) m

listenHttp :: ∀ e m. MR.MonadReader ExpressApp m => MonadAff (express :: EXPRESS | e) m => Int -> m Unit
listenHttp port = do
  app <- MR.ask
  liftAff $ fromEffFnAff $ _listenHttp app port

get :: ∀ e m a. MR.MonadReader ExpressApp m => MS.MonadState Request m => MonadEff (express :: EXPRESS | e) m => m Response
get = do
  app <- MR.ask
  req <- MS.get
  response <- liftEff $ _get app req
  pure response

foreign import _get :: ∀ e . ExpressApp -> Request -> Eff (express :: EXPRESS | e) Response

foreign import _listenHttp :: ∀ e m. ExpressApp -> Int -> EffFnAff (express :: EXPRESS | e) Unit