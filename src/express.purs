module Express where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader as MR
import Express.Effect (EXPRESS)
import Express.Response (EResponse, Response, genNativeResponse)
foreign import data Request :: Type
foreign import data ExpressApp :: Type


listenHttp :: ∀ e m b. (Request -> Response b) -> MonadAff (express :: EXPRESS | e) m => Int -> m Unit
listenHttp f port = do
  app <- liftEff _makeApp
  _ <- liftEff $ _get app $ genCallback f
  liftAff $ fromEffFnAff $ _listenHttp app port

get :: ∀ e m a. MR.MonadReader ExpressApp m => MonadEff (express :: EXPRESS | e) m => m EResponse
get = do
  app <- MR.ask
  response <- liftEff $ _get app (\_ _ -> pure unit)
  pure response

genCallback :: ∀ e b . (Request -> Response b) -> (Request -> EResponse -> Eff (express :: EXPRESS | e) Unit)
genCallback f =
  \req resp -> 
    genNativeResponse (f req) resp *> pure unit

foreign import _makeApp :: ∀ e . Eff (express :: EXPRESS | e) ExpressApp
foreign import _get :: ∀ e . ExpressApp -> (Request -> EResponse -> Eff (express :: EXPRESS | e) Unit) -> Eff (express :: EXPRESS | e) EResponse
foreign import _listenHttp :: ∀ e m. ExpressApp -> Int -> EffFnAff (express :: EXPRESS | e) Unit