module Express where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader as MR
import Express.Effect (EXPRESS)
import Express.Request (ExRequest, Request(..), makeRequest)
import Express.Response (EResponse, Response, genNativeResponse)

foreign import data ExpressApp :: Type

-- | An express handler is a type capable of reading an http request, performing asynchonrous effects and which ultimately returns an http response
type ExpressHandler e r = ReaderT Request (Aff (express :: EXPRESS | e)) (Response r)

listenHttp :: ∀ e r m b. ExpressHandler e r -> MonadAff (express :: EXPRESS | e) m => Int -> m Unit
listenHttp f port = do
  app <- liftEff _makeApp
  _ <- liftEff $ _get app $ genCallback f
  liftAff $ fromEffFnAff $ _listenHttp app port

get :: ∀ e m a. MR.MonadReader ExpressApp m => MonadEff (express :: EXPRESS | e) m => m EResponse
get = do
  app <- MR.ask
  response <- liftEff $ _get app (\_ _ -> pure unit)
  pure response

genCallback :: ∀ e r b . ExpressHandler e r -> (ExRequest -> EResponse -> Eff (express :: EXPRESS | e) Unit)
genCallback f =
  \req resp -> launchAff_ $ runReaderT2 (makeRequest req) $ do
    r <- MR.ask f
    x <- liftEff $ genNativeResponse r resp
    pure x 
  where runReaderT2 = flip runReaderT

foreign import _makeApp :: ∀ e . Eff (express :: EXPRESS | e) ExpressApp
foreign import _get :: ∀ e . ExpressApp -> (ExRequest -> EResponse -> Eff (express :: EXPRESS | e) Unit) -> Eff (express :: EXPRESS | e) EResponse
foreign import _listenHttp :: ∀ e m. ExpressApp -> Int -> EffFnAff (express :: EXPRESS | e) Unit