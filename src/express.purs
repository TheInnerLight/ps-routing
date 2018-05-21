module Express where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader as MR
import Express.Effect (EXPRESS)
import Express.Http (HttpError)
import Express.Request (ExRequest, Request(..), makeRequest)
import Express.Response (EResponse, Response, genNativeResponse)

foreign import data ExpressApp :: Type

-- | An express handler is a type capable of reading an http request, performing asynchonrous effects and which ultimately returns an http response
type ExpressHandler e r = ExceptT HttpError (ReaderT Request (Aff (express :: EXPRESS | e))) (Response r)

listenHttp :: forall e r m b. ExpressHandler e r -> MonadAff (express :: EXPRESS | e) m => Int -> m Unit
listenHttp f port = do
  app <- liftEff _makeApp
  _ <- liftEff $ _get app $ genCallback f
  liftAff $ fromEffFnAff $ _listenHttp app port

get :: forall e m a. MR.MonadReader ExpressApp m => MonadEff (express :: EXPRESS | e) m => m EResponse
get = do
  app <- MR.ask
  response <- liftEff $ _get app (\_ _ -> pure unit)
  pure response

genCallback :: forall e r b . ExpressHandler e r -> (ExRequest -> EResponse -> Eff (express :: EXPRESS | e) Unit)
genCallback f =
  \req resp -> launchAff_ $ runReaderT2 (makeRequest req) $ runExceptT $ do
    r <- MR.ask f
    x <- liftEff $ genNativeResponse r resp
    pure x 
  where runReaderT2 = flip runReaderT

foreign import _makeApp :: forall e . Eff (express :: EXPRESS | e) ExpressApp
foreign import _get :: forall e . ExpressApp -> (ExRequest -> EResponse -> Eff (express :: EXPRESS | e) Unit) -> Eff (express :: EXPRESS | e) EResponse
foreign import _listenHttp :: forall e m. ExpressApp -> Int -> EffFnAff (express :: EXPRESS | e) Unit