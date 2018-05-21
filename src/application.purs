module Express.Application where
  
import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.List (List(..), foldr, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Express (listenHttp)
import Express.Effect (EXPRESS)
import Express.Endpoint (Endpoint, createHandler)
import Express.Http (HttpError(..), NOT_FOUND, kind Status)
import Express.Response (notFound, text)
import Unsafe.Coerce (unsafeCoerce)

newtype ApplicationInternal s eff = ApplicationInternal (List (Endpoint eff s))

findM :: forall m t. Monad m => (t -> m Boolean) -> List t -> m (Maybe t)
findM p xs =
  skipWhileMRec p xs
  where 
    skipWhileMRec p Nil = pure Nothing
    skipWhileMRec p (Cons x xs') = do
      q <- p x
      case q of
        true  -> pure $ Just x
        false -> skipWhileMRec p xs'
    

run :: forall eff s m. MonadAff (express :: EXPRESS | eff) m => ApplicationInternal (notFound :: NOT_FOUND | s) eff -> Int -> m Unit
run (ApplicationInternal items) port =
  listenHttp handleShit port
  where
    handleShit = do 
      ep <- findM f items
      fromMaybe (pure $ notFound $ text "Not Found!") (map (createHandler) ep)
    f endpoint = catchError (map (\_ -> true) $ createHandler endpoint) (pure <<< errorhandler)
    errorhandler UnknownPath = false

        
