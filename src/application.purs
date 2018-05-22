module Express.Application where
  
import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Error.Class (catchError)
import Data.List (List, filterM, head)
import Data.Maybe (Maybe, fromMaybe)
import Express (listenHttp)
import Express.Effect (EXPRESS)
import Express.Endpoint (Endpoint, createHandler)
import Express.Http (HttpError(..), NOT_FOUND, kind Status)
import Express.Response (notFound, text)

newtype ApplicationInternal s eff = ApplicationInternal (List (Endpoint eff s))

findM :: forall m t. Monad m => (t -> m Boolean) -> List t -> m (Maybe t)
findM p xs = map head $ filterM p xs

run :: forall eff s m. MonadAff (express :: EXPRESS | eff) m => ApplicationInternal (notFound :: NOT_FOUND | s) eff -> Int -> m Unit
run (ApplicationInternal items) port =
  listenHttp chooseEndpoint port
  where
    chooseEndpoint = do 
      ep <- findM f items
      fromMaybe (pure $ notFound $ text "Not Found!") (map (createHandler) ep)
    f endpoint = catchError (map (\_ -> true) $ createHandler endpoint) (pure <<< errorhandler)
    errorhandler UnknownPath = false

        
