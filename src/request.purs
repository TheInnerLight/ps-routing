module Express.Request where

import Prelude

foreign import data ExRequest :: Type

newtype Request = Request {path :: String}

pathOf :: Request -> String
pathOf (Request req) = req.path

makeRequest :: ∀ e. ExRequest -> Request
makeRequest req = Request {path : _getPath req}

foreign import _getPath :: ∀ e. ExRequest -> String