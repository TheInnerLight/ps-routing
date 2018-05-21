module Express.Response where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Express.Effect (EXPRESS)
import Express.Http (BAD_REQUEST, NOT_FOUND, OK, kind Status)

data Header
  = Other String String

data Content
  = Text String

newtype Response (statuses :: # Status) = Response {code :: Int, headers :: Array Header, body :: Content}

foreign import data EResponse :: Type

genHeader :: forall e. Header -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genHeader (Other key value) = 
  \resp -> _setHeader key value resp

genNativeResponse :: forall e r . Response r -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genNativeResponse (Response {code : code, headers : headers, body : body}) = 
  \resp -> 
    traverse_ (\h -> genHeader h resp) headers 
    *> _setStatus code resp
    *> sendBody body resp 

sendBody :: forall e . Content -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
sendBody (Text text) = _sendText text

text :: forall t . String -> Content
text str = Text str

ok :: forall r. Content -> Response (ok :: OK | r)
ok content = Response {code: 200, headers : [], body : content}

badRequest :: forall r. Content -> Response (badRequest :: BAD_REQUEST | r)
badRequest content = Response {code: 400, headers : [], body : content}

notFound :: forall r. Content -> Response (notFound :: NOT_FOUND | r)
notFound content = Response {code: 404, headers : [], body : content}

foreign import _setHeader :: forall e. String -> String -> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _setStatus :: forall e. Int-> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _sendText :: forall e. String -> EResponse -> Eff (express :: EXPRESS | e) Unit
  
