module Express.Response where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Express.Effect (EXPRESS)

data Header
  = Other String String

data Content
  = Text String

newtype Response = Response {code :: Int, headers :: Array Header, body :: Content}

foreign import data EResponse :: Type

genHeader :: ∀ e. Header -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genHeader (Other key value) = 
  \resp -> _setHeader key value resp

genNativeResponse :: ∀ e . Response-> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genNativeResponse (Response {code : code, headers : headers, body : body}) = 
  \resp -> 
    traverse_ (\h -> genHeader h resp) headers 
    *> _setStatus code resp
    *> sendBody body resp 

sendBody :: ∀ e . Content -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
sendBody (Text text) = _sendText text

text :: ∀ t . String -> Content
text str = Text str

ok :: ∀ b. Content -> Response
ok content = Response {code: 200, headers : [], body : content}

badRequest :: Content -> Response
badRequest content = Response {code: 400, headers : [], body : content}

notFound ::  Content -> Response
notFound content = Response {code: 404, headers : [], body : content}

foreign import _setHeader :: ∀ e. String -> String -> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _setStatus :: ∀ e. Int-> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _sendText :: ∀ e. String -> EResponse -> Eff (express :: EXPRESS | e) Unit
  
