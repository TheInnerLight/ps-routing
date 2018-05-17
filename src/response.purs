module Express.Response where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Express.Effect (EXPRESS)

data Header
  = Other String String

data Content a
  = Text String

newtype Response a = Response {code :: Int, headers :: Array Header, body :: Content a}

foreign import data EResponse :: Type

genHeader :: ∀ e. Header -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genHeader (Other key value) = 
  \resp -> _setHeader key value resp

genNativeResponse :: ∀ e b. Response b -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genNativeResponse (Response {code : code, headers : headers, body : body}) = 
  \resp -> 
    traverse_ (\h -> genHeader h resp) headers 
    *> _setStatus code resp
    *> sendBody body resp 

sendBody :: ∀ e b. Content b -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
sendBody (Text text) = _sendText text

text :: ∀ t. String -> Content t
text str = Text str

ok :: ∀ b. Content b -> Response b
ok content = Response {code: 200, headers : [], body : content}

badRequest :: ∀ b. Content b -> Response b
badRequest content = Response {code: 400, headers : [], body : content}

notFound :: ∀ b. Content b -> Response b
notFound content = Response {code: 404, headers : [], body : content}

foreign import _setHeader :: ∀ e. String -> String -> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _setStatus :: ∀ e. Int-> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _sendText :: ∀ e. String -> EResponse -> Eff (express :: EXPRESS | e) Unit
  
