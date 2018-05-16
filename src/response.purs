module Express.Response where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Express.Effect (EXPRESS)

data Header
  = Other String String

data Content a
  = Text String

newtype Response a = Response {headers :: Array Header, body :: Content a}

foreign import data EResponse :: Type

genHeader :: ∀ e. Header -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genHeader (Other key value) = 
  \resp -> _setHeader key value resp

genNativeResponse :: ∀ e b. Response b -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
genNativeResponse (Response {headers : headers, body : body}) = 
  \resp -> traverse_ (\h -> genHeader h resp) headers *> sendBody body resp

sendBody :: ∀ e b. Content b -> (EResponse -> Eff (express :: EXPRESS | e) Unit)
sendBody (Text text) = _sendText text

foreign import _setHeader :: ∀ e m. String -> String -> EResponse -> Eff (express :: EXPRESS | e) Unit
foreign import _sendText :: ∀ e m. String -> EResponse -> Eff (express :: EXPRESS | e) Unit
  
