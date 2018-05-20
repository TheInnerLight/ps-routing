module Express.Status where

foreign import kind Status
foreign import data OK :: Status
foreign import data NOT_FOUND :: Status
foreign import data BAD_REQUEST :: Status