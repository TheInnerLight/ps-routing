module Routing where

import Prelude

import Data.Array (uncons)
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String (Pattern(..), joinWith, split, stripPrefix)
import Data.Tuple (Tuple(..), fst)

class Route a b c | a -> b, b -> c where
  run :: b -> String -> a -> Maybe (Tuple c String)

data ConstantRoute
  = ConstantRoute String

data ParseRoute a
  = ParseRoute (String -> Maybe a)

data CombinedRoute r1 r2
  = CombinedRoute (Tuple r1 r2)

instance constantRouteRoute :: Route (ConstantRoute) a a where
  run input str (ConstantRoute constStr) = map (\str' -> Tuple input str') $ stripPrefix (Pattern $ constStr <> "/") str

instance parseRouteRoute :: Route (ParseRoute b) (b -> a) a where
  run f str (ParseRoute parser) = 
    case uncons splitStr of
      Just {head : x, tail : xs} -> map (\x -> Tuple (f x) (joinWith "/" xs)) $ parser x
      Nothing -> Nothing
    where
    splitStr = split (Pattern "/") str

instance combinedRouteToute :: (Route a b c, Route d c e) => Route (CombinedRoute a d) b e where
  run input str (CombinedRoute(Tuple r1 r2)) = do
    Tuple a str' <- runRoute1
    t2 <- run a str' r2
    Just $ t2
    where 
    runRoute1 :: Maybe (Tuple c String)
    runRoute1 = run input str r1

constantR :: String -> ConstantRoute
constantR name = ConstantRoute name

intR :: ParseRoute Int
intR = ParseRoute (I.fromString)

numberR :: ParseRoute Number
numberR = ParseRoute (N.fromString)

stringR :: ParseRoute String
stringR = ParseRoute (Just)

combineR :: forall a b c d e . Route a b c => Route d c e => a -> d -> CombinedRoute a d
combineR a b = CombinedRoute (Tuple a b)

infix 4 combineR as </>

runRoute :: forall a b c. Route a b c => b -> String -> a -> Maybe c
runRoute x str v = 
  map fst $ run x str v



