module Routing where

import Prelude

import Data.Array (uncons)
import Data.DateTime (DateTime(..))
import Data.Either (hush)
import Data.Formatter.DateTime as FDT
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
  run input str (ConstantRoute constStr) = map (\str' -> Tuple input str') $ stripPrefix (Pattern constStr) str

instance parseRouteRoute :: Route (ParseRoute b) (b -> a) a where
  run f str (ParseRoute parser) = map (\x -> Tuple (f x) "") $ parser str

instance parseRouteFunctor :: Functor ParseRoute where
  map f (ParseRoute p) = ParseRoute(map (f) <<< p)

instance combinedRouteToute :: (Route a b c, Route d c e) => Route (CombinedRoute a d) b e where
  run input str (CombinedRoute(Tuple r1 r2)) = 
    case uncons splitStr of
      Just {head : x, tail : xs} -> do
        Tuple a str' <- run input x r1
        t2 <- run a (joinWith "/" xs) r2
        Just $ t2  
      Nothing ->
        Nothing
    where 
      splitStr = split (Pattern "/") str

-- | Accepts a route that exactly matches the supplied string
constantR :: String -> ConstantRoute
constantR name = ConstantRoute name

-- | Accepts a route that matches any Int
intR :: ParseRoute Int
intR = ParseRoute (I.fromString)

-- | Accepts a route that matches any Number
numberR :: ParseRoute Number
numberR = ParseRoute (N.fromString)

-- | Accepts a route that matches any String
stringR :: ParseRoute String
stringR = ParseRoute (Just)

-- | Accepts a route that matches any Boolean
booleanR :: ParseRoute Boolean
booleanR = ParseRoute (\str -> case str of
  "true"  -> Just true
  "false" -> Just false
  _       -> Nothing
  )

--- | Combine two routes together, separated by a "/"
combineR :: ∀ a b c d e . Route a b c => Route d c e => a -> d -> CombinedRoute a d
combineR a b = CombinedRoute (Tuple a b)

infixr 4 combineR as </>

runRoute :: ∀ a b c. Route a b c => b -> String -> a -> Maybe c
runRoute x str v = 
  map fst $ run x str v



