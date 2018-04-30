module Paths where

import Prelude
import Data.Array (uncons)
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(..), fst)

class Path a b c | a -> b, b -> c where
  run :: b -> Array String -> a -> Maybe (Tuple c (Array String))

-- | The type of constant paths
data ConstantPath
  = ConstantPath String

-- | The type of paths that are parsed to a particular type
data ParsePath a
  = ParsePath (String -> Maybe a)

-- | The type of paths that are composed from other paths
data CombinedPath r1 r2
  = CombinedPath (Tuple r1 r2)

instance constantPathPath :: Path (ConstantPath) a a where
  run input arr (ConstantPath constStr) = do 
    {head : x, tail : xs} <- uncons arr 
    if x == constStr then Just $ Tuple input xs else Nothing

instance parsePathPath :: Path (ParsePath b) (b -> a) a where
  run f arr (ParsePath parser) = do
    {head : x, tail : xs} <- uncons arr
    result <- parser x
    Just $ Tuple (f result) xs

instance parsePathFunctor :: Functor ParsePath where
  map f (ParsePath p) = ParsePath (map (f) <<< p)

instance combinedPathPoute :: (Path a b c, Path d c e) => Path (CombinedPath a d) b e where
  run input arr (CombinedPath(Tuple r1 r2)) = do
    Tuple a arr' <- run input arr r1
    t2 <- run a arr' r2
    Just $ t2  

-- | Accepts a path that exactly matches the supplied string
constantR :: String -> ConstantPath
constantR name = ConstantPath name

-- | Accepts a path that matches any Int
intR :: ParsePath Int
intR = ParsePath (I.fromString)

-- | Accepts a path that matches any Number
numberR :: ParsePath Number
numberR = ParsePath (N.fromString)

-- | Accepts a path that matches any String
stringR :: ParsePath String
stringR = ParsePath (Just)

-- | Accepts a path that matches any Boolean
booleanR :: ParsePath Boolean
booleanR = ParsePath (\str -> case str of
  "true"  -> Just true
  "false" -> Just false
  _       -> Nothing
  )

--- | Combine two path together, separated by a "/"
combineR :: ∀ a b c d e . Path a b c => Path d c e => a -> d -> CombinedPath a d
combineR a b = CombinedPath (Tuple a b)

infixr 4 combineR as </>

runRoute :: ∀ a b c. Path a b c => b -> String -> a -> Maybe c
runRoute x str v = 
  map fst $ run x (split (Pattern "/") str) v



