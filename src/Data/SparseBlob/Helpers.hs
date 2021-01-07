
-- | Internal module. General helper functions.
module Data.SparseBlob.Helpers
  ( (.:)
  , mapSnd
  , maybeBinR
  , eqLen
  , groupSliding
  , mergeAscOn
  , intercalateShow
  ) where

-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)), (<|) )


infixr 9 .:

-- | Composes two functions, where the seconds one is fed /two/ arguments.
-- Intuitively, the number of dots after the first signifies the argument count.
(.:) :: ( c -> d ) -> ( a -> b -> c ) -> a -> b -> d
(.:) g f a = g . f a

-- | Applies the function to the second tuple element.
mapSnd :: ( b -> c ) -> (a, b) -> (a, c)
mapSnd f (a,b) = (a, f b)

-- | Binary operator where the first argument is optional. If the first argument
-- is `Nothing`, then the second argument is returned.
--
-- Examples:
-- >>> maybeBinR (+) (Just 2) 3
-- 5
--
-- >>> maybeBinR (+) Nothing 3
-- 3
maybeBinR :: ( a -> b -> b ) -> Maybe a -> b -> b
maybeBinR f Nothing  b = b
maybeBinR f (Just a) b = f a b

-- | Returns `True` if the lengths are equal. Terminates if at least one of the
-- inputs has finite length.
eqLen :: [a] -> [b] -> Bool
eqLen []     []     = True
eqLen (_:xs) (_:ys) = eqLen xs ys
eqLen _      _      = False

-- | Groups adjacent elements that satisfy the predicate.
--
-- Example:
-- >>> map NE.toList $ groupSliding (\a b -> b - a <= 2) [1,2,4,7,11,12,13,16]
-- [[1,2,4],[7],[11,12,13],[16]]
groupSliding :: ( a -> a -> Bool ) -> [a] -> [NonEmpty a]
groupSliding f []     = []
groupSliding f (x:xs) = NE.toList $ groupSliding' f (x :| xs)
  where
  groupSliding' :: ( a -> a -> Bool ) -> NonEmpty a -> NonEmpty (NonEmpty a)
  groupSliding' _ (x :| []) = (x :| []) :| []
  groupSliding' f (x :| (y:ys))
    | f x y      =
        let (zs :| zss) = groupSliding' f (y :| ys)
        in (x <| zs) :| zss
    | otherwise  =
        (x :| []) <| groupSliding' f (y :| ys)

-- | Merges lists which are ascending on the property (defined by the argument
-- function) into another ascending list.
mergeAscOn :: Ord b => ( a -> b ) -> [a] -> [a] -> [a]
mergeAscOn f (x:xs) (y:ys)
  | f x <= f y  = x : mergeAscOn f xs (y:ys)
  | otherwise   = y : mergeAscOn f (x:xs) ys
mergeAscOn f xs [] = xs
mergeAscOn f [] ys = ys

-- | Intercalates the `ShowS` elements with a separator `ShowS`. Like
-- `Data.List.intercalate`, but for `ShowS` elements.
--
-- Example:
-- >>> intercalateShow (showString ",") (map shows [1,2,3,4]) ""
-- "1,2,3,4"
intercalateShow :: ShowS -> [ShowS] -> ShowS
intercalateShow _ []     = id
-- Specifically use `foldl`, as `foldr` quickly causes a stack overflow
intercalateShow v (x:xs) = foldl (\b a -> b.v.a) x xs
