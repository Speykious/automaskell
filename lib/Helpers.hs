module Helpers where

import Set (Set, empty, singleton, fromList)
import qualified Set as DS
import Data.List (foldl', intercalate)
import Data.Bifunctor (Bifunctor, bimap)

-- | ANSI color code to reset the styles.
clr :: String
clr = "\x1b[0m"
-- | ANSI color code to make the text bold.
bld :: String
bld = "\x1b[1m"
-- | ANSI color code to make the text dim.
dim :: String
dim = "\x1b[2m"
-- | ANSI color code to make the text red.
red :: String
red = "\x1b[31m"
-- | ANSI color code to make the text green.
grn :: String
grn = "\x1b[32m"
-- | ANSI color code to make the text yellow.
yel :: String
yel = "\x1b[33m"

-- | Map operator for sets, which can't implement the Functor typeclass due to type constraints.
(<<$>>) :: Ord b => (a -> b) -> Set a -> Set b
(<<$>>) = DS.map

-- | Flatmap operator for sets, which can't implement the Monad typeclass due to type constraints.
(<>>=>) :: (Foldable m, Foldable n, Monoid (n b)) => m a -> (a -> n b) -> n b
m <>>=> f = foldl' (\acc a -> acc <> f a) mempty m
 
-- | Whether a list has duplicates or not. Constructs a set and compares its size to the original list's size.
hasDuplicates :: (Eq a, Ord a) => [a] -> Bool
hasDuplicates xs = DS.size (fromList xs) /= length xs

-- | Get the complementary partition of an alphabet.
--
-- Example:
--     alphaComp "abcd" "bd" == "ac"
alphaComp :: (Foldable t, Eq a) => t a -- ^ The alphabet.
                                -> [a] -- ^ The other partition.
                                -> [a]
alphaComp s = filter $ not . (`elem` s)

-- | Zips a pair of maybes into a Maybe of a pair. It will be Just (a, b) when a and b are something, and Nothing otherwise.
zipMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
zipMaybe (Just a, Just b) = Just (a, b)
zipMaybe _                = Nothing