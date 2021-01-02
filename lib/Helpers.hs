module Helpers where

import Set (Set, empty, singleton, fromList)
import qualified Set as DS
import Data.List (foldl', intercalate)
import Data.Bifunctor (Bifunctor, bimap)

clr = "\x1b[0m"
bld = "\x1b[1m"
dim = "\x1b[2m"
red = "\x1b[31m"
grn = "\x1b[32m"
yel = "\x1b[33m"

(<<$>>) :: Ord b => (a -> b) -> Set a -> Set b
(<<$>>) = DS.map

(<>>=>) :: (Foldable m, Foldable n, Monoid (n b)) => m a -> (a -> n b) -> n b
m <>>=> f = foldl' (\acc a -> acc <> f a) mempty m

hasDuplicates :: (Eq a, Ord a) => [a] -> Bool
hasDuplicates xs = DS.size (fromList xs) /= length xs

alphaComp :: (Foldable t, Eq a) => t a -> [a] -> [a]
alphaComp s = filter $ not . (`elem` s)

zipMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
zipMaybe (Just a, Just b) = Just (a, b)
zipMaybe _                = Nothing