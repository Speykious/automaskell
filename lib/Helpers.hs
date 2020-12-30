module Helpers where

import Data.Set (Set, empty, singleton, fromList)
import qualified Data.Set as DS
import Data.List (foldl', intercalate)

clr = "\x1b[0m"
bld = "\x1b[1m"
dim = "\x1b[2m"
red = "\x1b[31m"
grn = "\x1b[32m"
yel = "\x1b[33m"

showSetg :: (Show a) => String -> Set a -> String
showSetg sep s
  | DS.null s    = "{}"
  | otherwise = "{ " ++ intercalate sep (foldl' (\acc a -> show a : acc) [] s) ++ " }"

showSet :: (Show a) => Set a -> String
showSet = showSetg ", "

showSetLn :: (Show a) => Set a -> String
showSetLn = showSetg "\n, "

printSet :: (Show a) => Set a -> IO ()
printSet = putStrLn . showSet
printSetLn :: (Show a) => Set a -> IO ()
printSetLn = putStrLn . showSetLn

(<>>=>) :: (Foldable m, Foldable n, Monoid (n b)) => m a -> (a -> n b) -> n b
m <>>=> f = foldl' (\acc a -> acc <> f a) mempty m

hasDuplicates :: (Eq a, Ord a) => [a] -> Bool
hasDuplicates xs = DS.size (fromList xs) /= length xs