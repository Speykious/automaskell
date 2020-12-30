module Helpers where

import Prelude hiding (null)
import Data.Set (Set, null)
import Data.List (foldl', intercalate)

showSetg :: (Show a) => String -> Set a -> String
showSetg sep s
  | null s    = "{}"
  | otherwise = "{ " ++ intercalate sep (foldl' (\acc a -> show a : acc) [] s) ++ " }"

showSet :: (Show a) => Set a -> String
showSet = showSetg ", "

showSetLn :: (Show a) => Set a -> String
showSetLn = showSetg "\n, "

printSet :: (Show a) => Set a -> IO ()
printSet = putStrLn . showSet
printSetLn :: (Show a) => Set a -> IO ()
printSetLn = putStrLn . showSetLn