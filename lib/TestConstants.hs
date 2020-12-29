module TestConstants where

import Prelude hiding (null)
import Automaton
import DotShow
import Data.List (foldl', intercalate)
import Data.Set (Set, null, fromList)

s0 = S 0 (False, False)
s1 = S 1 (False, True)
s2 = S 2 (True, False)
s3 = S 3 (True, True)

t0 = T s0 'a' s1
t1 = T s0 'b' s1
t2 = T s1 'a' s0
t3 = T s1 'b' s2
t4 = T s2 'a' s0

ts = [t0, t1, t2, t3, t4]
automaton = fsm "A" ts

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

main :: IO ()
main = do
  putStr $ bld ++ "States:" ++ clr ++ " "
  print [s0, s1, s2, s3]
  putStrLn $ bld ++ "Transitions:" ++ clr
  putStrLn $ unlines $ ("  " ++) . show <$> ts
  putStrLn ""
  printSetLn $ fromList (t0:ts)
  putStrLn ""
  printSet $ states automaton
