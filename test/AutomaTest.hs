module Main (main) where

import Automaton
import Data.List

s0 = S 0 (False, False)
s1 = S 1 (False, True)
s2 = S 2 (True, False)
s3 = S 3 (True, True)

t0 = T s0 'a' s1
t1 = T s0 'b' s1
t2 = T s1 'a' s0
t3 = T s1 'b' s2
t4 = T s2 'a' s0

main :: IO ()
main = do
  putStr $ bld ++ "States:" ++ clr ++ " "
  print [s0, s1, s2, s3]
  putStrLn $ bld ++ "Transitions:" ++ clr
  putStrLn $ unlines $ ("  " ++) . show <$> [t0, t1, t2, t3, t4]
