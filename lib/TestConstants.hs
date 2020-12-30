module TestConstants where

import Prelude hiding (null)
import Automaton
import DotShow
import Helpers
import Data.List (foldl', intercalate)
import Data.Set (fromList)

s0 = S 0 (False, False)
s1 = S 1 (False, True)
s2 = S 2 (True, False)
s3 = S 3 (True, True)

t0 = T s0 'a' s1
td = T s0 'a' s2
t1 = T s0 'b' s1
t2 = T s1 'a' s0
t3 = T s1 'b' s2
t4 = T s2 'a' s0

ts = [t0, td, t1, t2, t3, t4]
automaton = fsmFromList "A" ts


somePrints :: IO ()
somePrints = do
  putStr $ bld ++ "States:" ++ clr ++ " "
  print [s0, s1, s2, s3]
  putStrLn $ bld ++ "Transitions:" ++ clr
  putStrLn $ unlines $ ("  " ++) . show <$> ts
  putStrLn ""
  printSetLn $ fromList (t0:ts)
  putStrLn ""
  printSet $ states automaton
