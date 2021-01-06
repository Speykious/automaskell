module Main (main) where

import State
import Transition
import Automaton
import DotShow
import Helpers
import Data.List (foldl', intercalate)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.Bifunctor
import Data.Foldable (find)
import Set (Set, fromList)
import qualified Set as DS

type A = Integer

s0 = S 0 (False, False)
s1 = S 1 (False, True)
s2 = S 2 (True, False)

s10 = S 10 (True, True)
s11 = S 11 (False, True)
s12 = S 12 (False, False)
s13 = S 13 (False, False)

ta = T s2 'b' s1
tb = T s0 'a' s2
ts = [ T s0 'b' s1
     , T s0 'a' s1
     , T s1 'a' s0
     , T s1 'b' s2
     , T s2 'a' s0 ]

tt = [ T s10 'z' s11
     , T s10 'z' s12
     , T s11 'x' s12
     , T s11 'z' s13
     , T s12 'y' s13 ]


auto :: FSM A
auto = fsmFromList "auto" (ta:ts)
auto2 :: FSM A
auto2 = fsmFromList "auto2" (tb:ts)
deter :: FSM (Set A)
deter = convertToDFA auto2
autodeter :: FSM (A, Set A)
autodeter = auto <&> deter

e1 = S 1 (True, False)
e2 = S 2 (False, False)
e3 = S 3 (False, True)

example1 :: FSM A
example1 = fsmFromList "example1" [ T e1 'b' e1
                                  , T e1 'a' e2
                                  , T e2 'a' e2
                                  , T e2 'b' e3
                                  , T e3 'a' e3
                                  , T e3 'b' e3 ]
example2 :: FSM A
example2 = fsmFromList "example2" [ T e1 'b' e2
                                  , T e2 'b' e2
                                  , T e2 'a' e3
                                  , T e3 'b' e2
                                  , T e3 'a' e3 ]

examplete :: FSM A
examplete = complete (-1) "ab" example2



example3 :: FSM A
example3 = fsmFromList "example3" tt

exatenation :: FSM A
exatenation = example1 <> example3



main :: IO ()
main = do dotPDF (example1 <&> examplete)
          dotPDF (example1 <|> examplete)
          dotPDF (example1 <^> examplete)
          dotPDF (example1 <=> examplete)
