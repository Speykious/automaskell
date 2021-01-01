module TestConstants where

import Prelude hiding (null)
import State
import Transition
import Automaton
import DotShow
import Helpers
import Data.List (foldl', intercalate)
import Set (fromList)
import qualified Set as DS

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

auto1 = fsmFromList "auto1" [t0, t1, t2, t3, t4]
auto1C = complete (-1) "ab" auto1
auto2 = fsmFromList "auto2" [t0, td, t1, t2, t3, t4]
deter = convertToDFA auto2