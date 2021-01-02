module TestConstants where

import Prelude hiding (null)
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
t5 = T s2 'b' s1

type A = Integer

auto1 :: FSM A
auto1 = fsmFromList "auto1" [t0, t1, t2, t3, t4, t5]
auto1C :: FSM A
auto1C = complete (-1) "ab" auto1
auto2 :: FSM A
auto2 = fsmFromList "auto2" [t0, td, t1, t2, t3, t4]
deter :: FSM (Set A)
deter = convertToDFA auto2

autodeter :: FSM (A, Set A)
autodeter = auto1 <&> deter


nis :: S (A, Set A)
nis = pairStates True (&&) (f auto1, f deter)
  where f :: Ord a => FSM a -> S a
        f = fromJust . fsmInitialState

stack :: Set (S (A, Set A))
stack = DS.singleton nis

sta :: Set (T A)
stb :: Set (T (Set A))
(sta, stb) = fsmTransFromPairState nis (auto1, deter)

alpha :: String
alpha = DS.elems $ DS.fromList (alphaFromTrans sta ++ alphaFromTrans stb)

stp :: Set (T A, T (Set A))
stp = fromList $ catMaybes $ (\c -> zipMaybe (cfind c sta, cfind c stb)) <$> alpha
  where cfind c = find $ (== c) . symbol

snt :: Set (T (A, Set A))
snt = (\tp -> T nis (aFromTransPair tp) (pairEndingStates (&&) tp)) <<$>> stp

sans :: Set (S (A, Set A))
sans = DS.filter (not . (`labelMember` stack)) (endingStates snt)