module State where

import DotShow
import Helpers (clr, dim, red, grn)
import Set (Set)
import qualified Set as DS
import Data.Foldable (find)

-- | State of an automaton.
data S a = S { stateLabel :: a            -- ^ Label of the state, which can be of any type.
                                          -- Useful for various operations on automata.
             , stateFlags :: (Bool, Bool) -- ^ Flags of the state (initial, final).
             } deriving (Eq, Ord)

instance Show a => Show (S a) where
  show (S a (i, f)) = dim ++ "S" ++ clr ++ show a
                    ++ (if i then grn ++ "#I" ++ clr else "")
                    ++ (if f then red ++ "#F" ++ clr else "")

instance Show a => DotShow (S a) where
  dotShow    (S a _)      = show ("S" ++ show a)
  dotDeclare (S a (i, f)) = Just $ show ("S" ++ show a)
                          ++ case params of
                            [a, b] -> " [" ++ a ++ "," ++ b ++ "];"
                            [a]    -> " [" ++ a ++ "];"
                            _      -> ";"
    where params = filter (/= "") [ if i then "color=red" else ""
                                  , if f then "peripheries=2" else "" ]

-- | Get the initial states from the set of states.
initialStates :: Set (S a) -> Set (S a)
initialStates = DS.filter $ fst . stateFlags

-- | Get the unique initial state from the set of states. Returns `Nothing` if there is no initial state.
initialState :: Set (S a) -> Maybe (S a)
initialState = find $ fst . stateFlags

-- | Checks if a set of states contains initial states.
isInitialSet :: Set (S a) -> Bool
isInitialSet = not . DS.null . initialStates

-- | Get the final states from the set of states.
finalStates :: Set (S a) -> Set (S a)
finalStates = DS.filter $ snd . stateFlags

-- | Checks if a set of states contains final states.
isFinalSet :: Set (S a) -> Bool
isFinalSet = not . DS.null . finalStates

-- | Merges a set of states into a state of sets. This is where an arbitrary typed label for the set is useful.
mergeStates :: Ord a => Bool -> Set (S a) -> S (Set a)
mergeStates i ss = S (DS.map stateLabel ss) (i, isFinalSet ss)

-- | Boolean relation: two booleans being related or not.
type BoolRelation = Bool -> Bool -> Bool
-- | Pairs a pair of states into a state of pair.
pairStates :: Ord a => Bool -> BoolRelation -> (S a, S b) -> S (a, b)
pairStates i op (S a (_, fa), S b (_, fb)) = S (a, b) (i, op fa fb)

-- | Checks equality of state by label, disregarding the flags.
(|==|) :: Eq a => S a -> S a -> Bool
(S a _) |==| (S a' _) = a == a'

-- | Checks if a state is member of a set of states by label, disregarding the flags.
labelMember :: Eq a => S a -> Set (S a) -> Bool
labelMember sa = any (|==| sa)