module Transition where

import DotShow
import Helpers (clr, yel, (<>>=>))
import State
import Set (Set, fromList, elems)
import qualified Set as DS

-- | Transition of an automaton.
data T a = T { transStart :: S a -- ^ The starting state of the transition.
             , symbol :: Char    -- ^ The symbol of the transition.
             , transEnd :: S a   -- ^ The ending state of the transition.
             } deriving (Eq, Ord)

instance Show a => Show (T a) where
  show (T sa c sb) = show sa ++ " "
                   ++ yel ++ ">-" ++ [c] ++ "->" ++ clr
                   ++ " " ++ show sb

instance Show a => DotShow (T a) where
  dotShow (T sa c sb) = dotShow sa ++ " -> " ++ dotShow sb
                      ++ " [label=" ++ [c] ++ "];"

-- | Get all states from transitions.
statesFromTrans :: Ord a => Set (T a) -> Set (S a)
statesFromTrans = (<>>=> \(T sa _ sb) -> fromList [sa, sb])

-- | Get all transitions which have this starting state.
transFromState :: Eq a => S a -> Set (T a) -> Set (T a)
transFromState s = DS.filter $ (== s) . transStart

-- | Get all transitions with this label.
transWithLabel :: Char -> Set (T a) -> Set (T a)
transWithLabel c = DS.filter $ (== c) . symbol

startingStates :: Ord a => Set (T a) -> Set (S a)
startingStates = DS.map transStart

-- | Get all ending states of transitions.
endingStates :: Ord a => Set (T a) -> Set (S a)
endingStates = DS.map transEnd

-- | Create an ending state from transitions.
createEndingState :: Ord a => Set (T a) -> S (Set a)
createEndingState = mergeStates False . endingStates

-- | Pair ending states from a pair of transitions.
pairEndingStates :: Ord a => BoolRelation -> (T a, T b) -> S (a, b)
pairEndingStates op (ta, tb) = pairStates False op (transEnd ta, transEnd tb)

-- | Get the alphabet from transitions.
alphaFromTrans :: Set (T a) -> String
alphaFromTrans = elems . DS.map symbol

-- | Get the alphabet from a pair of transitions.
alphaFromTransPair :: (T a, T b) -> String
alphaFromTransPair (ta, tb) = elems $ fromList [symbol ta, symbol tb]

-- | Get the supposedly unique symbol from a set of transitions. Errors out if no such symbol exists.
aFromTrans :: Set (T a) -> Char
aFromTrans = head . alphaFromTrans

-- | Get the supposedly unique symbol from a pair of transitions.
aFromTransPair :: (T a, T b) -> Char
aFromTransPair = head . alphaFromTransPair

-- | Groups a set of transitions by their symbol.
groupBySymbol :: Ord a => Set (T a) -> Set (Set (T a))
groupBySymbol st = fromList $ (`transWithLabel` st) <$> alphaFromTrans st