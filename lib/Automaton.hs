module Automaton where

import DotShow
import Helpers
import State
import Transition
import Data.Set (Set, fromList, empty, elems)
import qualified Data.Set as DS
import Data.List (foldl', lines, intercalate)

data FSM a = FSM { label :: String
                 , transitions :: Set (T a) } deriving (Eq, Ord) -- Finite State Machine



instance (Ord a, Show a) => Show (FSM a) where
  show (FSM l st) = bld ++ "Automaton " ++ l ++ clr ++ ":\n"
                  ++ bld ++ "  States" ++ clr ++ ":\n    "
                  ++ indentedSet (statesFromTrans st) ++ "\n"
                  ++ bld ++ "  Transitions" ++ clr ++ ":\n    "
                  ++ indentedSet st
    where indentedSet :: (Show a) => Set a -> String
          indentedSet = intercalate "\n    " . lines . showSetLn

instance (Ord a, Show a) => DotShow (FSM a) where
  dotShow (FSM l st) = "digraph " ++ l ++ " {\n"
                     ++ "  rankdir=LR;\n"
                     ++ unlines (("  " ++) . dotShow <$> elems st)
                     ++ unlines (("  " ++) . unwrap . dotDeclare <$> elems (statesFromTrans st))
                     ++ "}"
    where unwrap (Just a) = a
          unwrap Nothing = error "Trying to unwrap Nothing"



fsmTransFromState :: Eq a => S a -> FSM a -> Set (T a)
fsmTransFromState s = transFromState s . transitions

fsmFromList :: Ord a => String -> [T a] -> FSM a
fsmFromList l = FSM l . fromList

fsmStates :: Ord a => FSM a -> Set (S a)
fsmStates = statesFromTrans . transitions

fsmInitialStates :: Ord a => FSM a -> Set (S a)
fsmInitialStates = initialStates . fsmStates

fsmFinalStates :: Ord a => FSM a -> Set (S a)
fsmFinalStates = finalStates . fsmStates

succState :: (Eq a, Ord a) => FSM a -> Char -> S a -> Set (S a)
succState (FSM _ st) c s = endingStates $ transWithLabel c $ transFromState s st

allSuccState :: Ord a => FSM a -> String -> S a -> [Set (S a)]
allSuccState m alpha s = (\c -> succState m c s) <$> alpha

succStates :: (Eq a, Ord a) => FSM a -> Char -> Set (S a) -> Set (S a)
succStates m c = (<>>=> succState m c)

fsmExec :: Ord a => FSM a -> String -> Bool
fsmExec m = isFinalSet . exec mis
  where mis = fsmInitialStates m
        exec ss _ | DS.null ss = ss
        exec ss []     = ss
        exec ss (c:cs) = exec (succStates m c ss) cs



isComplete :: (Eq a, Ord a) => String -> FSM a -> Bool
isComplete alpha m = and $ DS.map (notElem empty . allSuccState m alpha) (fsmStates m)

isDeterministic :: (Eq a, Ord a) => FSM a -> Bool
isDeterministic m = and $ DS.map ((<= 1) . DS.size . (`fsmTransFromState` m)) (fsmStates m)

