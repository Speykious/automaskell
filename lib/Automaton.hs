module Automaton where

import DotShow
import Helpers
import Data.Set (Set, fromList, empty)
import Data.List (foldl', lines, intercalate)

data S a = S a (Bool, Bool)         deriving (Eq, Ord) -- State
data T a = T (S a) Char (S a)       deriving (Eq, Ord) -- Transition
data FSM a = FSM String (Set (T a)) deriving (Eq, Ord) -- Finite State Machine

clr = "\x1b[0m"
bld = "\x1b[1m"
dim = "\x1b[2m"
red = "\x1b[31m"
grn = "\x1b[32m"
yel = "\x1b[33m"

instance Show a => Show (S a) where
  show (S a (i, f)) = dim ++ "S" ++ clr ++ show a
                    ++ (if i then grn ++ "#I" ++ clr else "")
                    ++ (if f then red ++ "#F" ++ clr else "")

instance Show a => Show (T a) where
  show (T sa c sb) = show sa ++ " "
                   ++ yel ++ ">-" ++ [c] ++ "->" ++ clr
                   ++ " " ++ show sb

instance (Ord a, Show a) => Show (FSM a) where
  show (FSM l st) = bld ++ "Automaton " ++ show l ++ clr ++ ":\n"
                  ++ bld ++ "  States" ++ clr ++ ":\n    "
                  ++ indentedSet (statesFromTransitions st) ++ "\n"
                  ++ bld ++ "  Transitions" ++ clr ++ ":\n    "
                  ++ indentedSet st
    where indentedSet :: (Show a) => Set a -> String
          indentedSet = intercalate "\n    " . lines . showSetLn


instance Show a => DotShow (S a) where
  dotShow    (S a _)      = "S" ++ show a
  dotDeclare (S a (i, f)) = Just $ "S" ++ show a
                          ++ case params of
                            [a, b] -> " [" ++ a ++ "," ++ b ++ "];"
                            [a]    -> " [" ++ a ++ "];"
                            _      -> ";"
    where params = filter (/= "") [ if i then "color=red" else ""
                                  , if f then "peripheries=2" else "" ]

instance Show a => DotShow (T a) where
  dotShow (T sa c sb) = dotShow sa ++ " -> " ++ dotShow sb
                      ++ " [label=" ++ [c] ++ "];"

statesFromTransitions :: Ord a => Set (T a) -> Set (S a)
statesFromTransitions = mapSet $ \(T sa _ sb) -> fromList [sa, sb]
                        --foldl' (\acc (T sa _ sb) -> acc <> fromList [sa, sb]) empty

states :: Ord a => FSM a -> Set (S a)
states (FSM l st) = statesFromTransitions st

fsm :: (Ord a) => String -> [T a] -> FSM a
fsm l ts = FSM l $ fromList ts

transitionsFromState :: FSM a -> S a -> Set (T a)
transitionsFromState = undefined

succStates :: FSM a -> S a -> Char -> Set (S a)
succStates (FSM l st) s c = undefined