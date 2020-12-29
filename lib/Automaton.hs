module Automaton where

import DotShow
import Data.Set (Set, fromList, empty)
import Data.List (foldl')

data S a = S a (Bool, Bool)      deriving (Eq, Ord) -- State
data T a = T (S a) Char (S a)    deriving (Eq, Ord) -- Transition
data FSM a b = FSM b (Set (T a)) deriving (Eq, Ord) -- Finite State Machine

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

instance (Show a, Show b) => Show (FSM a b) where
  show (FSM l st) = undefined

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

states :: Ord a => FSM a b -> Set (S a)
states (FSM l st) = foldl' (\acc (T sa _ sb) -> acc <> fromList [sa, sb]) empty st

fsm :: (Ord a) => b -> [T a] -> FSM a b
fsm b ts = FSM b $ fromList ts