module Automaton where

import DotShow
import Helpers
import Data.Set (Set, fromList, empty, elems)
import qualified Data.Set as DS
import Data.List (foldl', lines, intercalate)

data S a   = S a (Bool, Bool)                 deriving (Eq, Ord) -- State
data T a   = T { transStart :: S a
               , symbol :: Char
               , transEnd :: S a }            deriving (Eq, Ord) -- Transition
data FSM a = FSM { label :: String
                 , transitions :: Set (T a) } deriving (Eq, Ord) -- Finite State Machine

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
                  ++ indentedSet (statesFromTrans st) ++ "\n"
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

instance (Ord a, Show a) => DotShow (FSM a) where
  dotShow (FSM l st) = "digraph " ++ l ++ " {\n"
                     ++ "  graph [rotate=90];\n  rankdir=LR;\n"
                     ++ unlines (("  " ++) . dotShow <$> elems st)
                     ++ unlines (("  " ++) . unwrap . dotDeclare <$> elems (statesFromTrans st))
                     ++ "}"
    where unwrap (Just a) = a
          unwrap Nothing = error "Trying to unwrap Nothing"



statesFromTrans :: Ord a => Set (T a) -> Set (S a)
statesFromTrans = (<>>=> \(T sa _ sb) -> fromList [sa, sb])

states :: Ord a => FSM a -> Set (S a)
states = statesFromTrans . transitions

fsmFromList :: Ord a => String -> [T a] -> FSM a
fsmFromList l = FSM l . fromList

transFromState :: Eq a => S a -> Set (T a) -> Set (T a)
transFromState s = DS.filter $ (== s) . transStart

transWithLabel :: Char -> Set (T a) -> Set (T a)
transWithLabel c = DS.filter $ (== c) . symbol

endingStates :: Ord a => Set (T a) -> Set (S a)
endingStates = DS.map transEnd

fsmTransFromState :: Eq a => S a -> FSM a -> Set (T a)
fsmTransFromState s = transFromState s . transitions

succStates :: (Eq a, Ord a) => FSM a -> Char -> S a -> Set (S a)
succStates (FSM _ st) c s = endingStates $ transWithLabel c $ transFromState s st