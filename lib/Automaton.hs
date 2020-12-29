module Automaton where

data S a = S a (Bool, Bool)   deriving (Eq, Ord) -- State
data T a = T (S a) Char (S a) deriving (Eq, Ord) -- Transition

clr = "\x1b[0m"
bld = "\x1b[1m"
red = "\x1b[31m"
grn = "\x1b[32m"
yel = "\x1b[33m"

instance Show a => Show (S a) where
  show (S a (i, f)) = show a
                    ++ (if i then grn ++ "#I" ++ clr else "")
                    ++ (if f then red ++ "#F" ++ clr else "")

instance Show a => Show (T a) where
  show (T sa c sb) = show sa ++ " "
                   ++ yel ++ ">-" ++ [c] ++ "->" ++ clr
                   ++ " " ++ show sb