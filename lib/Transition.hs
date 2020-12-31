module Transition where

import DotShow
import Helpers (clr, yel, (<>>=>))
import State
import Set (Set, fromList, elems)
import qualified Set as DS

data T a = T { transStart :: S a
             , symbol :: Char
             , transEnd :: S a } deriving (Eq, Ord)

instance Show a => Show (T a) where
  show (T sa c sb) = show sa ++ " "
                   ++ yel ++ ">-" ++ [c] ++ "->" ++ clr
                   ++ " " ++ show sb

instance Show a => DotShow (T a) where
  dotShow (T sa c sb) = dotShow sa ++ " -> " ++ dotShow sb
                      ++ " [label=" ++ [c] ++ "];"

statesFromTrans :: Ord a => Set (T a) -> Set (S a)
statesFromTrans = (<>>=> \(T sa _ sb) -> fromList [sa, sb])

transFromState :: Eq a => S a -> Set (T a) -> Set (T a)
transFromState s = DS.filter $ (== s) . transStart

transWithLabel :: Char -> Set (T a) -> Set (T a)
transWithLabel c = DS.filter $ (== c) . symbol

endingStates :: Ord a => Set (T a) -> Set (S a)
endingStates = DS.map transEnd

createEndingState :: Ord a => Bool -> Set (T a) -> S (Set a)
createEndingState b = mergeStates b . endingStates

alphaFromTrans :: Set (T a) -> String
alphaFromTrans = elems . DS.map symbol

aFromTrans :: Set (T a) -> Char
aFromTrans = head . alphaFromTrans

groupBySymbol :: Ord a => Set (T a) -> Set (Set (T a))
groupBySymbol st = fromList $ (`transWithLabel` st) <$> alphaFromTrans st