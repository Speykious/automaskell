module State where

import DotShow
import Helpers (clr, dim, red, grn)
import Set (Set)
import qualified Set as DS
import Data.Foldable (find)

data S a = S { stateLabel :: a
             , stateFlags :: (Bool, Bool) } deriving (Eq, Ord) -- State

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

initialStates :: Set (S a) -> Set (S a)
initialStates = DS.filter $ fst . stateFlags

initialState :: Set (S a) -> Maybe (S a)
initialState = find $ fst . stateFlags

isInitialSet :: Set (S a) -> Bool
isInitialSet = not . DS.null . initialStates

finalStates :: Set (S a) -> Set (S a)
finalStates = DS.filter $ snd . stateFlags

isFinalSet :: Set (S a) -> Bool
isFinalSet = not . DS.null . finalStates

mergeStates :: Ord a => Bool -> Set (S a) -> S (Set a)
mergeStates i ss = S (DS.map stateLabel ss) (i, isFinalSet ss)

type BoolRelation = Bool -> Bool -> Bool
pairStates :: Ord a => Bool -> BoolRelation -> (S a, S b) -> S (a, b)
pairStates i op (S a (_, fa), S b (_, fb)) = S (a, b) (i, op fa fb)

(|==|) :: Eq a => S a -> S a -> Bool
(S a _) |==| (S a' _) = a == a'

labelMember :: Eq a => S a -> Set (S a) -> Bool
labelMember sa = any (|==| sa)