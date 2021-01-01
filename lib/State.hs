module State where

import DotShow
import Helpers (clr, dim, red, grn)
import Set (Set)
import qualified Set as DS

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
initialStates = DS.filter $ \(S _ (i, _)) -> i

isInitialSet :: Set (S a) -> Bool
isInitialSet = not . DS.null . initialStates

finalStates :: Set (S a) -> Set (S a)
finalStates = DS.filter $ \(S _ (_, f)) -> f

isFinalSet :: Set (S a) -> Bool
isFinalSet = not . DS.null . finalStates

mergeStates :: Ord a => Bool -> Set (S a) -> S (Set a)
mergeStates i ss = S (DS.map stateLabel ss) (i, isFinalSet ss)

(|==|) :: Eq a => S a -> S a -> Bool
(S a _) |==| (S a' _) = a == a'

labelMember :: Eq a => S a -> Set (S a) -> Bool
labelMember sa = any (|==| sa)