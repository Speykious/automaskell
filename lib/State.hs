module State where

import DotShow
import Helpers (clr, dim, red, grn)
import Data.Set (Set)
import qualified Data.Set as DS

data S a = S a (Bool, Bool) deriving (Eq, Ord) -- State

instance Show a => Show (S a) where
  show (S a (i, f)) = dim ++ "S" ++ clr ++ show a
                    ++ (if i then grn ++ "#I" ++ clr else "")
                    ++ (if f then red ++ "#F" ++ clr else "")

instance Show a => DotShow (S a) where
  dotShow    (S a _)      = "S" ++ show a
  dotDeclare (S a (i, f)) = Just $ "S" ++ show a
                          ++ case params of
                            [a, b] -> " [" ++ a ++ "," ++ b ++ "];"
                            [a]    -> " [" ++ a ++ "];"
                            _      -> ";"
    where params = filter (/= "") [ if i then "color=red" else ""
                                  , if f then "peripheries=2" else "" ]

initialStates :: Set (S a) -> Set (S a)
initialStates = DS.filter $ \(S _ (i, _)) -> i

finalStates :: Set (S a) -> Set (S a)
finalStates = DS.filter $ \(S _ (_, f)) -> f