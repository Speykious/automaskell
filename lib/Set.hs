module Set where

import qualified Data.Set as DS
import Data.List (intercalate)
import qualified Data.List as DL
import Prelude hiding (null, map, filter)

newtype Set a = Set { innerSet :: DS.Set a } deriving (Eq, Ord)

instance Ord a => Monoid (Set a) where
  mempty = Set mempty

instance Ord a => Semigroup (Set a) where
  (Set a) <> (Set b) = Set (a <> b)

instance Foldable Set where
  foldr f a = DS.foldr f a . innerSet

instance Show a => Show (Set a) where
  show s = if length one > 100 then many else one
    where one  = showSet s
          many = showSetLn s

showSetg :: Show a => String -> Set a -> String
showSetg sep (Set s)
  | DS.null s = "{}"
  | otherwise = "{ " ++ intercalate sep (DL.foldl' (\acc a -> show a : acc) [] s) ++ " }"

showSet :: Show a => Set a -> String
showSet = showSetg ", "

showSetLn :: (Show a) => Set a -> String
showSetLn = showSetg "\n, "

empty :: Set a
empty = Set DS.empty

singleton :: a -> Set a
singleton = Set . DS.singleton

fromList :: Ord a => [a] -> Set a
fromList = Set . DS.fromList

elems :: Set a -> [a]
elems = DS.elems . innerSet

null :: Set a -> Bool
null = DS.null . innerSet

size :: Set a -> Int
size = DS.size . innerSet

member :: Ord a => a -> Set a -> Bool
member a = DS.member a . innerSet

map :: Ord b => (a -> b) -> Set a -> Set b
map f (Set a) = Set $ DS.map f a

filter :: (a -> Bool) -> Set a -> Set a
filter f (Set a) = Set $ DS.filter f a

foldl' :: (c -> b -> c) -> c -> Set b -> c
foldl' f a = DS.foldl' f a . innerSet