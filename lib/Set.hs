module Set where

import qualified Data.Set as DS
import Data.List (intercalate)
import qualified Data.List as DL
import Prelude hiding (null, map, filter)

-- | Wrapper for Data.Set's Set datatype which has an awful instance of show.
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

-- | Shows the set with curly braces, and takes the separator as an argument.
showSetg :: Show a => String -> Set a -> String
showSetg sep (Set s)
  | DS.null s = "{}"
  | otherwise = "{ " ++ intercalate sep (DL.foldl' (\acc a -> show a : acc) [] s) ++ " }"

-- | Shows the set with curly braces, on one line.
showSet :: Show a => Set a -> String
showSet = showSetg ", "

-- | Shows the set with curly braces, on multiple lines.
showSetLn :: (Show a) => Set a -> String
showSetLn = showSetg "\n, "

-- | The empty set.
empty :: Set a
empty = Set DS.empty

-- | Outputs the singleton of an element.
singleton :: a -> Set a
singleton = Set . DS.singleton

-- | Builds a set from a list of elements.
fromList :: Ord a => [a] -> Set a
fromList = Set . DS.fromList

-- | Returns the list of elements of the set.
elems :: Set a -> [a]
elems = DS.elems . innerSet

-- | Checks if the set is empty.
null :: Set a -> Bool
null = DS.null . innerSet

-- | Get the size of the set.
size :: Set a -> Int
size = DS.size . innerSet

-- | Checks if the element is a member of the set.
member :: Ord a => a -> Set a -> Bool
member a = DS.member a . innerSet

-- | Inserts an element in the set.
insert :: Ord a => a -> Set a -> Set a
insert a = Set . DS.insert a . innerSet

-- | Maps a function to the set.
map :: Ord b => (a -> b) -> Set a -> Set b
map f (Set a) = Set $ DS.map f a

-- | Filters elements from the set.
filter :: (a -> Bool) -> Set a -> Set a
filter f (Set a) = Set $ DS.filter f a

-- | Strict left-to-right fold of the set. 
foldl' :: (c -> b -> c) -> c -> Set b -> c
foldl' f a = DS.foldl' f a . innerSet