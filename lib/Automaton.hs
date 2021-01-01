module Automaton where

import DotShow
import Helpers
import State
import Transition
import Set (Set, fromList, empty, elems)
import qualified Set as DS
import Data.List (foldl', lines, intercalate)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)

import Debug.Trace (trace)

data FSM a = FSM { label :: String
                 , transitions :: Set (T a) } deriving (Eq, Ord) -- Finite State Machine



instance (Ord a, Show a) => Show (FSM a) where
  show (FSM l st) = bld ++ "Automaton " ++ l ++ clr ++ ":\n"
                  ++ bld ++ "  States" ++ clr ++ ":\n    "
                  ++ indentedSet (statesFromTrans st) ++ "\n"
                  ++ bld ++ "  Transitions" ++ clr ++ ":\n    "
                  ++ indentedSet st
    where indentedSet :: (Show a) => Set a -> String
          indentedSet = intercalate "\n    " . lines . show

instance (Ord a, Show a) => DotShow (FSM a) where
  dotShow (FSM l st) = "digraph " ++ l ++ " {\n"
                     ++ "  rankdir=LR;\n"
                     ++ unlines (("  " ++) . dotShow <$> elems st)
                     ++ unlines (("  " ++) . unwrap . dotDeclare <$> elems (statesFromTrans st))
                     ++ "}"
    where unwrap (Just a) = a
          unwrap Nothing = error "Trying to unwrap Nothing"

  dotPDF m = dotShowPDF ("cache/" ++ label m) m



fsmAddTrans :: Ord a => FSM a -> Set (T a) -> FSM a
fsmAddTrans (FSM l st) sta = FSM l (st <> sta)

fsmTransFromState :: Eq a => S a -> FSM a -> Set (T a)
fsmTransFromState s = transFromState s . transitions

fsmTransFromSetState :: Ord a => S (Set a) -> FSM a -> Set (T a)
fsmTransFromSetState s = DS.filter ((`DS.member` stateLabel s) . stateLabel . transStart) . transitions

fsmFromList :: Ord a => String -> [T a] -> FSM a
fsmFromList l = FSM l . fromList

fsmStates :: Ord a => FSM a -> Set (S a)
fsmStates = statesFromTrans . transitions

fsmInitialStates :: Ord a => FSM a -> Set (S a)
fsmInitialStates = initialStates . fsmStates

fsmFinalStates :: Ord a => FSM a -> Set (S a)
fsmFinalStates = finalStates . fsmStates

succState :: Ord a => FSM a -> Char -> S a -> Set (S a)
succState (FSM _ st) c s = endingStates $ transWithLabel c $ transFromState s st

allSuccState :: Ord a => FSM a -> String -> S a -> [Set (S a)]
allSuccState m alpha s = (\c -> succState m c s) <$> alpha

succStates :: Ord a => FSM a -> Char -> Set (S a) -> Set (S a)
succStates m c = (<>>=> succState m c)

fsmExec :: Ord a => FSM a -> String -> Bool
fsmExec m = isFinalSet . exec mis
  where mis = fsmInitialStates m
        exec ss _ | DS.null ss = ss
        exec ss []     = ss
        exec ss (c:cs) = exec (succStates m c ss) cs



isComplete :: Ord a => String -> FSM a -> Bool
isComplete alpha m = and $ DS.map (notElem empty . allSuccState m alpha) (fsmStates m)

symbolsWithStates :: Ord a => FSM a -> [(S a, String)]
symbolsWithStates m = (\s -> (s, (symbol <$>) $ elems $ fsmTransFromState s m)) <$> elems (fsmStates m)

symbolsFromStates :: Ord a => FSM a -> [String]
symbolsFromStates = map snd . symbolsWithStates

fsmAlphaFromState :: Eq a => S a -> FSM a -> String
fsmAlphaFromState s = alphaFromTrans . fsmTransFromState s

isDeterministic :: Ord a => FSM a -> Bool
isDeterministic = not . any hasDuplicates . symbolsFromStates

complete :: Ord a => a -> String -> FSM a -> FSM a
complete a alpha m = fsmFromList ("complete_" ++ label m)
                   $ trashitions ++ complitions ++ elems (transitions m)
  where trash = S a (False, False)
        trashitions = (\c -> T trash c trash) <$> alpha
        complitions = symbolsWithStates m
                    >>= (\(s, cs) -> (\c -> T s c trash) <$> cs) . second (`alphaComp` alpha)



convertToDFA :: (Show a, Ord a) => FSM a -> FSM (Set a)
convertToDFA m = FSM (label m ++ "_deter") $ generateTrans initial (DS.singleton initial) m
  where initial = mergeStates True $ fsmInitialStates m
        generateTrans css stack m = snt <> snnt
          where getEndingState st = created `fromMaybe` found
                  where created = createEndingState st
                        found   = find (|==| created) stack
                csst = groupBySymbol $ fsmTransFromSetState css m
                snt  = (\st -> T css (aFromTrans st) (getEndingState st)) <<$>> csst
                sans = DS.filter (not . (`labelMember` stack)) (endingStates snt)
                snnt = sans <>>=> (\ans -> generateTrans ans (stack <> sans) m)



intersecTrans :: S (a, b) -> Set (S (a, b)) -> FSM a -> FSM b -> Set (T (a, b))
intersecTrans css stack ma mb = undefined
  

(<&>) :: (Ord a, Ord b) => FSM a -> FSM b -> FSM (a, b)
(FSM la sta) <&> (FSM lb stb) = FSM (la ++ "_and_" ++ lb) undefined 



(<|>) :: Ord a => FSM a -> FSM a -> FSM a
(FSM la sta) <|> (FSM lb stb) = FSM (la ++ "_" ++ lb) (sta <> stb)
