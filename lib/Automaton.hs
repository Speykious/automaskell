module Automaton where

import DotShow
import Helpers
import State
import Transition
import Set (Set, fromList, empty, elems)
import qualified Set as DS
import Data.List (foldl', lines, intercalate)
import Data.Foldable (find)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
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
                     ++ unlines (("  " ++) . fromJust . dotDeclare <$> elems (statesFromTrans st))
                     ++ "}"

  dotPDF m = dotShowPDF ("cache/" ++ label m) m



fsmAddTrans :: Ord a => FSM a -> Set (T a) -> FSM a
fsmAddTrans (FSM l st) sta = FSM l (st <> sta)

fsmTransFromState :: Eq a => S a -> FSM a -> Set (T a)
fsmTransFromState s = transFromState s . transitions

fsmTransFromSetState :: Ord a => S (Set a) -> FSM a -> Set (T a)
fsmTransFromSetState s = DS.filter ((`DS.member` stateLabel s) . stateLabel . transStart) . transitions

fsmTransFromPairState :: (Eq a, Eq b) => S (a, b) -> (FSM a, FSM b) -> (Set (T a), Set (T b))
fsmTransFromPairState (S (a, b) _) (ma, mb) = (filtrans a ma, filtrans b mb)
  where filtrans l = DS.filter ((== l) . stateLabel . transStart) . transitions

fsmFromList :: Ord a => String -> [T a] -> FSM a
fsmFromList l = FSM l . fromList

fsmStates :: Ord a => FSM a -> Set (S a)
fsmStates = statesFromTrans . transitions

fsmInitialStates :: Ord a => FSM a -> Set (S a)
fsmInitialStates = initialStates . fsmStates

fsmInitialState :: Ord a => FSM a -> Maybe (S a)
fsmInitialState = initialState . fsmStates

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
isDeterministic m = vi m && vt m
  where vi = (<= 1) . DS.size . fsmInitialStates
        vt = not . any hasDuplicates . symbolsFromStates

complete :: (Ord a, Show a) => a -> String -> FSM a -> FSM a
complete a alpha m = if isComplete alpha m then m
                     else fsmFromList ("complete_" ++ label m)
                        $ trashitions ++ complitions ++ elems (transitions m)
  where trash = S a (False, False)
        trashitions = (\c -> T trash c trash) <$> alpha
        complitions = symbolsWithStates m
                    >>= (\(s, cs) -> (\c -> T s c trash) <$> cs) . second (`alphaComp` alpha)

getEndingState :: (Ord a, Foldable t) => t (S (Set a)) -> Set (T a) -> S (Set a)
getEndingState stack st = created `fromMaybe` found
  where created = createEndingState st
        found   = find (|==| created) stack



type NewTransGen a w = S a -> Set (S a) -> w -> Set (T a)
generateTransFn :: Ord a => NewTransGen a w -> NewTransGen a w
generateTransFn gnt css stack w = snt <> snnt
  where snt  = gnt css stack w
        sans = DS.filter (not . (`labelMember` stack)) (endingStates snt)
        snnt = sans <>>=> (\ans -> generateTransFn gnt ans (stack <> sans) w)

generateFSMFn :: Ord a => String -> S a -> NewTransGen a w -> w -> FSM a
generateFSMFn l i gnt w = FSM l $ generateTransFn gnt i (DS.singleton i) w



generateDFATrans :: Ord a => S (Set a) -> Set (S (Set a)) -> FSM a -> Set (T (Set a))
generateDFATrans = generateTransFn gnt
  where gnt css stack m = (\st -> T css (aFromTrans st) (getEndingState stack st)) <<$>> csst
          where csst = groupBySymbol $ fsmTransFromSetState css m

convertToDFA :: (Show a, Ord a) => FSM a -> FSM (Set a)
convertToDFA m = generateFSMFn (label m ++ "_deter")
                               (mergeStates True $ fsmInitialStates m)
                               generateDFATrans m



intersecTrans :: (Ord a, Ord b) => S (a, b) -> Set (S (a, b)) -> (FSM a, FSM b) -> Set (T (a, b))
intersecTrans = generateTransFn gnt
  where gnt css stack (ma, mb) = (\tp -> T css (aFromTransPair tp) (pairEndingStates (&&) tp)) <<$>> cstp
          where (sta, stb) = fsmTransFromPairState css (ma, mb)
                alpha = DS.elems $ DS.fromList (alphaFromTrans sta ++ alphaFromTrans stb)
                cstp = fromList $ catMaybes $ (\c -> zipMaybe (cfind c sta, cfind c stb)) <$> alpha
                  where cfind c = find $ (== c) . symbol

(<&>) :: (Ord a, Ord b) => FSM a -> FSM b -> FSM (a, b)
ma <&> mb = generateFSMFn (label ma ++ "_and_" ++ label mb)
                          (pairStates True (&&) ( fromJust $ fsmInitialState ma
                                                , fromJust $ fsmInitialState mb ))
                          intersecTrans (ma, mb)



(<|>) :: Ord a => FSM a -> FSM a -> FSM a
(FSM la sta) <|> (FSM lb stb) = FSM (la ++ "_" ++ lb) (sta <> stb)
