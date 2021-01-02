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

-- | Finite state machine / Automaton, consisting of a label and a set of transitions.
data FSM a = FSM { label :: String          -- ^ The label, or name of the automaton.
                 , transitions :: Set (T a) -- ^ The set of transitions of the automaton.
                                            -- ^ Since they contain the states, we don't include
                                            -- ^ the set of all states in the structure itself,
                                            -- ^ as it can be obtained through the transitions.
                 } deriving (Eq, Ord)



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



-- | Add a set of transitions to an automaton.
fsmAddTrans :: Ord a => FSM a -> Set (T a) -> FSM a
fsmAddTrans (FSM l st) sta = FSM l (st <> sta)

-- | Get the transitions having this state as a starting point.
fsmTransFromState :: Eq a => S a -> FSM a -> Set (T a)
fsmTransFromState s = transFromState s . transitions

-- | Get the transitions having some state as a starting point, from all the states that this state represents.
fsmTransFromSetState :: Ord a => S (Set a) -> FSM a -> Set (T a)
fsmTransFromSetState s = DS.filter ((`DS.member` stateLabel s) . stateLabel . transStart) . transitions

-- | Get the paired transitions from a pair state in a pair of automata.
fsmTransFromPairState :: (Eq a, Eq b) => S (a, b) -> (FSM a, FSM b) -> (Set (T a), Set (T b))
fsmTransFromPairState (S (a, b) _) (ma, mb) = (filtrans a ma, filtrans b mb)
  where filtrans l = DS.filter ((== l) . stateLabel . transStart) . transitions

-- | Create an automaton from a list of transitions.
fsmFromList :: Ord a => String -> [T a] -> FSM a
fsmFromList l = FSM l . fromList

-- | Get the states of an automaton.
fsmStates :: Ord a => FSM a -> Set (S a)
fsmStates = statesFromTrans . transitions

-- | Get the initial states of an automaton.
fsmInitialStates :: Ord a => FSM a -> Set (S a)
fsmInitialStates = initialStates . fsmStates

-- | Get the supposedly unique initial state of an automaton. Errors out if there are none.
fsmInitialState :: Ord a => FSM a -> Maybe (S a)
fsmInitialState = initialState . fsmStates

-- | Get the final states of an automaton.
fsmFinalStates :: Ord a => FSM a -> Set (S a)
fsmFinalStates = finalStates . fsmStates

-- | Get all the succeeding states from a state and a symbol.
succState :: Ord a => FSM a -> Char -> S a -> Set (S a)
succState (FSM _ st) c s = endingStates $ transWithLabel c $ transFromState s st

-- | Get all the succeeding states from a state and an alphabet.
allSuccState :: Ord a => FSM a -> String -> S a -> [Set (S a)]
allSuccState m alpha s = (\c -> succState m c s) <$> alpha

-- | Get all the succeeding states from a set of states and a symbol.
succStates :: Ord a => FSM a -> Char -> Set (S a) -> Set (S a)
succStates m c = (<>>=> succState m c)

-- | Execute an automaton on a series of symbols.
fsmExec :: Ord a => FSM a -> String -> Bool
fsmExec m = isFinalSet . exec mis
  where mis = fsmInitialStates m
        exec ss _ | DS.null ss = ss
        exec ss []     = ss
        exec ss (c:cs) = exec (succStates m c ss) cs



-- | Whether the automaton is complete.
isComplete :: Ord a => String -> FSM a -> Bool
isComplete alpha m = and $ (notElem empty . allSuccState m alpha) <<$>> fsmStates m

-- | Get all individual symbols for each state from an automaton, with the states.
symbolsWithStates :: Ord a => FSM a -> [(S a, String)]
symbolsWithStates m = (\s -> (s, (symbol <$>) $ elems $ fsmTransFromState s m)) <$> elems (fsmStates m)

-- | Get all individual symbols for each state from an automaton, without the states.
symbolsFromStates :: Ord a => FSM a -> [String]
symbolsFromStates = map snd . symbolsWithStates

-- | Get the alphabet from the state of an automaton.
fsmAlphaFromState :: Eq a => S a -> FSM a -> String
fsmAlphaFromState s = alphaFromTrans . fsmTransFromState s

-- | Whether the automaton is deterministic.
isDeterministic :: Ord a => FSM a -> Bool
isDeterministic m = vi m && vt m
  where vi = (<= 1) . DS.size . fsmInitialStates
        vt = not . any hasDuplicates . symbolsFromStates

-- | Outputs a completed version of the automaton.
complete :: (Ord a, Show a) => a -> String -> FSM a -> FSM a
complete a alpha m = if isComplete alpha m then m
                     else fsmFromList ("complete_" ++ label m)
                        $ trashitions ++ complitions ++ elems (transitions m)
  where trash = S a (False, False)
        trashitions = (\c -> T trash c trash) <$> alpha
        complitions = symbolsWithStates m
                    >>= (\(s, cs) -> (\c -> T s c trash) <$> cs) . second (`alphaComp` alpha)



-- | New transition generator. Used to generate automata by construction, such as DFA conversion, intersection and union.
type NewTransGen a w = S a       -- ^ The current state to generate transitions for.
                    -> Set (S a) -- ^ The stack of handled states.
                    -> w         -- ^ Whatever object is used to generate the transitions from.
                    -> Set (T a)

-- | Generates a recursive transition generator.
generateTransFn :: Ord a => NewTransGen a w -> NewTransGen a w
generateTransFn gnt s stack w = snt <> snnt
  where snt  = gnt s stack w
        sans = DS.filter (not . (`labelMember` stack)) (endingStates snt)
        snnt = sans <>>=> (\ans -> generateTransFn gnt ans (stack <> sans) w)

-- | Generates an automaton by construction, starting from an initial state using a recursive transition generator.
generateFSMFn :: Ord a => String -> S a -> NewTransGen a w -> w -> FSM a
generateFSMFn l i gnt w = FSM l $ generateTransFn gnt i (DS.singleton i) w



-- | Recursive transition generator for the conversion to DFA.
generateDFATrans :: Ord a => NewTransGen (Set a) (FSM a)
generateDFATrans = generateTransFn gnt
  where gnt css stack m = (\st -> T css (aFromTrans st) (getEndingState st)) <<$>> csst
          where csst = groupBySymbol $ fsmTransFromSetState css m
                getEndingState st = created `fromMaybe` found
                  where created = createEndingState st
                        found   = find (|==| created) stack

-- | Converts an automaton to a DFA.
convertToDFA :: (Show a, Ord a) => FSM a -> FSM (Set a)
convertToDFA m = generateFSMFn (label m ++ "_deter")
                               (mergeStates True $ fsmInitialStates m)
                               generateDFATrans m



-- | Recursive transition generator for the intersection of two DFA.
intersecTrans :: (Ord a, Ord b) => NewTransGen (a, b) (FSM a, FSM b)
intersecTrans = generateTransFn gnt
  where gnt css stack (ma, mb) = (\tp -> T css (aFromTransPair tp) (pairEndingStates (&&) tp)) <<$>> cstp
          where (sta, stb) = fsmTransFromPairState css (ma, mb)
                alpha = DS.elems $ DS.fromList (alphaFromTrans sta ++ alphaFromTrans stb)
                cstp = fromList $ catMaybes $ (\c -> zipMaybe (cfind c sta, cfind c stb)) <$> alpha
                  where cfind c = find $ (== c) . symbol

-- | Outputs the intersection of two automata.
-- # Hypothesis
-- Both have to be deterministic, but not necessarily complete. However, the function doesn't check.
(<&>) :: (Ord a, Ord b) => FSM a -> FSM b -> FSM (a, b)
ma <&> mb = generateFSMFn (label ma ++ "_and_" ++ label mb)
                          (pairStates True (&&) ( fromJust $ fsmInitialState ma
                                                , fromJust $ fsmInitialState mb ))
                          intersecTrans (ma, mb)



-- | Is *supposed* to output the union of two automata. It just doesn't work for now.
(<|>) :: Ord a => FSM a -> FSM a -> FSM a
(FSM la sta) <|> (FSM lb stb) = FSM (la ++ "_" ++ lb) (sta <> stb)
