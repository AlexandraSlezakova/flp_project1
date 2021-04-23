{-|
Project     : VUT FIT FLP Project 1 - DKA-2-MKA
Author      : Alexandra Slezakova (xsleza20)
Year        : 2021
Module      : MDFAModule
Description : An implementation of the minimization of deterministic finite automaton

The implementation is realized in 3 steps according to algorithms from TIN scripts:
elimination of the unreachable states, conversion of automaton to a fully defined DFA
and subsequently reduced DFA
-}

module MDFAModule(
  minimizeDFA
) where

import Data.List
import DFAModule
import Control.Monad
import Data.List.Split
import Data.Maybe
import Parser


minimizeDFA :: DFAStruct -> IO()
minimizeDFA dfa
  | null (transitions dfa) = printDFA (createEmptyDKA (alphabet dfa))
  | otherwise =
      printDFA (reduceDFA mdfaStates (transitions completeDFA) completeDFA)
  where
    -- remove unreachable states
    completeDFA = removeUnreachableStates dfa
    -- set of final states
    finalStatesSet = acceptStates completeDFA
    -- set of non-final states
    nonFinalStatesSet = states completeDFA \\ finalStatesSet
    -- new states of minimal DFA
    mdfaStates = createStates [] (finalStatesSet : [nonFinalStatesSet]) (transitions completeDFA)


-- |Create empty DFA when no transitions are as an input
createEmptyDKA :: Alphabet -> DFAStruct
createEmptyDKA alphabet = DFAStruct {
  states        = ["0", "1"],
  alphabet      = alphabet,
  startState    = "0",
  acceptStates  = [],
  transitions   = tr
}
  where
    st = ["0", "1"]
    tr = [Transition src [symbol] "1" | src <- st, symbol <- alphabet]


-- |Remove transitions to sink state
removeSinkStateT :: [Transition] -> [Transition] -> [[State]] -> [Transition]
removeSinkStateT transitions newT [] = newT
removeSinkStateT transitions newT (states:sinkStates) =
  newT ++ filter (\t -> (src t `notElem` states) && (dst t `notElem` states)) transitions


-- |Remove sink state from set of states of minimal DFA
removeSinkState :: [[State]] -> [[State]] -> [[State]]
removeSinkState [] removedSink = removedSink
removeSinkState (set:setOfSetsOfStates) removedSink
  | "x" `elem` set = removeSinkState setOfSetsOfStates removedSink
  | otherwise = removeSinkState setOfSetsOfStates (removedSink ++ [set])
  where
    index = fromJust $ elemIndex "x" set
    noSinkState = take index set ++ drop (1 + index) set


-- |Minimize the given DFA
reduceDFA :: [[State]] -> [Transition] -> DFAStruct -> DFAStruct
reduceDFA mdfaStates transitions completeDFA = DFAStruct {
  states        = sort [show (snd x) | x <- fst newST],
  alphabet      = alphabet completeDFA,
  startState    = "0",
  acceptStates  = getAcceptStates (fst newST) (acceptStates completeDFA) [],
  transitions   = snd stateMapT ++ snd newST
 }
 where
  -- original initial state
  initialState = concat (filter (\x -> startState completeDFA `elem` x) mdfaStates)
  -- set of states and the corresponding new state
  stateMap = [(initialState, 0)]
  initStateT = getTransitionsOfState (startState completeDFA) transitions
  -- create first transition for initial state
  stateMapT = createTransition initStateT [] mdfaStates stateMap
  difference = fst stateMapT \\ stateMap
  -- transitions and map of states of minimal DFA
  newST = createTransitions transitions mdfaStates stateMapT difference []


-- |Get numbers of accept states in minimal DFA
getAcceptStates :: StateMap -> [State] -> [State] -> [State]
getAcceptStates stateMap [] set = sort set
getAcceptStates stateMap (state:acceptStates) set
  | number `elem` set = getAcceptStates stateMap acceptStates set
  | otherwise = getAcceptStates stateMap acceptStates (set ++ [number])
  where
    number = show (snd (head (filter (\x -> state `elem` fst x) stateMap)))


-- |Create transitions for every state
createTransitions :: [Transition] -> [[State]] -> (StateMap, [Transition]) -> StateMap
  -> [Transition] -> (StateMap, [Transition])
createTransitions originalT mdfaState stateMapT [] newT = (fst stateMapT, newT)
createTransitions originalT mdfaState stateMapT (state:states) newT = do
  let s = head (fst state)
  -- get transitions of next state
  let t = getTransitionsOfState s originalT
  -- create transitions for next state
  let stateMapTr = createTransition t [] mdfaState (fst stateMapT)
  let difference = fst stateMapTr \\ fst stateMapT
  createTransitions originalT mdfaState stateMapTr (states ++ difference) (newT ++ snd stateMapTr)


-- |Create transition from destination state to source state in minimal DFA
createTransition :: [Transition] -> [Transition] -> [[State]] -> StateMap -> (StateMap, [Transition])
createTransition [] newT mdfaState states = (states, newT)
createTransition (t:transitions) newT mdfaState states = do
  let dstState = filter (elem (dst t) . fst) states
  let srcNum = head (filter (elem (src t) . fst) states)

  if null dstState
    then do
      -- if new state is not in set of states, take last one and increment its value
      let lastState = if length states == 1 then head states else last states
      let newStateNum = snd lastState + 1
      -- get set of states of minimized DFA
      let s = concat (filter (\x -> dst t `elem` x) mdfaState)
      -- create new transition from last state
      let newTr = [Transition (show (snd srcNum)) (symbol t) (show newStateNum)]
      createTransition transitions (newT ++ newTr) mdfaState (states ++ [(s, newStateNum)])
    else do
      let dstNum = snd (head dstState)
      let newTr = [Transition (show (snd srcNum)) (symbol t) (show dstNum)]
      createTransition transitions (newT ++ newTr) mdfaState states


-- |Create states of minimal deterministic finite automaton
createStates :: [[State]] -> [[State]] -> [Transition] -> [[State]]
createStates oldSet setOfSetsOfStates transitions
  | oldSet == s = s
  | otherwise = createStates s s transitions
  where
    s = calculateSet setOfSetsOfStates setOfSetsOfStates transitions []


-- |Find partitions by partitioning the different sets of states
-- |in each set of partition take all possible pair od states
calculateSet :: [[State]] -> [[State]] -> [Transition] -> [[State]] -> [[State]]
calculateSet [] setOfSetsOfStatesCopy transitions newSet = newSet
calculateSet (setOfStates:setOfSetsOfStates) setOfSetsOfStatesCopy transitions newSet
  | length setOfStates == 1 =
      calculateSet setOfSetsOfStates setOfSetsOfStatesCopy transitions (newSet ++ [setOfStates])
  | otherwise = do
      let set = newSet ++ takePairs statesSet setOfSetsOfStatesCopy transitions []
      calculateSet setOfSetsOfStates setOfSetsOfStatesCopy transitions set
  where
    statesSet = [(x,y) | (x:rest) <- tails setOfStates, y <- rest, x /= y]


-- |Check if two states of a set are distinguishable
-- |set is split into different sets in partition
takePairs :: [(State, State)] -> [[State]] -> [Transition] -> [[State]] -> [[State]]
takePairs [] setOfSetsOfStates transitions partitions = partitions
takePairs (pair:pairOfStates) setOfSetsOfStates transitions partitions =
    takePairs pairOfStates setOfSetsOfStates transitions p
    where
      -- for every two states check what is their destination state
      -- if destination state is in same set of states, states are indistinguishable
      isDistinguishable = areIndistinguishable pair setOfSetsOfStates transitions
      -- filter set in which pair is
      pairInSet = filter(\setOfStates -> elem (fst pair) setOfStates && elem (snd pair) setOfStates) partitions
      isState = statesExist pair partitions
      p | isDistinguishable && isState && null pairInSet = concatPairSets pair partitions
        | isState = partitions
        | otherwise = editSet partitions isDistinguishable pair


concatPairSets :: (State, State) -> [[State]] -> [[State]]
concatPairSets pair partitions = do
  let firstSet = concat (filter (\set -> fst pair `elem` set) partitions)
  let secondSet = concat (filter (\set -> snd pair `elem` set) partitions)
  filter (\set -> fst pair `notElem` set && snd pair `notElem` set) partitions
      ++ [firstSet ++ secondSet]



-- |Edit set of states
editSet :: [[State]] -> Bool -> (State, State) -> [[State]]
editSet partitions isIndistinguishable pairOfStates =
  if index /= -1
    then do
      -- store partition to edit
      let p = partitions!!index
      -- remove set
      let newP = take index partitions ++ drop (1 + index) partitions
      -- add new state to set
      newP ++ findPartition p isIndistinguishable pairOfStates
    else
      findPartition [] isIndistinguishable pairOfStates
  where
    k = map(\x -> fst pairOfStates `elem` x) partitions
    index = if True `elem` k then fromJust $ elemIndex True k else -1


-- |If two states of set are distinguishable, the set is split into different sets
-- |otherwise it is added to existing partition
findPartition :: [State] -> Bool -> (State, State) -> [[State]]
findPartition p isIndistinguishable pairOfStates
  | isIndistinguishable =
    if fst pairOfStates `elem` p
      -- two states of set are indistinguishable
      then
        if snd pairOfStates `elem` p then [p] else [p ++ [snd pairOfStates]]
      else
        if null p then [fst pairOfStates : [snd pairOfStates]] else [p]
    -- two states of set are distinguishable
  | fst pairOfStates `elem` p =
        if snd pairOfStates `elem` p then [p] else p : [[snd pairOfStates]]
  | otherwise = [fst pairOfStates] : [[snd pairOfStates]]


-- |Check if both states from pair are in partition
statesExist :: (State, State) -> [[State]] -> Bool
statesExist pairOfStates partitions = stateExists (fst pairOfStates) partitions
  + stateExists (snd pairOfStates) partitions > 1
  where
    stateExists state set = length (filter (\p -> state `elem` p) set)


-- |Check if two state are indistinguishable
areIndistinguishable :: (State, State) -> [[State]] -> [Transition] -> Bool
areIndistinguishable pairOfStates setOfSetsOfStates transitions = do
  -- all transitions of first state
  let state1T = getTransitionsOfState (fst pairOfStates) transitions
  -- all transitions of second state
  let state2T = getTransitionsOfState (snd pairOfStates) transitions
  -- destination states with same input symbol
  let ds = [(dst t1, dst t2) | t1 <- state1T, t2 <- state2T, symbol t1 == symbol t2]
  isInSameSet ds setOfSetsOfStates


-- |Destination states of two indistinguishable states is in same set of states (final or non-final)
isInSameSet :: [(State, State)] -> [[State]] -> Bool
isInSameSet [] setOfSetsOfStates = True
isInSameSet (pair:destinationStates) setOfSetsOfStates = do
  let ret = filter(\setOfStates -> elem (fst pair) setOfStates
                  && elem (snd pair) setOfStates) setOfSetsOfStates
  not (null ret) && isInSameSet destinationStates setOfSetsOfStates


-- |Remove unreachable states from deterministic finite automaton
-- |and check if DFA is complete
removeUnreachableStates :: DFAStruct -> DFAStruct
removeUnreachableStates dfa =
  DFAStruct {
    states        = if null sinkT then fst reachable else fst reachable ++ ["x"],
    alphabet      = alphabet dfa,
    startState    = initState,
    acceptStates  = getReachableAcceptStates (acceptStates dfa) reachableS,
    transitions   = sort $ t ++ sinkT
  }
  where
    initState = startState dfa
    -- states without initial state
    s = states dfa \\ [initState]
    -- only reachable states and transitions
    reachable = eliminateStates [] (transitions dfa) [initState] [initState] []
    reachableS = fst reachable
    reachableT = snd reachable
    -- transitions over same state
    tOverSameState = findTransitionOverSameState (transitions dfa) reachableT reachableS
    -- accept state that are in set of reachable states
    getReachableAcceptStates acceptStates reachableStates =
      filter (`elem` reachableStates) acceptStates
    t = reachableT ++ tOverSameState
    -- check if DFA is complete and get transitions to sink state
    sinkT = isComplete t reachableS (alphabet dfa)


-- |Check if DFA is complete, if not create transition to sink state
isComplete :: [Transition] -> [State] -> Alphabet -> [Transition]
isComplete transitions states alphabet
  | null (missingT transitions states alphabet) = []
  | otherwise = createSinkStateT $ missingT transitions states alphabet
  where
    -- all possible transitions for every symbol from set of input symbols
    allT states alphabet = [(x, y) | x <- states, y <- chunksOf 1 alphabet]
    -- existing transitions
    existingT transitions = [(src t, symbol t) | t <- transitions]
    -- missing transitions
    missingT transitions states alphabet = allT states alphabet \\ existingT transitions
    -- create transitions with sink state represented as x
    createSinkStateT missingT = [uncurry Transition t "x" | t <- missingT] ++ sinkSTransitions
    sinkSTransitions = if missingT transitions states alphabet /= []
      then [Transition "x" [symbol] "x" | symbol <- alphabet]
      else []


-- |For every state from set find transition
-- |where source state is in reachableStates set
eliminateStates :: [Transition] -> [Transition] -> [State] -> [State] -> [State] -> ([State],[Transition])
eliminateStates reachableT transitions [] statesCopy reachableS = (reachableS, reachableT)
eliminateStates reachableT transitions (state:states) statesCopy reachableS
  | state `elem` reachableS = eliminateStates reachableT transitions states statesCopy reachableS
  | otherwise = eliminateStates (reachableT ++ edge) transitions (states ++ statesDifference)
                statesCopy (reachableS ++ [state])
  where
    -- find all transitions from some of the reachable states to destination state
    edge = findTransition transitions state
    statesDifference = [dst t | t <- edge]


-- |For every reachable state find transition to destination state
findTransition :: [Transition] -> State -> [Transition]
findTransition transitions srcState =
  filter (\transition -> src transition == srcState) transitions


-- |Check if set of transitions doesn't contain transition over state
-- |that is in set of reachable states
findTransitionOverSameState :: [Transition] -> [Transition] -> [State] -> [Transition]
findTransitionOverSameState transitions reachableT reachableStates =
  filter (\transition -> (elem (src transition) reachableStates
    && dst transition `elem` reachableStates)
    && (transition `notElem` reachableT)) transitions
