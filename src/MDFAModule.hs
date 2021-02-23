{-|
Project     :
Author      :
Year        :
Module      : DFAModule
Description : An implementation of the minimization of deterministic finite automaton

The implementation is realised in 3 steps according to algorithms from TIN scripts:
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
minimizeDFA dfa = do
  let completeDFA = removeUnreachableStates dfa
  let finalStatesSet = (acceptStates completeDFA)
  let nonFinalStatesSet = (states completeDFA) \\ finalStatesSet
  let newStates = createStates [] ([finalStatesSet] ++ [nonFinalStatesSet]) (transitions completeDFA)

  --putStrLn $ show m
  --createReducesDFA completeDFA
  reduceDFA newStates completeDFA

  putStrLn "tu"

  --printDFA completeDFA

reduceDFA :: [[State]] -> DFAStruct -> IO()
reduceDFA mdfaState completeDFA = do
  -- original initial state
  let initialState = concat (filter (\x -> ((startState completeDFA) `elem` x)) mdfaState)
  -- set of states and the corresponding new state
  let stateMap = [(initialState, 0)]
  let initStateT = getTransitionsOfState (startState completeDFA) (transitions completeDFA)
  -- create first transition for initial state
  let stateMapT = createTransition initStateT [] mdfaState stateMap
  --printTransitions (snd stateMapT)
  let newTransitions = createTransitions (transitions completeDFA) mdfaState stateMapT []
  let finalStates = snd (head (filter (\x -> ((head (acceptStates completeDFA) `elem` (fst x)))) (fst newTransitions)))
  --createTransitions (transitions completeDFA) mdfaState stateMapT []
  --createTransitions (transitions completeDFA) states s []
  --putStrLn $ show states
  --putStrLn $ show m
  putStrLn "finalStates"
  print finalStates
  printTransitions (snd newTransitions)
  putStrLn "symbol"

createTransitions :: [Transition] -> [[State]] -> (StateMap, [Transition]) -> [Transition] -> (StateMap, [Transition])
createTransitions originalT mdfaState stateMapT newT
  | length (fst stateMapT) < length mdfaState = createTransitions originalT mdfaState stateMapTr newTr
  | otherwise = ((fst stateMapT), (newTr ++ (snd stateMapTr)))
  where
    state = head (fst (last (fst stateMapT)))
    newTr = newT ++ (snd stateMapT)
    -- get transitions of next state
    t = getTransitionsOfState state originalT
    -- create transitions for next state
    stateMapTr = createTransition t [] mdfaState (fst stateMapT)


createTransition :: [Transition] -> [Transition] -> [[State]] -> StateMap -> (StateMap, [Transition])
createTransition [] newT newStates states = (states, newT)
createTransition (t:transitions) newT newStates states = do
  let state = filter (\s -> (elem (dst t) (fst s))) states
  if state == []
    then do
      -- if new state is not in set of states, take last one and increment its value
      let lastState = if length states == 1 then head states else head (tail states)
      let newStateNum = (snd lastState) + 1
      -- get set of states of minimized DFA
      let s = concat (filter (\x -> ((dst t) `elem` x)) newStates)
      -- create new transition from last state
      let newTr = [Transition (show (snd lastState)) (symbol t) (show newStateNum)]
      createTransition transitions (newT ++ newTr) newStates (states ++ [(s, newStateNum)])
    else do
      -- loop over same state
      let stateNum = (snd (head state))
      let newTr = [Transition (show stateNum) (symbol t) (show stateNum)]
      createTransition transitions (newT ++ newTr) newStates states


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
      p = if statesExist pair partitions then partitions else editSet partitions isDistinguishable pair


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
    else do
      findPartition [] isIndistinguishable pairOfStates
  where
    k = map(\x -> elem (fst pairOfStates) x) partitions
    index = if True `elem` k then fromJust $ elemIndex True k else -1


-- |If two states of set are distinguishable, the set is split into different sets
-- |otherwise it is added to existing partition
findPartition :: [State] -> Bool -> (State, State) -> [[State]]
findPartition p isIndistinguishable pairOfStates =
  if isIndistinguishable then
    if (fst pairOfStates) `elem` p
      -- two states of set are indistinguishable
      then [p ++ [(snd pairOfStates)]]
      else do
        if p == [] then [[(fst pairOfStates)] ++ [(snd pairOfStates)]] else [p]
    else do
      -- two states of set are distinguishable
      if (fst pairOfStates) `elem` p
        then [p] ++ [[(snd pairOfStates)]]
        else [[(fst pairOfStates)]] ++ [[(snd pairOfStates)]]


-- |Check if both states from pair are in partition
statesExist :: (State, State) -> [[State]] -> Bool
statesExist pairOfStates partitions = (stateExists (fst pairOfStates) partitions)
  + (stateExists (snd pairOfStates) partitions) > 1
  where
    stateExists state set = length (filter (\p -> ((elem state p))) set)


-- |Check if two state are indistinguishable
areIndistinguishable :: (State, State) -> [[State]] -> [Transition] -> Bool
areIndistinguishable pairOfStates setOfSetsOfStates transitions = do
  -- all transitions of first state
  let state1T = getTransitionsOfState (fst pairOfStates) transitions
  -- all transitions of second state
  let state2T = getTransitionsOfState (snd pairOfStates) transitions
  -- destination states with same input symbol
  let ds = [((dst t1), (dst t2)) | t1 <- state1T, t2 <- state2T, (symbol t1) == (symbol t2)]
  isInSameSet ds setOfSetsOfStates


-- |Destination states of two indistinguishable states is in same set of states (final or non-final)
isInSameSet :: [(State, State)] -> [[State]] -> Bool
isInSameSet [] setOfSetsOfStates = True
isInSameSet (pair:destinationStates) setOfSetsOfStates = do
  let ret = filter(\setOfStates -> (elem (fst pair) setOfStates && elem (snd pair) setOfStates)) setOfSetsOfStates
  if ret == [] then False else isInSameSet destinationStates setOfSetsOfStates


-- |Remove unreachable states from deterministic finite automaton
-- |and check if DFA is complete
removeUnreachableStates :: DFAStruct -> DFAStruct
removeUnreachableStates dfa =
  DFAStruct {
    states        = if sinkT == [] then (fst reachable) else (fst reachable) ++ ["x"],
    alphabet      = (alphabet dfa),
    startState    = initState,
    acceptStates  = getReachableAcceptStates (acceptStates dfa) reachableS,
    transitions   = sort $ t ++ sinkT
  }
  where
    initState = startState dfa
    -- states without initial state
    s = ((states dfa) \\ [initState])
    -- only reachable states and transitions
    reachable = eliminateStates [] (transitions dfa) s [initState]
    reachableS = fst reachable
    reachableT = snd reachable
    -- transitions over same state
    tOverSameState = findTransitionOverSameState (transitions dfa) reachableT reachableS
    -- accept state that are in set of reachable states
    getReachableAcceptStates acceptStates reachableStates =
      filter (\state -> (elem state reachableStates)) acceptStates
    t = reachableT ++ tOverSameState
    -- check if DFA is complete and get transitions to sink state
    sinkT = isComplete t reachableS (alphabet dfa)


-- |Check if DFA is complete, if not create transition to sink state
isComplete :: [Transition] -> [State] -> Alphabet -> [Transition]
isComplete transitions states alphabet
  | (missingT transitions states alphabet) == [] = []
  | otherwise = createSinkStateT $ missingT transitions states alphabet
  where
    -- all possible transitions for every symbol from set of input symbols
    allT states alphabet = [(x, y) | x <- states, y <- chunksOf 1 alphabet]
    -- existing transitions
    existingT transitions = [((src t),(symbol t)) | t <- transitions]
    -- missing transitions
    missingT transitions states alphabet = (allT states alphabet) \\ (existingT transitions)
    -- create transitions with sink state represented as x
    createSinkStateT missingT = [Transition (fst t) (snd t) "x" | t <- missingT]


-- |For every state from set find transition
-- |where source state is in reachableStates set
eliminateStates :: [Transition] -> [Transition] -> [State] -> [State] -> ([State],[Transition])
eliminateStates reachableT transitions [] reachableS = (reachableS, reachableT)
eliminateStates reachableT transitions (state:states) reachableS = do
  -- find all transitions from some of the reachable states to destination state
  let edge = findTransition transitions state reachableS
  let s = saveReachableStates reachableS edge
  eliminateStates (reachableT ++ edge) transitions states s


-- |For every reachable state find transition to destination state
findTransition :: [Transition] -> State -> [State] -> [Transition]
findTransition transitions dstState reachableStates =
  filter (\transition -> (elem (src transition) reachableStates
    && ((dst transition) == dstState))) transitions


-- |Check if set of transitions doesn't contain transition over state
-- |that is in set of reachable states
findTransitionOverSameState :: [Transition] -> [Transition] -> [State] -> [Transition]
findTransitionOverSameState transitions reachableT reachableStates =
  filter (\transition -> ((elem (src transition) reachableStates
    && (elem (dst transition) reachableStates))
    && (not $ elem transition reachableT))) transitions


-- |Add reachable state to set of reachable states
saveReachableStates :: [State] -> [Transition] -> [State]
saveReachableStates reachableStates [] = reachableStates
saveReachableStates reachableStates (transition:transitions) = do
  if not $ (dst transition) `elem` reachableStates
    then saveReachableStates (reachableStates ++ [(dst transition)]) transitions
    else saveReachableStates reachableStates transitions
