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
import Parser


minimizeDFA :: DFAStruct -> IO()
minimizeDFA dfa = do
  let completeDFA = removeUnreachableStates dfa
  createReducesDFA completeDFA
  putStrLn "tu"

  --printDFA newDfa


createReducesDFA :: DFAStruct -> IO()
createReducesDFA completeDFA = do
  -- 0th partition: one set contains final states and the other non-final states
  --let partition0 = (acceptStates completeDFA):((states completeDFA) \\ (acceptStates completeDFA)):[]
  let nonFinalStates = ((states completeDFA) \\ (acceptStates completeDFA))
  -- combination of states from 1st set
  let combination1 = [(x,y) | (x:rest) <- tails (acceptStates completeDFA), y <- rest, x /= y]
  let combination2 = [(x,y) | (x:rest) <- tails nonFinalStates, y <- rest, x /= y]
  let k = takePairOfStates combination2 nonFinalStates (transitions completeDFA) []
  --putStrLn $ show combination2
  putStrLn $ show k
  --putStrLn $ show nonFinalStates
  putStrLn " tu"


takePairOfStates :: [(State, State)] -> [State] -> [Transition] -> [[State]] -> [[State]]
takePairOfStates [] setOfStates transitions partitions = partitions
takePairOfStates (pair:pairOfStates) setOfStates transitions partitions = do
  -- for every two states check what is their destination state
  -- if destination state is in same set of states, states are indistinguishable
  let isDistinguishable = areIndistinguishable pair setOfStates transitions

  let p = if partitions == []
            then do
              if not isDistinguishable
                then partitions ++ [[(fst pair)]] ++ [[(snd pair)]]
                else partitions ++ [[(fst pair)] ++ [(snd pair)]]
            else
              editPartitions partitions [] isDistinguishable pair

  takePairOfStates pairOfStates setOfStates transitions p


editPartitions :: [[State]] -> [[State]] -> Bool -> (State, State) -> [[State]]
editPartitions [] newPartitions isDistinguishable pairOfStates = newPartitions
editPartitions (p:partitions) newPartitions isDistinguishable pairOfStates = do
  if isDistinguishable then do
    if (fst pairOfStates) `elem` p
      then do
        let states = p ++ [(snd pairOfStates)]
        editPartitions partitions (newPartitions ++ [states]) isDistinguishable pairOfStates
      else do
        editPartitions partitions (newPartitions ++ [p]) isDistinguishable pairOfStates
    else do
      let n = newPartitions ++ [[(fst pairOfStates)]] ++ [[(snd pairOfStates)]]
      editPartitions partitions n isDistinguishable pairOfStates


areIndistinguishable :: (State, State) -> [State] -> [Transition] -> Bool
areIndistinguishable pairOfStates setOfStates transitions = do
  -- all transitions of first state
  let state1T = getTransitionsOfState (fst pairOfStates) transitions
  -- all transitions of second state
  let state2T = getTransitionsOfState (snd pairOfStates) transitions
  -- destination states with same input symbol
  let ds = [((dst t1), (dst t2)) | t1 <- state1T, t2 <- state2T, (symbol t1) == (symbol t2)]
  isInSameSet ds setOfStates


isInSameSet :: [(State, State)] -> [State] -> Bool
isInSameSet [] setOfStates = True
isInSameSet (pair:destinationStates) setOfStates =
  if (fst pair) `elem` setOfStates && (snd pair) `elem` setOfStates
    then isInSameSet destinationStates setOfStates
    else False


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
    existingT transitions = map (\transition -> ((src transition),(symbol transition)))(transitions)
    -- missing transitions
    missingT transitions states alphabet = (allT states alphabet) \\ (existingT transitions)
    -- create transitions with sink state represented as x
    createSinkStateT missingT = map (\t -> Transition (fst t) (snd t) "x") missingT


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
