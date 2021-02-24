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
  if (transitions dfa) == []
    then do
      let aut = DFAStruct {
        states        = ["0"],
        alphabet      = (alphabet dfa),
        startState    = "0",
        acceptStates  = [],
        transitions   = []
      }
      printDFA aut
    else do
      let completeDFA = removeUnreachableStates dfa
      --removeUnreachableStates dfa

      let finalStatesSet = (acceptStates completeDFA)
      let nonFinalStatesSet = (states completeDFA) \\ finalStatesSet

      let mdfaStates = createStates [] ([finalStatesSet] ++ [nonFinalStatesSet]) (transitions completeDFA)
      putStrLn $ show mdfaStates
      let k = removeSinkState mdfaStates []
      let t = removeSinkStateT (transitions completeDFA)
      -- printTransitions (transitions completeDFA)
      -- putStrLn "-------------------------"
      -- printTransitions t
      -- putStrLn "-------------------------"
      -- putStrLn $ show k
      -- putStrLn "completeDFA"
      -- printDFA completeDFA

      reduceDFA k t completeDFA

      putStrLn "tu"

  --printDFA completeDFA

removeSinkStateT :: [Transition] -> [Transition]
removeSinkStateT transitions = filter (\transition -> ((dst transition) /= "x")) transitions


removeSinkState :: [[State]] -> [[State]] -> [[State]]
removeSinkState [] removedSink = removedSink
removeSinkState (set:setOfSetsOfStates) removedSink
  | "x" `elem` set = removeSinkState setOfSetsOfStates (removedSink ++ [noSinkState])
  | otherwise = removeSinkState setOfSetsOfStates (removedSink ++ [set])
  where
    index = fromJust $ elemIndex "x" set
    noSinkState = (take index set ++ drop (1 + index) set)


reduceDFA :: [[State]] -> [Transition] -> DFAStruct -> IO()
reduceDFA mdfaStates transitions completeDFA = do
  -- original initial state
  let initialState = concat (filter (\x -> ((startState completeDFA) `elem` x)) mdfaStates)
  putStrLn "original"
  printTransitions transitions
  -- set of states and the corresponding new state
  let stateMap = [(initialState, 0)]
  let initStateT = getTransitionsOfState (startState completeDFA) transitions
  -- create first transition for initial state
  let stateMapT = createTransition initStateT [] mdfaStates stateMap
  putStrLn "stateMapT"
  printTransitions (snd stateMapT)
  let newT = createTransitions transitions mdfaStates stateMapT []
  let finalStates = snd (head (filter (\x -> ((head (acceptStates completeDFA) `elem` (fst x)))) (fst newT)))
  --createTransitions transitions mdfaStates stateMapT []
  putStrLn "-------------------------"
  putStrLn $ show (fst newT)
  --putStrLn $ show m
  putStrLn "finalStates"
  --print finalStates
  putStrLn "-------------------------"
  printTransitions (snd newT)
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
-- createTransitions :: [Transition] -> [[State]] -> (StateMap, [Transition]) -> [Transition] -> IO()
-- createTransitions originalT mdfaState stateMapT newT = do
--   let state = head (fst (last (fst stateMapT)))
--   let newTr = newT ++ (snd stateMapT)
--   -- get transitions of next state
--   let t = getTransitionsOfState state originalT
--   -- create transitions for next state
--   createTransition t [] mdfaState (fst stateMapT)
--   -- if length (fst stateMapT) < length mdfaState
--   --   then createTransitions originalT mdfaState stateMapTr newTr
--   --   else ((fst stateMapT), (newTr ++ (snd stateMapTr)))
--   putStrLn "-------------------------"




createTransition :: [Transition] -> [Transition] -> [[State]] -> StateMap -> (StateMap, [Transition])
createTransition [] newT newStates states = (states, newT)
createTransition (t:transitions) newT newStates states = do
  let dstState = filter (\s -> (elem (dst t) (fst s))) states
  let srcNum = head (filter (\s -> (elem (src t) (fst s))) states)

  if dstState == []
    then do
      -- if new state is not in set of states, take last one and increment its value
      let lastState = if length states == 1 then head states else last states
      let newStateNum = (snd lastState) + 1
      -- get set of states of minimized DFA
      let s = concat (filter (\x -> ((dst t) `elem` x)) newStates)
      -- create new transition from last state
      let newTr = [Transition (show (snd srcNum)) (symbol t) (show newStateNum)]
      createTransition transitions (newT ++ newTr) newStates (states ++ [(s, newStateNum)])
    else do
      let dstNum = (snd (head dstState))

      let newTr = [Transition (show (snd srcNum)) (symbol t) (show dstNum)]
      createTransition transitions (newT ++ newTr) newStates states


-- |Create states of minimal deterministic finite automaton
createStates :: [[State]] -> [[State]] -> [Transition] -> [[State]]
createStates oldSet setOfSetsOfStates transitions
  | oldSet == s = s
  | otherwise = createStates s s transitions
  where
    s = calculateSet setOfSetsOfStates setOfSetsOfStates transitions []
-- createStates :: [[State]] -> [[State]] -> [Transition] -> IO()
-- createStates oldSet setOfSetsOfStates transitions = do
--
--   calculateSet setOfSetsOfStates setOfSetsOfStates transitions []
--   putStrLn "calculateSet--------------"
--   -- putStrLn $ show s
--   -- if oldSet == s
--   --   then do
--   --     putStrLn "rovnake--------------"
--   --     putStrLn $ show s
--   --   else do
--   --     createStates s s transitions
--   --     putStrLn "znovu--------------"




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

-- calculateSet :: [[State]] -> [[State]] -> [Transition] -> [[State]] -> IO()
-- calculateSet [] setOfSetsOfStatesCopy transitions newSet = putStrLn "koniec"
-- calculateSet (setOfStates:setOfSetsOfStates) setOfSetsOfStatesCopy transitions newSet = do
--   let statesSet = [(x,y) | (x:rest) <- tails setOfStates, y <- rest, x /= y]
--   --putStrLn $ show statesSet
--   putStrLn $ show setOfSetsOfStatesCopy
--   if length setOfStates == 1
--     then do
--       calculateSet setOfSetsOfStates setOfSetsOfStatesCopy transitions (newSet ++ [setOfStates])
--       putStrLn "calculateSet"
--     else do
--       --let set = newSet ++ takePairs statesSet setOfSetsOfStatesCopy transitions []
--       takePairs statesSet setOfSetsOfStatesCopy transitions []
--       --putStrLn $ show setOfSetsOfStatesCopy
--       calculateSet setOfSetsOfStates setOfSetsOfStatesCopy transitions newSet
--       putStrLn "else"
--
--   putStrLn "calculateSet2"



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
      pairInSet = filter(\setOfStates -> (elem (fst pair) setOfStates && elem (snd pair) setOfStates)) partitions
      isState = statesExist pair partitions
      noSink = "x" `notElem` pair
      p = if isDistinguishable && isState && pairInSet == [] && noSink
            then concatPairSets pair partitions
            else if isState then partitions
            else editSet partitions isDistinguishable pair

-- takePairs :: [(State, State)] -> [[State]] -> [Transition] -> [[State]] -> IO()
-- takePairs [] setOfSetsOfStates transitions partitions = putStrLn "koniec"
-- takePairs (pair:pairOfStates) setOfSetsOfStates transitions partitions = do
--   putStrLn "------------------set-----------"
--   putStrLn $ show setOfSetsOfStates
--   let isDistinguishable = areIndistinguishable pair setOfSetsOfStates transitions
--   let pairInSet = filter(\setOfStates -> (elem (fst pair) setOfStates && elem (snd pair) setOfStates)) partitions
--   if isDistinguishable then putStrLn "true isDistinguishable" else putStrLn "false isDistinguishable"
--   let k = "x" `notElem` pair
--   putStrLn $ show pair
--   if k then putStrLn "nie je" else putStrLn "je"
--   let p = if isDistinguishable && (statesExist pair partitions) && pairInSet == []
--             then concatPairSets pair partitions
--             else if (statesExist pair partitions) then partitions
--             else editSet partitions isDistinguishable pair

  -- if isDistinguishable && (statesExist pair partitions) && pairInSet == []
  --   then concatPairSets pair partitions
  --   else putStrLn $ show "tu"

  --let p = if statesExist pair partitions then partitions else editSet partitions isDistinguishable pair
  --if statesExist pair partitions then putStrLn "true pair statesExist" else putStrLn "false statesExist"

  -- putStrLn "nove rozdelenie"
  -- putStrLn $ show p
  --
  -- takePairs pairOfStates setOfSetsOfStates transitions p

concatPairSets :: (State, State) -> [[State]] -> [[State]]
concatPairSets pair partitions = do
  let firstSet = concat (filter (\set -> ((fst pair) `elem` set)) partitions)
  let secondSet = concat (filter (\set -> ((snd pair) `elem` set)) partitions)
  (filter (\set -> ((fst pair) `notElem` set && (snd pair) `notElem` set)) partitions) ++ [(firstSet ++ secondSet)]
  --(map (\set -> (notElem (fst pair) set && notElem (snd pair) set)) partitions) ++ [(firstSet ++ secondSet)]
  --let k = restOfSet ++ [(firstSet ++ secondSet)]




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
  let ret = filter(\setOfStates -> (elem (fst pair) setOfStates
                  && elem (snd pair) setOfStates)) setOfSetsOfStates
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
    reachable = eliminateStates [] (transitions dfa) [initState] [initState] []
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

-- removeUnreachableStates :: DFAStruct -> IO()
-- removeUnreachableStates dfa = do
--   let initState = startState dfa
--   -- states without initial state
--   let s = ((states dfa) \\ [initState])
--   -- only reachable states and transitions let reachable =
--   eliminateStates [] (transitions dfa) [initState] [initState] []
--
--   --let reachableS = fst reachable
--   --let reachableT = snd reachable
--   --putStrLn $ show reachableS
--   -- -- transitions over same state
--   -- let tOverSameState = findTransitionOverSameState (transitions dfa) reachableT reachableS
--   -- -- accept state that are in set of reachable states
--   -- let getReachableAcceptStates acceptStates reachableStates =
--   --   filter (\state -> (elem state reachableStates)) acceptStates
--   -- let t = reachableT ++ tOverSameState
--   -- -- check if DFA is complete and get transitions to sink state
--   -- let sinkT = isComplete t reachableS (alphabet dfa)
--   -- DFAStruct {
--   --   states        = if sinkT == [] then (fst reachable) else (fst reachable) ++ ["x"],
--   --   alphabet      = (alphabet dfa),
--   --   startState    = initState,
--   --   acceptStates  = getReachableAcceptStates (acceptStates dfa) reachableS,
--   --   transitions   = sort $ t ++ sinkT
--   -- }
--   putStrLn "remove"


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
eliminateStates :: [Transition] -> [Transition] -> [State] -> [State] -> [State] -> ([State],[Transition])  --([State],[Transition])
eliminateStates reachableT transitions [] statesCopy reachableS = (reachableS, reachableT) -- putStrLn "eliminateStates"(reachableS, reachableT)
eliminateStates reachableT transitions (state:states) statesCopy reachableS = do
  -- putStrLn $ show reachableS
  -- putStrLn state
  if state `elem` reachableS
    then
      eliminateStates reachableT transitions states statesCopy reachableS
    else do
      -- find all transitions from some of the reachable states to destination state
      let edge = findTransition transitions state
      --printTransitions edge
      let statesDifference = [(dst t) | t <- edge]
      --putStrLn "difference"
      --putStrLn $ show statesDifference
      eliminateStates (reachableT ++ edge) transitions (states ++ statesDifference) statesCopy (reachableS ++ [state])
  --putStrLn "tu"
      -- if statesCopy == s
      --   then putStrLn "eliminateStates1"
      --   else do
      --     eliminateStates (reachableT ++ edge) transitions statesDifference statesCopy s
      --     putStrLn "eliminateStates2"

  --putStrLn "eliminateStates"
  -- if statesDifference == s
  --   then putStrLn "eliminateStates"
  --   else do
  --     eliminateStates (reachableT ++ edge) transitions statesDifference statesCopy s
  --     putStrLn "eliminateStates2"
  -- let edge = findTransition transitions (head statesDifference)
  -- printTransitions edge
  -- let s = saveReachableStates reachableS edge
  -- putStrLn $ show s
  -- let statesDifference = s \\ statesCopy
  -- putStrLn $ show statesDifference
  -- putStrLn $ show (last reachableS)


-- |For every reachable state find transition to destination state
findTransition :: [Transition] -> State -> [Transition]
findTransition transitions srcState =
  filter (\transition -> ((src transition) == srcState)) transitions


-- |Check if set of transitions doesn't contain transition over state
-- |that is in set of reachable states
findTransitionOverSameState :: [Transition] -> [Transition] -> [State] -> [Transition]
findTransitionOverSameState transitions reachableT reachableStates =
  filter (\transition -> ((elem (src transition) reachableStates
    && (elem (dst transition) reachableStates))
    && (not $ elem transition reachableT))) transitions
