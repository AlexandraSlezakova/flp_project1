module DFAModule(
  eliminateStates
) where

import FAModule

eliminateStates :: FAStruct -> IO()
eliminateStates fa = do
  -- set of states is initialized with initial state
  let initState = startState fa
  let states = [initState]
  -- transitions from initial state to another state are found
  let list = []
  let t = transitions fa
  let k = getTransitions [] t initState

  printTransitions k

  let s = addDestinationStates k states
  putStrLn $ show s
  putStrLn "tu"


------------------------------------
-- Get transitions from source state
------------------------------------
getTransitions :: [Transition] -> [Transition] -> State -> [Transition]
getTransitions list [] sourceState = list
getTransitions list (transition:transitions) sourceState =
  if (src transition) == sourceState
    then do
      getTransitions (list ++ [transition]) transitions sourceState
    else
      getTransitions list transitions sourceState


----------------------------------------------------------
-- Add destination states from transitions to set of states
-- if it is not already in set
----------------------------------------------------------
addDestinationStates :: [Transition] -> [State] -> [State]
addDestinationStates [] states = states
addDestinationStates (transition:transitions) states =
  if not $ (dst transition) `elem` states
    then do
      addDestinationStates transitions (states ++ [(dst transition)])
    else
      addDestinationStates transitions states
