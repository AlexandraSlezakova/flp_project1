module DFAModule(
  DFAStruct(..),
  Transition(..),
  State,
  Alphabet,
  Sink,
  StateMap,
  printDFA,
  isDFAValid,
  printTransitions,
  getTransitionsOfState
) where

import Data.Function
import Data.Char
import Data.List
import Control.Monad

-- |synonym State for type String
type State = String
-- |synonym Alphabet for type String
type Alphabet = String
-- |synonym Sink for type String
type Sink = String
-- |synonym StateMap for the set of states and the corresponding new state
type StateMap = [([State], Int)]


-- |data structure for transitions
data Transition = Transition {
  src     :: String,
  symbol  :: String,
  dst     :: String
} deriving (Eq, Ord)


-- |data structure for automaton
data DFAStruct = DFAStruct {
  states        :: [State],
  alphabet      :: Alphabet,
  startState    :: State,
  acceptStates  :: [State],
  transitions   :: [Transition]
}


-- |Print automaton
printDFA :: DFAStruct -> IO()
printDFA dfa = do
  -- states
  putStrLn $ intercalate "," $ states dfa
  -- alphabet
  putStrLn $ alphabet dfa
  -- start state
  putStrLn $ startState dfa
  -- accept states
  when ((acceptStates dfa) /= []) $ putStrLn $ intercalate "," $ acceptStates dfa
  -- transitions
  printTransitions $ transitions dfa


-- |Print transitions
printTransitions :: [Transition] -> IO()
printTransitions [] = putStr ""
printTransitions (transition:transitions) = do
   putStrLn $ src transition ++ "," ++ symbol transition ++ "," ++  dst transition
   printTransitions transitions


-- |Check if state contains only digits
isSetOfStatesValid :: [State] -> Bool
isSetOfStatesValid [] = True
isSetOfStatesValid (x:xs) = if all isDigit x then isSetOfStatesValid xs else False


-- |Check if alphabet contains lower letters
isAlphabetValid :: Alphabet -> Bool
isAlphabetValid [] = True
isAlphabetValid (x:xs) =
  if x `elem` ['a'..'z'] then isAlphabetValid xs else False


-- |Check if start state is digit and is in set of states
isStartStateValid :: State -> [State] -> Bool
isStartStateValid s states = all isDigit s && s `elem` states


-- |Check if accept state exists in set of states
checkAcceptStates :: [State] -> [State] -> Bool
checkAcceptStates [] states = True
checkAcceptStates (state:acceptStates) states  =
  if state `elem` states then checkAcceptStates acceptStates states else False


-- |Check if all transitions are valid
checkTransitions :: [Transition] -> [State] -> Alphabet -> Bool
checkTransitions [] states alphabet = True
checkTransitions (transition:transitions) states alphabet =
  if isTransitionValid transition states alphabet
    then checkTransitions transitions states alphabet
    else False


-- |Check if transition is valid:
-- |if source and destination state exist in set of states
-- |and if alphabet contains input symbol
isTransitionValid :: Transition -> [State] -> Alphabet -> Bool
isTransitionValid transition states alphabet = do
  if (src transition) `elem` states
    && isInfixOf (symbol transition) alphabet
    && (not $ null (symbol transition))
    && (dst transition) `elem` states
    then True
    else False


-- |Check if set of states has unique values
hasUniqueState :: [State] -> Bool
hasUniqueState [] = True
hasUniqueState (s:states) = s `notElem` states && hasUniqueState states


-- |Check if set of transitions has unique values
hasUniqueTransitions :: [Transition] -> Bool
hasUniqueTransitions [] = True
hasUniqueTransitions (t:transitions) = t `notElem` transitions && hasUniqueTransitions transitions


-- |Check if automaton is valid
isDFAValid :: DFAStruct -> String
isDFAValid dfa = do
  if (states dfa) == [""] || (states dfa) == []
    then "Error: Empty set of states"
    else if (acceptStates dfa) == [""] || (acceptStates dfa) == []
      then "Error: Empty set of accept states"
    else if (alphabet dfa) == ""
      then "Error: Empty alphabet"
    else if not $ isSetOfStatesValid (states dfa)
      then "Error: Invalid set of states"
    else if not $ isAlphabetValid (alphabet dfa)
      then "Error: Invalid alphabet"
    else if not $ isStartStateValid (startState dfa) (states dfa)
      then "Error: Invalid initial state"
    else if not $ checkAcceptStates (acceptStates dfa) (states dfa)
      then "Error: Invalid accept states"
    else if not $ hasUniqueState (states dfa)
      then "Error: Duplicates in set of states"
    else if not $ hasUniqueState (acceptStates dfa)
      then "Error: Duplicates in set of accept states"
    else if not $ checkTransitions (transitions dfa) (states dfa) (alphabet dfa)
      then "Error: Invalid transitions"
    else if not $ hasUniqueTransitions (transitions dfa)
      then "Error: Duplicates in set of transitions"
    else ""


-- |Get transition where source state is "state"
getTransitionsOfState :: State -> [Transition] -> [Transition]
getTransitionsOfState state transitions =
    filter (\transition -> (state  == (src transition))) transitions
