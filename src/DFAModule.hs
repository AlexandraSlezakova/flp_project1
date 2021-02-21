module DFAModule(
  DFAStruct(..),
  Transition(..),
  State,
  Alphabet,
  Sink,
  printDFA,
  isDFAValid,
  printTransitions,
  getTransitionsOfState
) where

import Data.Function
import Data.Char
import Data.List

-- |synonym State for type String
type State = String
-- |synonym Alphabet for type String
type Alphabet = String
-- |synonym Sink for type String
type Sink = String


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
  putStrLn $ intercalate "," $ acceptStates dfa
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


-- |Check if start state is valid
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


-- |Check if automaton is valid
isDFAValid :: DFAStruct -> Bool
isDFAValid dfa =
  isSetOfStatesValid (states dfa)
  && isAlphabetValid (alphabet dfa)
  && isStartStateValid (startState dfa) (states dfa)
  && checkAcceptStates (acceptStates dfa) (states dfa)
  && checkTransitions (transitions dfa) (states dfa) (alphabet dfa)


getTransitionsOfState :: State -> [Transition] -> [Transition]
getTransitionsOfState state transitions =
    filter (\transition -> (state  == (src transition))) transitions
