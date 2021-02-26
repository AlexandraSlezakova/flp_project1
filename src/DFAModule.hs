{-|
Project     : VUT FIT FLP Project 1 - DKA-2-MKA
Author      : Alexandra Slezakova (xsleza20)
Year        : 2021
Module      : DFAModule
Description : Print and validation of input
-}

module DFAModule(
  DFAStruct(..),
  Transition(..),
  State,
  Alphabet,
  Sink,
  StateMap,
  printDFA,
  isDFAValid,
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
  when (acceptStates dfa /= []) $ putStrLn $ intercalate "," $ acceptStates dfa
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
isSetOfStatesValid = foldr ((&&) . all isDigit) True


-- |Check if alphabet contains lower letters
isAlphabetValid :: Alphabet -> Bool
isAlphabetValid = foldr (\ x -> (&&) (x `elem` ['a' .. 'z'])) True


-- |Check if start state is digit and is in set of states
isStartStateValid :: State -> [State] -> Bool
isStartStateValid s states = all isDigit s && s `elem` states


-- |Check if accept state exists in set of states
checkAcceptStates :: [State] -> [State] -> Bool
checkAcceptStates acceptStates states = foldr (\ state -> (&&) (state `elem` states)) True acceptStates


-- |Check if all transitions are valid
checkTransitions :: [Transition] -> [State] -> Alphabet -> Bool
checkTransitions transitions states alphabet
  = foldr (\ transition -> (&&) (isTransitionValid transition states alphabet)) True transitions


-- |Check if transition is valid:
-- |if source and destination state exist in set of states
-- |and if alphabet contains input symbol
isTransitionValid :: Transition -> [State] -> Alphabet -> Bool
isTransitionValid transition states alphabet =
  src transition `elem` states && isInfixOf (symbol transition) alphabet &&
    not (null (symbol transition)) && dst transition `elem` states


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
isDFAValid dfa
  | states dfa == [""] || null (states dfa) =
      "Error: Empty set of states"
  | acceptStates dfa == [""] || null (acceptStates dfa) =
      "Error: Empty set of accept states"
  | alphabet dfa == "" =
      "Error: Empty alphabet"
  | not $ isSetOfStatesValid (states dfa) =
      "Error: Invalid set of states"
  | not $ isAlphabetValid (alphabet dfa) =
      "Error: Invalid alphabet"
  | not $ isStartStateValid (startState dfa) (states dfa) =
      "Error: Invalid initial state"
  | not $ checkAcceptStates (acceptStates dfa) (states dfa) =
      "Error: Invalid accept states"
  | not $ hasUniqueState (states dfa) =
      "Error: Duplicates in set of states"
  | not $ hasUniqueState (acceptStates dfa) =
      "Error: Duplicates in set of accept states"
  | not $ checkTransitions (transitions dfa) (states dfa) (alphabet dfa) =
      "Error: Invalid transitions"
  | not $ hasUniqueTransitions (transitions dfa) =
      "Error: Duplicates in set of transitions"
  | otherwise = ""


-- |Get transition where source state is "state"
getTransitionsOfState :: State -> [Transition] -> [Transition]
getTransitionsOfState state
    = filter (\transition -> state == src transition)
