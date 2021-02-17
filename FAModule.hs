module FAModule(
  FAStruct(..),
  Transition(..),
  printFA,
  isFAValid
) where

import Data.Function
import Data.Char
import Data.List


data Transition = Transition {
  src     :: String,
  symbol  :: String,
  dst     :: String
}


data FAStruct = FAStruct {
  states        :: [String],
  alphabet      :: String,
  startState    :: String,
  acceptStates  :: [String],
  transitions   :: [Transition]
}

------------------
-- Print automaton
------------------
printFA :: FAStruct -> IO()
printFA fa = do
  -- states
  putStrLn $ intercalate "," $ states fa
  -- alphabet
  putStrLn $ alphabet fa
  -- start state
  putStrLn $ startState fa
  -- accept states
  putStrLn $ intercalate "," $ acceptStates fa
  -- transitions
  printTransitions $ transitions fa

------------------------------
-- Print transitions to stdout
------------------------------
printTransitions :: [Transition] -> IO()
printTransitions [] = putStrLn ""
printTransitions (transition:transitions) = do
   putStrLn $ src transition ++ "," ++ symbol transition ++ "," ++  dst transition
   printTransitions transitions

--------------------------------------
-- Check if state contains only digits
--------------------------------------
isSetOfStatesValid :: [String] -> Bool
isSetOfStatesValid [] = True
isSetOfStatesValid (x:xs) = if all isDigit x then isSetOfStatesValid xs else False


-------------------------------------------
-- Check if alphabet contains lower letters
-------------------------------------------
isAlphabetValid :: [Char] -> Bool
isAlphabetValid [] = True
isAlphabetValid (x:xs) = if isLower x then isAlphabetValid xs else False


--------------------------------
-- Check if start state is valid
--------------------------------
isStartStateValid :: String -> [String] -> Bool
isStartStateValid s states = all isDigit s && s `elem` states


------------------------------------------------
-- Check if accept state exists in set of states
------------------------------------------------
checkAcceptStates :: [String] -> [String] -> Bool
checkAcceptStates [] states = True
checkAcceptStates (state:acceptStates) states  =
  if state `elem` states then checkAcceptStates acceptStates states else False


-------------------------------------
-- Check if all transitions are valid
-------------------------------------
checkTransitions :: [Transition] -> [String] -> String -> Bool
checkTransitions [] states alphabet = True
checkTransitions (transition:transitions) states alphabet =
  if isTransitionValid transition states alphabet
    then checkTransitions transitions states alphabet
    else False


----------------------------------------------------------
-- Check if transition is valid:
-- if source and destination state exist in set of states
-- and if alphabet contains input symbol
----------------------------------------------------------
isTransitionValid :: Transition -> [String] -> String -> Bool
isTransitionValid transition states alphabet = do
  if (src transition) `elem` states
    && isInfixOf (symbol transition) alphabet
    && (dst transition) `elem` states
    then True
    else False


------------------------------
-- Check if automaton is valid
------------------------------
isFAValid :: FAStruct -> Bool
isFAValid fa =
  isSetOfStatesValid (states fa)
  && isAlphabetValid (alphabet fa)
  && isStartStateValid (startState fa) (states fa)
  && checkAcceptStates (acceptStates fa) (states fa)
  && checkTransitions (transitions fa) (states fa) (alphabet fa)
