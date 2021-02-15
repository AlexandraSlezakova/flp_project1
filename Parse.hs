module Parse(
  parseArgs,
  parseContent
) where

import DKA
import Data.Char
import Data.List.Split    -- splitOn
import Data.List
import Control.Monad      -- when

parseArgs :: [String] -> IO ()
parseArgs args =
  if length args > 2 || length args < 1
    then
      error "Error: Invalid input"
    else do
      case args of
          ["-i", filename] -> do
            content <- readFile filename
            let linesOfFile = lines content
            let states = tail linesOfFile
            let h = head states
            let t = tail states
            parseContent linesOfFile
            -- let s = DKA.get_states $ states
            putStrLn "tu"

          ["-i"] -> putStrLn ("Top of the morning to you, ")
          ["-t", filename] -> putStrLn ("Top of the morning to you, ")
          ["-t"]  -> putStrLn ("Top of the morning to you, ")
          otherwise -> error "Error: Unknown option"


-- Parse content from file or stdin
parseContent :: [String] -> IO()
parseContent content = do
  -- set of states
  let states = splitOn "," $ head content
  when (not $ isValidSetOfStates states) $ error "Error: Invalid set of states"

  -- content without states
  let without_states = tail content

  -- alphabet
  let alphabet = head without_states
  when (not $ isValidAlphabet alphabet) $ error "Error: Invalid alphabet"

  -- content without alphabet
  let without_alphabet = tail without_states

  -- start state
  let startState = head without_alphabet
  when (not $ isValidStartState startState) $ error "Error: Invalid start state"

  -- content without accept states
  let without_accept_states = tail without_alphabet

  -- accept states
  let accept_states = splitOn ", " $ head without_accept_states
  when (not $ isValidSetOfStates accept_states) $ error "Error: Invalid set of accept states"

  -- transitions
  let transitions = tail without_accept_states
  let t = head transitions
  let m = getTransition states alphabet t
  putStrLn $ show "tu"


-- parseTransitions :: [[String]] -> [DKA.Transition]
-- parseTransitions (transition:transitions) = do
--     if length transition == 3
--       then getTransition transition
--       else error "Error: Invalid transition"



getTransition :: [String] -> String -> String -> Either Bool Transition
getTransition states alphabet transition = do
  let t = splitOn "," transition
  -- source state
  let src = t!!0
  when (not $ src `elem` states) $ Left False

  -- input symbol
  let symbol = t!!1
  when (not $ isInfixOf symbol alphabet) $ Left False

  -- destination state
  let dst = t!!2
  when (not $ dst `elem` states) $ Left False

  let dkaTransition = Transition src symbol dst
  Right dkaTransition



isValidSetOfStates :: [String] -> Bool
isValidSetOfStates [] = True
isValidSetOfStates (x:xs) = if all isDigit x then isValidSetOfStates xs else False


isValidAlphabet :: [Char] -> Bool
isValidAlphabet [] = True
isValidAlphabet (x:xs) = if isLower x then isValidAlphabet xs else False


isValidStartState :: String -> Bool
isValidStartState s = all isDigit s
