{-|
Project     : VUT FIT FLP Project 1 - DKA-2-MKA
Author      : Alexandra Slezakova (xsleza20)
Year        : 2021
Module      : Parser
Description : An input parser
-}

module Parser (
  parseContent
) where

import DFAModule
import Data.List.Split    -- splitOn
import System.IO
import Data.List


-- |Parse content from file or stdin
parseContent :: [String] -> DFAStruct
parseContent content = DFAStruct {
  -- the states are on the first line
  states        = if not (null content) then splitOn "," $ head content else [],
  -- the alphabet is on the second line
  alphabet      = if length content >= 2 then sort (content !! 1) else "",
  -- the start state is on the third line
  startState    = if length content >= 3 then content !! 2 else "",
  -- the accept states are on the fourth line
  acceptStates  = if length content >= 4 then splitOn "," $ content !! 3 else [],
  -- the rest of content contains transitions
  transitions   = sort $ parseTransitions content
}


-- |Parse lines with rules and create list of transitions
parseTransitions :: [String] -> [Transition]
parseTransitions content = do
  let transitions = snd $ splitAt 4 content
  -- parse set of transitions
  [ parseTransition transition | transition <- transitions ]


-- |Parse transition rule and create Transition structure
parseTransition :: String -> Transition
parseTransition transition =
  Transition {
    src     = head t,  -- source state
    symbol  = t!!1,    -- input symbol
    dst     = t!!2     -- destination state
  }
  where
    t = splitOn "," transition
