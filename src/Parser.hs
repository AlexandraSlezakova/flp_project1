module Parser(
  parseContent
) where

import FAModule
import Data.List.Split    -- splitOn
import System.IO

-----------------------------------
-- Parse content from file or stdin
-----------------------------------
parseContent :: [String] -> FAStruct
parseContent content = FAStruct {
  states        = splitOn "," $ content !! 0,     -- the states are on the first line
  alphabet      = content !! 1,                   -- the alphabet is on the second line
  startState    = content !! 2,                   -- the start state is on the third line
  acceptStates  = splitOn "," $ content !! 3,     -- the accept states are on the fourth line
  transitions   = parseTransitions content        -- the rest of content contains transitions
}


--------------------------------------------------------
-- Parse lines with rules and create list of transitions
--------------------------------------------------------
parseTransitions :: [String] -> [Transition]
parseTransitions content = do
  let transitions = snd $ splitAt 4 content
  -- parse set of transitions
  [ parseTransition transition | transition <- transitions ]


--------------------------------------------------------
-- Parse transition rule and create Transition structure
--------------------------------------------------------
parseTransition :: String -> Transition
parseTransition transition = do
  let t = splitOn "," transition
  -- source state
  let src = t!!0

  -- input symbol
  let symbol = t!!1

  -- destination state
  let dst = t!!2

  Transition src symbol dst
