module DKA(
  DKA(..),
  Transition(..)
  -- alphabet(),
  -- start_state(),
  -- accept_states(),
  -- transitions()
) where

import Data.List.Split

data Transition = Transition {
  src   :: String,
  dst   :: String,
  input :: String
}

data DKA = DKA {
  states      :: [[String]],
  alphabet    :: [String],
  transitions :: [Transition]
}
