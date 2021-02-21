import System.Environment
import System.Directory
import Data.Char
import Control.Monad

import Parser
import MDFAModule
import DFAModule


-- |Read user input or file content
getContent :: [String] -> IO String
getContent args = do
  if length args == 1
    then getContents
    else readFile $ head $ tail args


main :: IO ()
main = do
    args <- getArgs
    if length args > 2 || length args < 1
      then
        error "Error: Invalid input"
      else do
        content <- getContent args
        let dfa = parseContent $ lines content

        when (not $ isDFAValid dfa) $ error "Error: Invalid input"

        case (head args) of
          "-i" -> printDFA dfa
          "-t" -> minimizeDFA dfa
          otherwise -> error "Error: Unknown option"
