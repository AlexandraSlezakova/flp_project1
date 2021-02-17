import System.Environment
import System.Directory
import Data.Char
import Control.Monad

import Parser
import FAModule

-------------------------------------
-- Read user input or content of file
-------------------------------------
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
        let fa = parseContent $ lines content

        when (not $ isFAValid fa) $ error "Error: Invalid input"

        case (head args) of
          "-i" -> printFA fa
          otherwise -> error "Error: Unknown option"
