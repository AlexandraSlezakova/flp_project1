{-|
Project     : VUT FIT FLP Project 1 - DKA-2-MKA
Author      : Alexandra Slezakova (xsleza20)
Year        : 2021
File        : dka-2-mka.hs
-}

import System.Environment
import System.Directory
import Data.Char
import Control.Monad

import Parser
import MDFAModule
import DFAModule


-- |Read user input or file content
getContent :: [String] -> IO String
getContent args =  if length args == 1 then getContents else readFile $ head $ tail args


main :: IO ()
main = do
    args <- getArgs
    if length args > 2 || length args < 1
      then
        error "Error: Invalid input"
      else do
        content <- getContent args
        let dfa = parseContent $ lines content

        let err = isDFAValid dfa
        when (err /= "") $ error err

        case head args of
          "-i" -> printDFA dfa
          "-t" -> minimizeDFA dfa
          _ -> error "Error: Unknown option"
