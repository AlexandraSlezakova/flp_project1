import System.Environment
import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Monad

import Parse

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
