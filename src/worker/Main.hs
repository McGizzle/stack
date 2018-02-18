module Main where

import Control.Concurrent.Distributed
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
          ["worker",host,port] -> startWorker host port
          ["manager",host,port] -> startManager host port
