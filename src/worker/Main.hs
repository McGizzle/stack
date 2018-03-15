module Main where

import           Distributed.Execute
import           System.Environment

main :: IO ()
main = do
        print "Starting worker executable"
        args <- getArgs
        case args of
          [host,port] -> startWorker host port
          _           -> putStrLn "Bad args"
