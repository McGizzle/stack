{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment                                 (getArgs)

import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node                   as PN

import           Distributed

main :: IO ()
main = do
    getArgs >>= \case
        ["build", host, port] -> do
            putStrLn "Build invoked"
            backend <- initializeBackend host port PN.initRemoteTable
            node <- newLocalNode backend
            PN.runProcess node (runBuild backend)
        ["sit", host, port] -> do
            putStrLn "Node joining network..."
            node <-
                newLocalNode =<< initializeBackend host port PN.initRemoteTable
            PN.runProcess node joinNetwork
        _ -> putStrLn "Bar args, Maysh."
