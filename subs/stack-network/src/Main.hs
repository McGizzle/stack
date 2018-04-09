{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment  (getArgs)

import           Network.Distributed

main :: IO ()
main = do
    getArgs >>= \case
        ["build", host, port] -> do
            putStrLn "Build invoked"
            runRequestNode $ NetworkConfig host port
        ["sit", host, port] -> do
            putStrLn "Node joining network..."
            joinNetwork $ NetworkConfig host port
        _ -> putStrLn "Bar args, Maysh."
