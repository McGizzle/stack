{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Distributed.Distributed where

import           Distributed.Build.Execute

import           Control.Concurrent                                 (threadDelay)
import           Debug.Trace
import           Stack.Prelude
import           System.IO
-- CLOUD HASKELL
import           Control.Distributed.Process                        hiding
                                                                     (mask)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node
import           Network.Transport.TCP                              (createTransport,
                                                                     defaultTCPParameters)

import           Control.Concurrent.Types
import           Data.Binary

import           Distributed.Types

data FakeException = FakeException
        deriving(Show)
instance Exception FakeException


---- CLOUD HASKELL -------------------------------------------------------------
plog :: String -> Process ()
plog msg = say $ "--> " ++ msg

runActionDist :: Int -> BuildInfo -> ActionContext -> IO ()
runActionDist count action context = do
        print "Distribution code in action"
        startManager count "127.0.0.1" "5600" action context


worker :: ProcessId -> Process ()
worker manager = do
        me <- getSelfPid
        plog $ "Started Node: " ++ show me
        send manager me
        run manager
        where
            run manager = receiveWait[match end, match work]
              where
                work :: (BuildInfo,ActionContext) -> Process ()
                work (BuildInfo{..},ac) = do
                  plog "Data received! "
                  liftIO $ runRIO (singleBuild ac exeEnv task installedMap isFB) envConfig
                  send manager True
                  run manager
                end () = do
                  plog "Bye"
                  return ()

remotable['worker]

rtable :: RemoteTable
rtable = Control.Concurrent.Distributed.__remoteTable initRemoteTable

startWorker :: String -> String -> IO ()
startWorker host port = do
        print "Loading Worker"
        backend <- initializeBackend host port rtable
        startSlave backend

startManager :: String -> String -> BuildInfo -> ActionContext -> IO ()
startManager host port action context = do
        print "Loading Manager"
        backend <- initializeBackend host port rtable
        startMaster backend $ \ nids -> do
                me <- getSelfPid
                (pid:_) <- forM nids $ \ nid -> spawn nid $ $(mkClosure 'worker) me
                send pid (action,context)
                True <- expect
                liftIO $ putStrLn "Worker Done!"
                send pid ()
                terminateAllSlaves backend
        return ()

----------------------------------------------------------------------------------


