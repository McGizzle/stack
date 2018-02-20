{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Control.Concurrent.Distributed where

import           Stack.Prelude
import System.IO
import           Control.Concurrent                                 (threadDelay)
import           Debug.Trace
-- CLOUD HASKELL
import           Control.Distributed.Process                        hiding
                                                                     (mask)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node
import           Network.Transport.TCP                              (createTransport,
                                                                     defaultTCPParameters)

import           Control.Concurrent.Types
import Data.Binary

data FakeException = FakeException
        deriving(Show)
instance Exception FakeException


---- CLOUD HASKELL -------------------------------------------------------------
plog :: String -> Process ()
plog msg = say $ "--> " ++ msg

worker :: ProcessId -> Process ()
worker manager = do
        me <- getSelfPid
        plog $ "Started Node: " ++ show me
        send manager me
        run manager
        where
            run manager = receiveWait[match end, match work]
              where 
                work :: String -> Process ()
                work ad = do
                  plog "Functions received! "
                  send manager "I got the functions!"
                  run manager
                end () = do
                  plog "Bye"
                  send manager "Done"
                  return ()

remotable['worker]

rtable :: RemoteTable
rtable = Control.Concurrent.Distributed.__remoteTable initRemoteTable

runActionDist :: Action -> ActionContext -> IO ()
runActionDist action context = do
        print "Distribution code in action"
        startManager "127.0.0.1" "5600" action context

startWorker :: String -> String -> IO ()
startWorker host port = do
        print "Loading Worker" 
        backend <- initializeBackend host port rtable 
        startSlave backend

startManager :: String -> String -> Action -> ActionContext -> IO ()
startManager host port action context = do
        print "Loading Manager"
        backend <- initializeBackend host port rtable
        startMaster backend $ \ nids -> do
                me <- getSelfPid
                let f = actionDo action
                forM_ nids $ \ nid -> spawn nid $ $(mkClosure 'worker) me
                pid <- expect
                send pid (encode $ actionDo action)
                resp <- expect
                liftIO . putStrLn $ "resp: " ++ resp
                send pid ()
                kill :: String <- expect 
                terminateAllSlaves backend        
        return ()

----------------------------------------------------------------------------------


