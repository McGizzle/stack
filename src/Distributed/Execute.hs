{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Distributed.Execute where

import           Control.Concurrent                                 (threadDelay)
import           Control.Monad                                      (when)
import           Debug.Trace
import           Stack.Prelude
import           Stack.Types.Build
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

import           Distributed.Run

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
                work :: SendTask -> Process ()
                work stask = do
                  plog "Work received! "
                  liftIO $ runTask stask
                  send manager "I got the Work!"
                  run manager
                end () = do
                  plog "Bye"
                  send manager "Done"
                  return ()

remotable['worker]

rtable :: RemoteTable
rtable = Distributed.Execute.__remoteTable initRemoteTable

runDistributed task = do
        print "Distribution code in action"
        startManager "127.0.0.1" "5600" task

startWorker :: String -> String -> IO ()
startWorker host port = do
        print "Loading Worker"
        backend <- initializeBackend host port rtable
        startSlave backend

startManager :: String -> String -> Task -> IO ()
startManager host port task = do
        print "Loading Manager"
        loop
        return ()
         where
           loop = do
                   backend <- initializeBackend host port rtable
                   startMaster backend $ \ nids -> do
                        liftIO $ print "Searching for worker..."
                        when (null nids) $ liftIO $ print "No lads about :( ffs"
                        liftIO $ print nids
                        me <- getSelfPid
                        forM_ nids $ \ nid -> spawn nid $ $(mkClosure 'worker) me
                        pid <- expect
                        let stask = convertTask task
                        send pid stask
                        resp :: String <- expect
                        liftIO $ putStrLn resp
                        _ :: String <- expect
                        terminateAllSlaves backend

----------------------------------------------------------------------------------


