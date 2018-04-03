module DistributedSpec where

import           Control.Concurrent.Async
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node                   as PN
import           Distributed.Main
import           System.Directory
import           Test.Hspec

spec :: Spec
spec = do
    temp1 <- createTempDirectory "temp1" "root"
    temp2 <- createTempDirectory "temp2" "root"
    createDirectory "root"
    writeFile temp1 "This is a test."
    runIO runTests
    describe "Sending/Receiving a file" $ do
        it "Compares directory structure" $ do
            temp1Conts <- getDirectoryContents temp1
            temp2Conts <- getDirectoryContents temp2
            temp1Conts `shouldBe` temp2Conts

runTests :: IO ()
runTests = do
    node1 <-
        newLocalNode =<< initializeBackend "127.0.0.1" "5000" PN.initRemoteTable
    node2 <-
        newLocalNode =<< initializeBackend "127.0.0.1" "5001" PN.initRemoteTable
    done <-
        concurrently (PN.runProcess node1 send) (PN.runProcess node2 receive)
    waitBoth done

send :: Process ()
send = do
    liftIO $ setWorkingDrecotry temp1
    joinNetwork

receive :: Process ()
receive = do
    liftIO $ setWorkingDrecotry temp1
    runBuild
