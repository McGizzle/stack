{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distributed where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch                                (bracket)
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     log)

import           Control.Distributed.Process                        hiding
                                                                     (bracket)
import           Control.Distributed.Process.Backend.SimpleLocalnet

import           Control.Concurrent.Async                           (async)

import           Data.Binary                                        hiding
                                                                     (decode,
                                                                     encode)
import           Data.ByteString                                    (ByteString)
import           Data.DirStream
import           Data.List                                          (find,
                                                                     intersect)
import           Data.Maybe                                         (catMaybes)
import           Data.Text                                          (Text)
import           Data.Typeable
import           GHC.Generics

import           Filesystem
import           Filesystem.Path                                    (FilePath,
                                                                     directory)
import           Filesystem.Path.CurrentOS                          (decode,
                                                                     encode)
import           System.Console.ANSI
import           System.Directory
import           System.Exit                                        (ExitCode (..))
import           System.FilePath                                    (takeExtension)
import           System.IO                                          (BufferMode (..),
                                                                     hGetContents,
                                                                     hSetBuffering)
import           System.Process

import           Pipes
import qualified Pipes.Prelude                                      as P
import           Pipes.Safe                                         (MonadMask,
                                                                     runSafeT)

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse

-- Data Types ==========================================================
type Node = ProcessId

type Network = [Node]

type ProcessDeps = (Deps, Node)

type Deps = [String]

data Request
    = Ping ProcessId
    | Transfer (SendPort TransferData)
    | Terminate
    deriving (Generic, Typeable)

instance Binary Request

data Response
    = PD ProcessDeps
    | TransferData FileInfo
    deriving (Generic, Typeable)

instance Binary Response

type FileInfo = (Text, ByteString)

data TransferData
    = TransferInProg FileInfo
    | TransferDone
    deriving (Generic, Typeable)

instance Binary TransferData

-- LOGGING ==============================================================
log :: MonadIO m => String -> m ()
log msg =
    liftIO $ do
        setSGR [SetColor Foreground Vivid Black]
        putStrLn msg
        setSGR [Reset]

----------------------------------------------------------------------
-- DISTRIBUTED NODES ============================================================
runBuild :: Backend -> Process ()
runBuild backend = do
    me <- getSelfPid
    log "Searching the Network..."
    pids <- findPids backend
    log $ "Found Nodes: " ++ show pids
    deps <- gatherDeps me pids
    myDeps <- listDeps
    log "Finding most compatable node..."
    let bestPid = getBestPid deps myDeps Nothing
    case bestPid of
        Nothing -> do
            log "No Nodes share dependencies, aborting."
        Just (_, n) -> do
            log $ "Node: " ++ show n ++ " Is the best match."
            (sPort, rPort) <- newChan
            send n (Transfer sPort)
            log "Working..."
            receiveF rPort
            log "Transmission complete. All files received. Ready to build."
            mapM_ (flip send Terminate) pids

joinNetwork :: Process ()
joinNetwork = do
    me <- getSelfPid
    register "nodeS" me
    loop me
  where
    loop me = do
        receiveWait [match $ \req -> receiveReq me req]
        loop me
    receiveReq :: ProcessId -> Request -> Process ()
    receiveReq me (Ping pid) = do
        log "Received a Ping!"
        deps <- listDeps
        send pid (PD (deps, me))
        loop me
    receiveReq me (Transfer sPort) = do
        log "Received request to transmit files... Beginning transmission"
        pipeFiles (sendChan sPort) *> sendChan sPort TransferDone
        loop me
    receiveReq _ Terminate = pure ()

gatherDeps :: ProcessId -> Network -> Process [ProcessDeps]
gatherDeps me pids = do
    mapM_ (flip send (Ping me)) pids
    replicateM (length pids) $ receiveWait [match $ \(PD pd) -> pure pd]

-- HELPER FUNCTION ==================================================================
findPids :: Backend -> Process [ProcessId]
findPids backend = do
    loop
  where
    loop = do
        nids <- liftIO $ findPeers backend 1000000
        pids <-
            bracket (mapM monitorNode nids) (mapM unmonitor) $ \_ -> do
                forM_ nids $ \n -> whereisRemoteAsync n "nodeS"
                catMaybes <$>
                    replicateM
                        (length nids)
                        (receiveWait
                             [match (\(WhereIsReply "nodeS" mPid) -> pure mPid)])
        if null pids
            then loop
            else pure pids

--------------------------------------------------------------------------------------
-- RECEIVE FILES ====================================================================
receiveF :: ReceivePort TransferData -> Process ()
receiveF rPort =
    work =<< receiveChan rPort --[match $ \(TransferData d) -> work d, match $ \() -> pure ()]
  where
    work :: TransferData -> Process ()
    work (TransferInProg i) = do
        _ <- liftIO $ async $ saveFile i
        receiveF rPort
    work TransferDone = pure ()

saveFile :: FileInfo -> IO ()
saveFile (path, file) = do
    createTree (directory path')
    Filesystem.writeFile path' file
  where
    path' = decode path

-- SEND FILES ======================================================================
pipeFiles :: (MonadMask m, MonadIO m) => (TransferData -> m ()) -> m ()
pipeFiles func = do
    runSafeT $
        runEffect $
        for
            (every (descendentOf "root/snapshots") >->
             P.filterM (liftIO . isFile) >->
             P.mapM (liftIO . packageFile))
            (lift . lift . func)

packageFile :: FilePath -> IO TransferData
packageFile file =
    TransferInProg <$>
    liftA2 (,) (pure (encode file)) (Filesystem.readFile file)

--------------------------------------------------------------------------------------
-- FIND BEST MATCH ==================================================================
getBestPid ::
       [ProcessDeps] -> [String] -> Maybe ProcessDeps -> Maybe ProcessDeps
getBestPid [] _ best = best
getBestPid (x:xs) myDeps best
    | length (fst x `intersect` myDeps) > maybe 0 length best =
        getBestPid xs myDeps (Just x)
    | otherwise = getBestPid xs myDeps best

-- List Deps ========================================================================
listDeps :: MonadIO m => m [String]
listDeps =
    liftIO $ do
        (_, Just hStdout, _, p) <-
            System.Process.createProcess
                (proc "stack" ["list-dependencies"])
                {std_out = CreatePipe, std_err = Inherit}
        hSetBuffering hStdout NoBuffering
        exit_code <- waitForProcess p
        case exit_code of
            ExitSuccess   -> lines <$> hGetContents hStdout
            ExitFailure _ -> pure []

-- PARSE CABAL =======================================================================
parseC :: IO [PackageName]
parseC = do
    mcbl <-
        find (\x -> takeExtension x == ".cabal") <$> getDirectoryContents "."
    case mcbl of
        Nothing -> pure []
        Just cbl -> do
            parsed <- parseGenericPackageDescription <$> Prelude.readFile cbl
            case parsed of
                ParseFailed _ -> pure []
                ParseOk _ d -> pure $ (\(Dependency x _) -> x) <$> extractDeps d

extractDeps :: GenericPackageDescription -> [Dependency]
extractDeps d = ldeps ++ edeps
  where
    ldeps =
        case condLibrary d of
            Nothing -> []
            Just c  -> condTreeConstraints c
    edeps = concatMap (condTreeConstraints . snd) $ condExecutables d
