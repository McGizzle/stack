{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch                                (bracket)
import           Prelude                                            hiding
                                                                     (FilePath,
                                                                     log)

import           Control.Distributed.Process                        hiding
                                                                     (bracket)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node                   as PN

import           Data.ByteString                                    (ByteString)
import           Data.DirStream
import           Data.List                                          (delete,
                                                                     find,
                                                                     intersect,
                                                                     take)
import           Data.Maybe                                         (catMaybes)
import           Data.Text                                          (Text)

import           Filesystem
import           Filesystem.Path                                    (FilePath,
                                                                     directory,
                                                                     filename)
import           Filesystem.Path.CurrentOS                          (decode,
                                                                     encode)
import           System.Console.ANSI
import           System.Directory
import           System.Environment                                 (getArgs)
import           System.FilePath                                    (takeExtension)
import           System.IO                                          (hFlush,
                                                                     hPutStr,
                                                                     hPutStrLn,
                                                                     stderr,
                                                                     stdin,
                                                                     stdout)

import           Pipes
import qualified Pipes.Prelude                                      as P
import           Pipes.Safe                                         (MonadMask,
                                                                     runSafeT)

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn

logProgress :: MonadIO m => String -> m ()
logProgress msg =
    liftIO $ do
        setCursorColumn 0
        clearLine
        setSGR [SetColor Foreground Vivid Black]
        putStr "-------> File Received: "
        setSGR [SetColor Foreground Vivid White]
        putStr msg
        setSGR [Reset]

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

runBuild :: Backend -> Process ()
runBuild backend = do
    me <- getSelfPid
    log "Searching the Network..."
    pids <- findPids backend
    log $ "Found Nodes: " ++ show pids
    mapM_ (flip send me) pids
    deps :: [ProcessDeps] <- replicateM (length pids) expect
    myDeps <- liftIO parseC
    log "Finding most compatable node..."
    let bestPid = getBestPid deps myDeps ([], head pids)
        rest = delete bestPid pids
    log $ "Node: " ++ show bestPid ++ " Is the best match."
    mapM_ (flip send False) rest
    send bestPid True
    log "About to receive files..."
    receiveF
    mapM_ (flip send ()) pids

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

joinNetwork :: Process ()
joinNetwork = do
    me <- getSelfPid
    register "nodeS" me
    loop me
  where
    loop me = do
        to <- expect
        deps <- liftIO parseC
        send to (deps, me)
        (go :: Bool) <- expect
        if go
            then do
                log "Received request to transmit files... About to begin send"
                pipeFiles (send to) *> send to ()
            else loop me

type FileInfo = (Text, ByteString)

receiveF :: Process ()
receiveF = receiveWait [match work, match $ \() -> pure ()]
  where
    work :: FileInfo -> Process ()
    work f = do
        liftIO $ saveFile f
        receiveF

saveFile :: FileInfo -> IO ()
saveFile (path, file) = do
    logProgress . show . filename $ path'
    createTree $ directory path'
    Filesystem.writeFile path' file
  where
    path' = decode path

pipeFiles :: (MonadMask m, MonadIO m) => (FileInfo -> m ()) -> m ()
pipeFiles func = do
    runSafeT $
        runEffect $
        for
            (every (descendentOf "root/snapshots") >->
             P.filterM (liftIO . isFile) >->
             P.mapM (liftIO . packageFile))
            (lift . lift . func)

packageFile :: FilePath -> IO FileInfo
packageFile file = liftA2 (,) (pure $ encode file) (Filesystem.readFile file)

type ProcessDeps = ([PackageName], ProcessId)

getBestPid :: [ProcessDeps] -> [PackageName] -> ProcessDeps -> ProcessId
getBestPid [] _ best = snd best
getBestPid (x:xs) myDeps best
    | length (fst x `intersect` myDeps) > (length $ fst best) =
        getBestPid xs myDeps x
    | otherwise = getBestPid xs myDeps best

parseC :: IO [PackageName]
parseC = do
    Just cbl <-
        find (\x -> takeExtension x == ".cabal") <$> getDirectoryContents "."
    parsed <- parsePackageDescription <$> Prelude.readFile cbl
    case parsed of
        ParseFailed _ -> pure []
        ParseOk _ d   -> pure $ (\(Dependency x _) -> x) <$> extractDeps d

extractDeps :: GenericPackageDescription -> [Dependency]
extractDeps d = ldeps ++ edeps
  where
    ldeps =
        case condLibrary d of
            Nothing -> []
            Just c  -> condTreeConstraints c
    edeps = concatMap (condTreeConstraints . snd) $ condExecutables d
