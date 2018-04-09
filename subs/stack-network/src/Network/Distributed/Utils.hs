{-# LANGUAGE OverloadedStrings #-}

module Network.Distributed.Utils where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Configurator         as C
import           Data.List                 (intersect)
import           Network.Distributed.Types
import           System.Console.ANSI
import           System.Exit               (ExitCode (..))
import           System.IO                 (BufferMode (..), hGetContents,
                                            hSetBuffering)
import           System.Process

-- CONFIG ==========================================================================
parseNetConfig :: IO NetworkConfig
parseNetConfig = do
    cfg <- C.load [C.Required "network.config"]
    NetworkConfig <$> C.require cfg "net.host" <*> C.require cfg "net.port"

-- LOGGING ==========================================================================
log :: MonadIO m => String -> m ()
log = log' [[SetColor Foreground Vivid Black]]

logSucc :: MonadIO m => String -> m ()
logSucc = log' [[SetColor Foreground Vivid Green]]

logWarn :: MonadIO m => String -> m ()
logWarn = log' [[SetColor Foreground Dull Red]]

log' :: MonadIO m => [[SGR]] -> String -> m ()
log' styles msg =
    liftIO $ do
        mapM_ setSGR styles
        putStrLn msg
        setSGR [Reset]

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

-- FIND BEST MATCH ==================================================================
getBestPid :: [ProcessDeps] -> [String] -> (Maybe Node, Int) -> Maybe Node
getBestPid [] _ best = fst best
getBestPid ((curDeps, curPid):xs) cmpDeps (pid, best)
    | curLen > best = recurse (Just curPid, curLen)
    | otherwise = recurse (pid, best)
  where
    curLen = length (curDeps `intersect` cmpDeps)
    recurse n = getBestPid xs cmpDeps n
