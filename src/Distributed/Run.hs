{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Distributed.Run where

import           Control.Concurrent.Execute
import           Control.Concurrent.STM           (check)
import           Crypto.Hash
import           Data.Attoparsec.Text             hiding (try)
import           Data.Binary
import qualified Data.ByteArray                   as Mem (convert)
import qualified Data.ByteString                  as S
import qualified Data.ByteString.Base64.URL       as B64URL
import           Data.Char                        (isSpace)
import           Data.Conduit
import qualified Data.Conduit.Binary              as CB
import qualified Data.Conduit.List                as CL
import           Data.Conduit.Process.Typed       (ExitCodeException (..),
                                                   createPipe, createSource,
                                                   getStderr, getStdin,
                                                   getStdout, runProcess_,
                                                   setStderr, setStdin,
                                                   setStdout, useHandleOpen,
                                                   waitExitCode)
import qualified Data.Conduit.Text                as CT
import           Data.FileEmbed                   (embedFile,
                                                   makeRelativeToProject)
import           Data.IORef.RunOnce               (runOnce)
import           Data.List                        hiding (any)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import qualified Data.Map.Strict                  as M
import qualified Data.Map.Strict                  as Map
import           Data.Set                         as Set
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Time.Clock                  (getCurrentTime)
import           Data.Tuple
import           Distributed.MainHelpers
import qualified Distribution.PackageDescription  as C
import qualified Distribution.Simple.Build.Macros as C
import           Distribution.System              (OS (Windows),
                                                   Platform (Platform))
import qualified Distribution.Text                as C
import           Path
import           Path.CheckInstall
import           Path.Extra                       (rejectMissingFile,
                                                   toFilePathNoTrailingSep)
import           Path.IO                          hiding (findExecutable,
                                                   makeAbsolute,
                                                   withSystemTempDir)
import           RIO.Process
import           Stack.Build
import           Stack.Build.Cache
import           Stack.Build.Execute
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Config
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Coverage
import           Stack.Fetch                      as Fetch
import           Stack.GhcPkg
import           Stack.Options.GlobalParser       (globalOptsFromMonoid)
import           Stack.Package
import           Stack.PackageDump
import           Stack.Prelude
import           Stack.PrettyPrint
import           Stack.Runners                    (loadConfigWithOpts)
import           Stack.Setup                      (setupEnv)
import           Stack.Types.Build
import           Stack.Types.BuildPlan            (LoadedSnapshot (..))
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Runner
import           Stack.Types.Version
import           System.Directory                 as D
import qualified System.Directory                 as D
import           System.Environment               as E
import           System.Environment               (getExecutablePath)
import           System.Exit                      (ExitCode (..))
import qualified System.FilePath                  as FP
import           System.IO
import           System.PosixCompat.Files         (createLink)
--------------
import           Debug.Trace

data SendTask = SendTask
  { staskProvides        :: !PackageIdentifier
  , staskType            :: TaskType
  , staskPresent         :: !(Map PackageIdentifier GhcPkgId)
  , staskAllInOne        :: !Bool
  , staskCachePkgSrc     :: !CachePkgSrc
  , staskAnyMissing      :: !Bool
  , staskBuildTypeConfig :: !Bool
  }
    deriving (Show , Generic, Typeable)

instance Binary SendTask

convertTask :: Task -> SendTask
convertTask Task{..} = SendTask
  { staskProvides = taskProvides
  , staskType = taskType
  , staskPresent = taskPresent
  , staskAllInOne = taskAllInOne
  , staskCachePkgSrc = taskCachePkgSrc
  , staskAnyMissing = taskAnyMissing
  , staskBuildTypeConfig = taskBuildTypeConfig
  }

convertSendTask :: SendTask -> EnvConfig -> BaseConfigOpts -> Package -> Task
convertSendTask SendTask{..} envConfig baseConfigOpts package = Task
  { taskProvides = staskProvides
  , taskType = staskType
  , taskPresent = staskPresent
  , taskAllInOne = staskAllInOne
  , taskCachePkgSrc = staskCachePkgSrc
  , taskAnyMissing = staskAnyMissing
  , taskBuildTypeConfig = staskBuildTypeConfig
  , taskConfigOpts = mkConfigOpts
  }
          where mkConfigOpts = TaskConfigOpts Set.empty $ \missing' ->
                    let allDeps = Map.union Map.empty missing'
                     in configureOpts
                            envConfig
                            baseConfigOpts
                            allDeps
                            True -- local
                            Local
                            package

runTask :: SendTask -> IO ()
runTask task = do
        cfg <- getEnvConfig
        runRIO cfg (buildTask task cfg)
        putStrLn "Done task"
        return ()

buildTask :: HasEnvConfig env => SendTask -> EnvConfig -> RIO env ()
buildTask stask envCfg = do
        bopts <- view buildOptsL
        liftIO $ putStrLn "buildTask"
        let profiling = boptsLibProfile bopts || boptsExeProfile bopts
        let symbols = not (boptsLibStrip bopts || boptsExeStrip bopts)
        let boptsCli = defaultBuildOptsCLI
        baseConfigOpts <- mkBaseConfigOpts boptsCli
        let ac = ActionContext mempty mempty ConcurrencyAllowed
        (targets, ls, locals, extraToBuild, sourceMap) <- loadSourceMapFull NeedTargets boptsCli
        liftIO $ print locals
        let task = convertSendTask stask envCfg baseConfigOpts $ lpPackage $ head locals
        liftIO $ traceIO "Task converted!"
        let getInstalledOpts =
                     GetInstalledOpts
                         { getInstalledProfiling = profiling
                         , getInstalledHaddock   = shouldHaddockDeps bopts
                         , getInstalledSymbols   = symbols }
        liftIO $ print getInstalledOpts
        (installedMap, globalPkgs, snapshotPkgs, localPkgs) <- getInstalled getInstalledOpts sourceMap
        liftIO $ traceIO "got installedmap and package dumps"
        withExecuteEnv bopts boptsCli baseConfigOpts locals globalPkgs snapshotPkgs localPkgs $ \ ee -> do
                liftIO $ traceIO "Got ExecuteEnv!"
                (allDepsMap, cache) <- getConfigCache ee task installedMap True True
                liftIO $ traceIO "Got Config Cache!"
                withSingleContext ac ee task (Just allDepsMap) Nothing $ \package cabalfp pkgDir cabal announce _console _mlogFile -> do
                        liftIO $ putStrLn "About to run cabal"
                        config <- view configL
                        cabal stripTHLoading ["build exe:testbuild"]
                        return ()
                                where stripTHLoading = ExcludeTHLoading

getBuildConfig :: LoadConfig -> IO BuildConfig
getBuildConfig lc = lcLoadBuildConfig lc Nothing

getEnvConfig :: IO EnvConfig
getEnvConfig = do
    currentDir <- D.getCurrentDirectory
    progName <- E.getProgName
    (mon,_) <- withArgs ["build","-v"] (commandLineHandler currentDir progName False)
    let opts = globalOptsFromMonoid False mon
    buildConfig <- loadConfigWithOpts opts getBuildConfig
    envConfig <- runRIO buildConfig (setupEnv Nothing)
    return envConfig

