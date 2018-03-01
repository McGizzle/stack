{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Distributed.Build.Types where


import           Control.Exception
import           Data.Binary
import           GHC.Generics                     (Generic)

import           Control.Concurrent.STM           (check)
import           Crypto.Hash
import           Data.Attoparsec.Text             hiding (try)
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
import qualified Data.Map.Strict                  as M
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Time.Clock                  (getCurrentTime)
import           Data.Tuple
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
import           Stack.Build.Cache
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
import           Stack.Package
import           Stack.PackageDump
import           Stack.Prelude
import           Stack.PrettyPrint
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Runner
import           Stack.Types.Version
import qualified System.Directory                 as D
import           System.Environment               (getExecutablePath)
import           System.Exit                      (ExitCode (..))
import qualified System.FilePath                  as FP
import           System.IO                        (hPutStr, stderr, stdout)
import           System.PosixCompat.Files         (createLink)

import           Data.Binary

data BuildInfo = BuildInfo
    { installedMap :: InstalledMap
    , exeEnv       :: ExecuteEnv
    , task         :: Task
    , isFB         :: Bool
    , envConfig    :: EnvConfig
    }
  deriving (Generic,Typeable)

instance Binary BuildInfo

data ExecuteEnv = ExecuteEnv
    {
      eeBuildOpts        :: !BuildOpts
    , eeBuildOptsCLI     :: !BuildOptsCLI
    , eeBaseConfigOpts   :: !BaseConfigOpts
    , eeGhcPkgIds        :: !(Map PackageIdentifier Installed)
    , eeTempDir          :: !(Path Abs Dir)
    , eeSetupHs          :: !(Path Abs File)
    , eeSetupShimHs      :: !(Path Abs File)
    , eeSetupExe         :: !(Maybe (Path Abs File))
    , eeCabalPkgVer      :: !Version
    , eeTotalWanted      :: !Int
    , eeWanted           :: !(Set PackageName)
    , eeLocals           :: ![LocalPackage]
    , eeGlobalDB         :: !(Path Abs Dir)
    , eeGlobalDumpPkgs   :: !(Map GhcPkgId (DumpPackage () () ()))
    , eeSnapshotDumpPkgs :: !(Map GhcPkgId (DumpPackage () () ()))
    , eeLocalDumpPkgs    :: !(Map GhcPkgId (DumpPackage () () ()))
    , eeGetGhcPath       :: !(Path Abs File)
    , eeGetGhcjsPath     :: !(Path Abs File)
    , eeCustomBuilt      :: !(Set PackageName)
    } deriving (Generic,Typeable)
instance Binary ExecuteEnv

data ActionType
    = ATBuild
    | ATBuildFinal
    | ATRunTests
    | ATRunBenchmarks
    deriving (Show, Eq, Ord, Typeable, Generic)

instance Binary ActionType

data ActionId = ActionId !PackageIdentifier !ActionType
        deriving (Show, Eq, Ord, Generic)

data Action = Action
    { actionId          :: !ActionId
    , actionDeps        :: !(Set ActionId)
    , actionConcurrency :: !Concurrency
    , actionInfo        :: !BuildInfo
    }
    deriving(Generic)


data Concurrency = ConcurrencyAllowed | ConcurrencyDisallowed
    deriving (Eq,Show,Generic)
instance Binary Concurrency

data ActionContext = ActionContext
    { acRemaining   :: !(Set ActionId)
    , acDownstream  :: [Action]
    , acConcurrency :: !Concurrency
    }
    deriving(Generic,Typeable)

data ExecuteState = ExecuteState
    { esActions    :: TVar [Action]
    , esExceptions :: TVar [SomeException]
    , esInAction   :: TVar (Set ActionId)
    , esCompleted  :: TVar Int
    , esKeepGoing  :: Bool
    }

data ExecuteException
    = InconsistentDependencies
    deriving Typeable
instance Exception ExecuteException

instance Show ExecuteException where
    show InconsistentDependencies =
        "Inconsistent dependencies were discovered while executing your build plan. This should never happen, please report it as a bug to the stack team."


--------------------

deriving instance Generic GHCVariant
instance Binary GHCVariant
