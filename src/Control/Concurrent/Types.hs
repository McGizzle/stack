{-# LANGUAGE DeriveGeneric #-}
module Control.Concurrent.Types where

import           Control.Concurrent.STM
import           Control.Exception
import           Data.Binary
import qualified Data.Set                      as Set
import           GHC.Generics                  (Generic)
import           Stack.Prelude
import           Stack.Types.Build
import           Stack.Types.PackageIdentifier

data ActionType
    = ATBuild
      -- ^ Action for building a package's library and executables. If
      -- 'taskAllInOne' is 'True', then this will also build benchmarks
      -- and tests. It is 'False' when then library's benchmarks or
      -- test-suites have cyclic dependencies.
    | ATBuildFinal
      -- ^ Task for building the package's benchmarks and test-suites.
      -- Requires that the library was already built.
    | ATRunTests
      -- ^ Task for running the package's test-suites.
    | ATRunBenchmarks
      -- ^ Task for running the package's benchmarks.
    deriving (Show, Eq, Ord, Typeable, Generic)

instance Binary ActionType

data ActionId = ActionId !PackageIdentifier !ActionType
        deriving (Show, Eq, Ord, Generic)

data Action = Action
    { actionId          :: !ActionId
    , actionDeps        :: !(Set ActionId)
    , actionDo          :: !(ActionContext -> IO ())
    , actionConcurrency :: !Concurrency
      , actionTask      :: !Task
    }
    deriving(Generic)

instance Show Action where
        show (Action a b c d e) = "actionId: " ++ show a ++ " actionDeps:" ++ show b ++ " Action Do!!!!!" ++ " Concurrency" ++ show d


data Concurrency = ConcurrencyAllowed | ConcurrencyDisallowed
    deriving (Eq,Show,Generic)
instance Binary Concurrency

data ActionContext = ActionContext
    { acRemaining   :: !(Set ActionId)
    -- ^ Does not include the current action
    , acDownstream  :: [Action]
    -- ^ Actions which depend on the current action
    , acConcurrency :: !Concurrency
    -- ^ Whether this action may be run concurrently with others
    }
    deriving(Show,Generic)

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



