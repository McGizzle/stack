{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Concurrent execution with dependencies. Types currently hard-coded for needs
-- of stack, but could be generalized easily.
module Distributed.Execute
    (
     runActions
    ) where

import           Distributed.Build.Types
import           System.IO

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM        (retry)
import           Data.List                     (sortBy)
import qualified Data.Set                      as Set
import           Stack.Prelude
import           Stack.Types.PackageIdentifier

import           Debug.Trace

--------------------------------------
runActions :: Int -- ^ threads
           -> Bool -- ^ keep going after one task has failed
           -> [Action]
           -> MVar ()
           -> (TVar Int -> TVar (Set ActionId) -> IO ()) -- ^ progress updated
           -> IO [SomeException]
runActions threads keepGoing actions0 lock withProgress = do
    es <- ExecuteState
        <$> newTVarIO (sortActions actions0)
        <*> newTVarIO []
        <*> newTVarIO Set.empty
        <*> newTVarIO 0
        <*> pure keepGoing
    _ <- async $ withProgress (esCompleted es) (esInAction es)
    runActions' es lock
    readTVarIO $ esExceptions es

-- | Sort actions such that those that can't be run concurrently are at
-- the end.
sortActions :: [Action] -> [Action]
sortActions = sortBy (compareConcurrency `on` actionConcurrency)
  where
    -- NOTE: Could derive Ord. However, I like to make this explicit so
    -- that changes to the datatype must consider how it's affecting
    -- this.
    compareConcurrency ConcurrencyAllowed ConcurrencyDisallowed = LT
    compareConcurrency ConcurrencyDisallowed ConcurrencyAllowed = GT
    compareConcurrency _ _                                      = EQ

runActions' :: ExecuteState -> MVar () -> IO ()
runActions' ExecuteState {..} lock = do
  loop
  where
    breakOnErrs inner = do
        errs <- atomically $ readTVar esExceptions
        if null errs || esKeepGoing
            then inner
            else return ()
    withActions :: ([Action] -> IO ()) -> IO ()
    withActions inner = do
        as <- atomically $ readTVar esActions
        if null as
            then return ()
            else inner as
    loop = breakOnErrs $ withActions $ \as ->
      -- break (> 3) [1,2,3,4,5] == ([1,2,3],[4,5])
        case break (Set.null . actionDeps) as of
            (_, []) -> do
                inAction <- atomically $ readTVar esInAction --trace ("n\nempty case\n") $
                if Set.null inAction
                    then do
                        unless esKeepGoing $
                              atomically $ modifyTVar esExceptions (toException InconsistentDependencies:)
                        return ()
                    else loop
            (xs, action:ys) -> do
                inAction <- atomically $ readTVar esInAction
                case actionConcurrency action of
                  ConcurrencyAllowed    -> return ()
                  ConcurrencyDisallowed -> unless (Set.null inAction) loop
                let as' = xs ++ ys
                    remaining = Set.union
                        (Set.fromList $ map actionId as')
                        inAction
                atomically $ writeTVar esActions as'
                atomically $ modifyTVar esInAction (Set.insert $ actionId action)
                mask' as' remaining

               where
                       mask' :: [Action] -> Set ActionId -> IO ()
                       mask' as' remaining = mask $ \restore -> do
                        let context = ActionContext remaining (downstreamActions (actionId action) as') (actionConcurrency action)
                        --eres <- try $ restore $ actionDo action context
                        eres <- runActionDist count (actionInfo action) context
                        atomically $ do
                          modifyTVar esInAction (Set.delete $ actionId action)
                          modifyTVar esCompleted (+1)
                          case eres of
                              Left err -> modifyTVar esExceptions (err:)
                              Right () ->
                                  let dropDep a = a { actionDeps = Set.delete (actionId action) $ actionDeps a }
                                   in modifyTVar esActions $ map dropDep
                        restore $ loop

downstreamActions :: ActionId -> [Action] -> [Action]
downstreamActions aid = filter (\a -> aid `Set.member` actionDeps a)

