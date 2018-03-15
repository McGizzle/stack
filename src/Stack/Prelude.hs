{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Stack.Prelude
  ( withSourceFile
  , withSinkFile
  , withSinkFileCautious
  , withSystemTempDir
  , sinkProcessStderrStdout
  , sinkProcessStdout
  , logProcessStderrStdout
  , module X
  ) where

import RIO as X
import           Path                 as X (Abs, Dir, File, Path, Rel,
                                            toFilePath)
import qualified Path.IO

import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)

import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import qualified Data.Conduit.Binary as CB 
import qualified Data.ByteString as S
import qualified System.Process.Typed as P

import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed (eceStdout, eceStderr,createSource, ProcessConfig, Process)
import           RIO.Process (HasEnvOverride, setStdin, closed, getStderr, getStdout, withProc, withProcess_, setStdout, setStderr)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)

--------
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy as BL


import Debug.Trace
-- | Get a source for a file. Unlike @sourceFile@, doesn't require
-- @ResourceT@. Unlike explicit @withBinaryFile@ and @sourceHandle@
-- usage, you can't accidentally use @WriteMode@ instead of
-- @ReadMode@.
withSourceFile :: MonadUnliftIO m => FilePath -> (ConduitM i ByteString m () -> m a) -> m a
withSourceFile fp inner = withBinaryFile fp ReadMode $ inner . sourceHandle

-- | Same idea as 'withSourceFile', see comments there.
withSinkFile :: MonadUnliftIO m => FilePath -> (ConduitM ByteString o m () -> m a) -> m a
withSinkFile fp inner = withBinaryFile fp WriteMode $ inner . sinkHandle

-- | Like 'withSinkFile', but ensures that the file is atomically
-- moved after all contents are written.
withSinkFileCautious
  :: MonadUnliftIO m
  => FilePath
  -> (ConduitM ByteString o m () -> m a)
  -> m a
withSinkFileCautious fp inner =
    withRunInIO $ \run -> bracket acquire cleanup $ \(tmpFP, h) ->
      run (inner $ sinkHandle h) <* (IO.hClose h *> Dir.renameFile tmpFP fp)
  where
    acquire = IO.openBinaryTempFile (FP.takeDirectory fp) (FP.takeFileName fp FP.<.> "tmp")
    cleanup (tmpFP, h) = do
        IO.hClose h
        Dir.removeFile tmpFP `catch` \e ->
            if isDoesNotExistError e
                then return ()
                else throwIO e

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run -> Path.IO.withSystemTempDir str $ run . inner

sinkProcessStderrStdout
  :: forall e o env. HasEnvOverride env
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> ConduitM ByteString Void (RIO env) e -- ^ Sink for stderr
  -> ConduitM ByteString Void (RIO env) o -- ^ Sink for stdout
  -> RIO env (e,o)
sinkProcessStderrStdout name args sinkStderr sinkStdout =
  withProc name args $ \pc0 -> do
    let pc = setStdout createSource
           $ setStderr createSource
             pc0
    withProcess_ pc $ \p ->
      runConduit (getStderr p .| sinkStderr) `concurrently`
      runConduit (getStdout p .| sinkStdout)


-------
modwithProcess :: ProcessConfig stdin stdout stderr
            -> (Process stdin stdout stderr -> IO a)
            -> IO a
modwithProcess config = trace "modWithProcess" $ bracket (P.startProcess config) P.stopProcess


createSourceLogged ref =
    (\h ->
       (  CB.sourceHandle h
       .| CL.iterM (\bs -> liftIO $ modifyIORef ref (. (bs:))))
    )
    `fmap` P.createPipe

withLoggedProcess_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> (Process stdin (ConduitM () S.ByteString m ()) (ConduitM () S.ByteString m ()) -> m a)
  -> m a
withLoggedProcess_ pc inner = withUnliftIO $ \u -> do
  stdoutBuffer <- trace "newIOREFF" $ newIORef id
  stderrBuffer <-  trace "newIOREFF" $ newIORef id
  let pc' = setStdout (createSourceLogged stdoutBuffer)
          $ setStderr (createSourceLogged stderrBuffer) pc
  traceShow pc' $ modwithProcess pc' $ \p -> do
    a <-  trace "unliftIO" $ unliftIO u $ inner p
    let drain src = unliftIO u (runConduit (src .| CL.sinkNull))
    ((), ()) <- trace "draining" $ drain (getStdout p) `concurrently`
                drain (getStderr p)
    P.checkExitCode p `catch` \ece -> do
      stdout <- readIORef stdoutBuffer
      stderr <- readIORef stderrBuffer
      throwIO ece
        { eceStdout = BL.fromChunks $ stdout []
        , eceStderr = BL.fromChunks $ stderr []
        }
    return a


-- | Consume the stdout of a process feeding strict 'ByteString's to a consumer.
-- If the process fails, spits out stdout and stderr as error log
-- level. Should not be used for long-running processes or ones with
-- lots of output; for that use 'sinkProcessStderrStdout'.
--
-- Throws a 'ReadProcessException' if unsuccessful.
sinkProcessStdout
    :: HasEnvOverride env
    => String -- ^ Command
    -> [String] -- ^ Command line arguments
    -> ConduitM ByteString Void (RIO env) a -- ^ Sink for stdout
    -> RIO env a
sinkProcessStdout name args sinkStdout =
  withProc name args $ \pc ->
          -- THIS IS WHERE THE ISSUE IS, withLoggedProcess seems to stall
  trace ("Cmd: " ++ name ++ show args) $ withLoggedProcess_ (setStdin closed pc) $ \p -> trace ("in withLoggedProcess" ++ show p) $ runConcurrently
    $ Concurrently (runConduit $ getStderr p .| CL.sinkNull)
   *> Concurrently (runConduit $ getStdout p .| sinkStdout)

logProcessStderrStdout
    :: (HasCallStack, HasEnvOverride env)
    => String
    -> [String]
    -> RIO env ()
logProcessStderrStdout name args = do
    let logLines = CB.lines .| CL.mapM_ (logInfo . decodeUtf8With lenientDecode)
    ((), ()) <- sinkProcessStderrStdout name args logLines logLines
    return ()
