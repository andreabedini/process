-- | "System.Process.CommunicationHandle" for 'System.OsString.OsString'.
--
-- Only 'readCreateProcessWithExitCodeCommunicationHandle' is different: it
-- takes the 'CreateProcess' record of "System.Process.OsString". The other
-- functions are the same as in "System.Process.CommunicationHandle".
--
-- @since 1.6.31.0
module System.Process.CommunicationHandle.OsString
  ( -- * 'CommunicationHandle': a 'Handle' that can be serialised,
    -- enabling inter-process communication.
    CommunicationHandle
      -- NB: opaque, as the representation depends on the operating system
  , openCommunicationHandleRead
  , openCommunicationHandleWrite
  , closeCommunicationHandle
    -- * Creating 'CommunicationHandle's to communicate with
    -- a child process
  , createWeReadTheyWritePipe
  , createTheyReadWeWritePipe
   -- * High-level API
  , readCreateProcessWithExitCodeCommunicationHandle
  )
 where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)

import System.Process.CommunicationHandle hiding
  ( readCreateProcessWithExitCodeCommunicationHandle )
import System.Process.Internals (ignoreSigPipe, withForkWait)
import System.Process.OsString
  ( CreateProcess, waitForProcess, withCreateProcess )

-- | 'System.Process.CommunicationHandle.readCreateProcessWithExitCodeCommunicationHandle'
-- for 'System.OsString.OsString'.
--
-- A 'CommunicationHandle' has a 'Show' instance and no conversion to
-- 'System.OsString.OsString'. To pass one as an argument, encode the result
-- of 'show', for example with 'System.OsString.encodeUtf'.
--
-- @since 1.6.31.0
readCreateProcessWithExitCodeCommunicationHandle
  :: NFData a
  => ((CommunicationHandle, CommunicationHandle) -> CreateProcess)
    -- ^ Process to spawn, given a @(read, write)@ pair of
    -- 'CommunicationHandle's that are inherited by the spawned process
  -> (Handle -> IO a)
    -- ^ read action
  -> (Handle -> IO ())
    -- ^ write action
  -> IO (ExitCode, a)
readCreateProcessWithExitCodeCommunicationHandle mkProg readAction writeAction = do
  (chTheyRead, hWeWrite   ) <- createTheyReadWeWritePipe
  (hWeRead   , chTheyWrite) <- createWeReadTheyWritePipe
  let cp = mkProg (chTheyRead, chTheyWrite)
  -- The following implementation is the same as the one of
  -- 'System.Process.CommunicationHandle.readCreateProcessWithExitCodeCommunicationHandle'.
  withCreateProcess cp $ \ _ _ _ ph -> do

    -- Close the parent's references to the 'CommunicationHandle's after they
    -- have been inherited by the child (we don't want to keep pipe ends open).
    closeCommunicationHandle chTheyWrite
    closeCommunicationHandle chTheyRead

    -- Fork off a thread that waits on the output.
    output <- readAction hWeRead
    withForkWait (evaluate $ rnf output) $ \ waitOut -> do
      ignoreSigPipe $ writeAction hWeWrite
      ignoreSigPipe $ hClose hWeWrite
      waitOut
      hClose hWeRead

    ex <- waitForProcess ph
    return (ex, output)
