-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.OsString
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires concurrency)
--
-- Operations for creating and interacting with sub-processes, for 'OsPath'
-- and 'OsString'.
--
-- This module has the same interface as "System.Process", with one
-- difference: the executable and the working directory are an 'OsPath', and
-- the arguments, the shell command, and the names and values of the
-- environment are an 'OsString'.
--
-- The functions that do not have a string in their type, such as
-- 'waitForProcess', are the same functions as in "System.Process". A
-- 'ProcessHandle' from this module works with "System.Process" and the
-- other way around.
--
-- == Why use this module
--
-- On POSIX systems an argument, a file name or an environment variable is a
-- sequence of bytes. "System.Process" takes them as a 'String', and encodes
-- that 'String' with the encoding of the current locale. Because of this it
-- cannot pass every sequence of bytes, and what the new process receives
-- depends on the locale. For example, with the @C@ locale an argument loses
-- all its characters that are not ASCII.
--
-- The functions in this module give the bytes of an 'OsString' to the
-- operating system as they are. The locale has no effect.
--
-- On Windows an 'OsString' is a sequence of UTF-16 code units, and the new
-- process also receives these as they are. The rules that "System.Process"
-- uses to build the command line of the new process apply here too.
--
-- The standard input and the standard output of a process are still a
-- 'String' in functions such as 'readProcess': they are the content of a
-- stream, not a name that the operating system interprets.
--
-- @since 1.6.31.0
--
-----------------------------------------------------------------------------

module System.Process.OsString (
    -- * Running sub-processes

    -- ** General interface to process creation
    createProcess,
    createProcess_,

    -- ** Creating a @CreateProcess@
    shell, proc,

    -- ** @CreateProcess@ and associated types
    CreateProcess(..),
    CmdSpec(..),
    StdStream(..),
    ProcessHandle,

    -- * Simpler functions for common tasks

    -- ** Call, the simplest way of launching a process
    callCreateProcess,
    callProcess,
    callCommand,

    -- ** Spawn, obtaining a @ProcessHandle@
    spawnProcess,
    spawnCommand,

    -- ** Read, passing @stdin@ and obtaining @stdout@
    readCreateProcess,
    readProcess,

    -- ** Read with exit code, passing @stdin@, obtaining exit code, @stdout@ and @stderr@
    readCreateProcessWithExitCode,
    readProcessWithExitCode,

    -- ** Other
    withCreateProcess,
    cleanupProcess,

    -- ** Related utilities
    showCommandForUser,
    Pid,
    getPid,
    getCurrentPid,

    -- * Process completion
    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    interruptProcessGroupOf,

    -- * Interprocess communication
    createPipe,
    createPipeFd,

    -- * Old functions
    -- | These functions pre-date 'createProcess' which is much more
    -- flexible.
    runProcess,
    runCommand,
    runInteractiveProcess,
    runInteractiveCommand,
    system,
    rawSystem,
    ) where

import Control.DeepSeq (rnf)
import qualified Control.Exception as C
import Control.Monad (unless)
import Data.Maybe (fromJust)
import GHC.IO.Exception (IOErrorType(InvalidArgument, OtherError), ioException)
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.OsPath (OsPath, OsString)

import System.Process
  ( Pid
  , ProcessHandle
  , StdStream(..)
  , cleanupProcess
  , createPipe
  , createPipeFd
  , getCurrentPid
  , getPid
  , getProcessExitCode
  , interruptProcessGroupOf
  , terminateProcess
  , waitForProcess
  )
import qualified System.Process as F
import System.Process.Common.OsString
  ( CmdSpec(..)
  , CreateProcess(..)
  , createProcess_
  , decodeLenient
  , encodeLenient
  )
import System.Process.Internals (ignoreSigPipe, withForkWait)

-- ----------------------------------------------------------------------------
-- createProcess

-- | 'System.Process.proc' for 'OsString'.
--
-- @since 1.6.31.0
proc :: OsPath -> [OsString] -> CreateProcess
proc cmd args = defaultCreateProcess (RawCommand cmd args)

-- | 'System.Process.shell' for 'OsString'.
--
-- @since 1.6.31.0
shell :: OsString -> CreateProcess
shell str = defaultCreateProcess (ShellCommand str)

defaultCreateProcess :: CmdSpec -> CreateProcess
defaultCreateProcess spec =
  CreateProcess { cmdspec = spec,
                  cwd = Nothing,
                  env = Nothing,
                  std_in = Inherit,
                  std_out = Inherit,
                  std_err = Inherit,
                  close_fds = False,
                  create_group = False,
                  delegate_ctlc = False,
                  detach_console = False,
                  create_new_console = False,
                  new_session = False,
                  child_group = Nothing,
                  child_user = Nothing,
                  use_process_jobs = False }

-- | 'System.Process.createProcess' for 'OsString'.
--
-- For example, to run @ls@ in a directory and read its output from a pipe:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import System.OsPath (osp)
-- > import System.OsString (osstr)
-- > import System.Process.OsString
-- >
-- > (_, Just hout, _, ph) <-
-- >     createProcess (proc [osp|ls|] [[osstr|-l|]])
-- >         { cwd = Just [osp|/home/bob|], std_out = CreatePipe }
--
-- @since 1.6.31.0
createProcess
  :: CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  r <- createProcess_ "createProcess" cp
  maybeCloseStd (std_in  cp)
  maybeCloseStd (std_out cp)
  maybeCloseStd (std_err cp)
  return r
 where
  maybeCloseStd :: StdStream -> IO ()
  maybeCloseStd (UseHandle hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeCloseStd _ = return ()

-- | 'System.Process.withCreateProcess' for 'OsString'.
--
-- @since 1.6.31.0
withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    C.bracket (createProcess c) cleanupProcess
              (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- wrapper so we can get exceptions with the appropriate function name.
withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracketOnError (createProcess_ fun c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- ----------------------------------------------------------------------------
-- spawnProcess/spawnCommand

-- | 'System.Process.spawnProcess' for 'OsString'.
--
-- @since 1.6.31.0
spawnProcess :: OsPath -> [OsString] -> IO ProcessHandle
spawnProcess cmd args = do
    (_,_,_,p) <- createProcess_ "spawnProcess" (proc cmd args)
    return p

-- | 'System.Process.spawnCommand' for 'OsString'.
--
-- @since 1.6.31.0
spawnCommand :: OsString -> IO ProcessHandle
spawnCommand cmd = do
    (_,_,_,p) <- createProcess_ "spawnCommand" (shell cmd)
    return p

-- ----------------------------------------------------------------------------
-- callProcess/callCommand

-- | 'System.Process.callCreateProcess' for 'OsString'.
--
-- @since 1.6.31.0
callCreateProcess :: CreateProcess -> IO ()
callCreateProcess = callCreateProcess_ "callCreateProcess"

-- | 'System.Process.callProcess' for 'OsString'.
--
-- @since 1.6.31.0
callProcess :: OsPath -> [OsString] -> IO ()
callProcess cmd = callCreateProcess_ "callProcess" . proc cmd

-- | 'System.Process.callCommand' for 'OsString'.
--
-- @since 1.6.31.0
callCommand :: OsString -> IO ()
callCommand = callCreateProcess_ "callCommand" . shell

callCreateProcess_ :: String -> CreateProcess -> IO ()
callCreateProcess_ fun command = do
    exit_code <- withCreateProcess_ fun
                   command { delegate_ctlc = True } $ \_ _ _ p ->
                   waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> processFailed fun (cmdspec command) r

-- The message is the same as the one of "System.Process". An error message is
-- a String, so we have to decode the command.
processFailed :: String -> CmdSpec -> Int -> IO a
processFailed fun spec exit_code =
    ioError (mkIOError OtherError (fun ++ ": " ++ decodeLenient cmd ++
                                   concatMap ((' ':) . show . decodeLenient) args ++
                                   " (exit " ++ show exit_code ++ ")")
                                Nothing Nothing)
  where
    (cmd, args) = case spec of
      ShellCommand c  -> (c, [])
      RawCommand c as -> (c, as)

-- ----------------------------------------------------------------------------
-- readProcess

-- | 'System.Process.readProcess' for 'OsString'.
--
-- @since 1.6.31.0
readProcess
    :: OsPath                   -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [OsString]               -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcess cmd args = readCreateProcess $ proc cmd args

-- | 'System.Process.readCreateProcess' for 'OsString'.
--
-- @since 1.6.31.0
readCreateProcess
    :: CreateProcess
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readCreateProcess cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe
                  }
    -- The following implementation is the same as the one of
    -- 'System.Process.readCreateProcess'.
    (ex, output) <- withCreateProcess_ "readCreateProcess" cp_opts $
      \mb_inh mb_outh _ ph ->
        case (mb_inh, mb_outh) of
          (Just inh, Just outh) -> do

            -- fork off a thread to start consuming the output
            output  <- hGetContents outh
            withForkWait (C.evaluate $ rnf output) $ \waitOut -> do

              -- now write any input
              unless (null input) $
                ignoreSigPipe $ hPutStr inh input
              -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
              ignoreSigPipe $ hClose inh

              -- wait on the output
              waitOut
              hClose outh

            -- wait on the process
            ex <- waitForProcess ph
            return (ex, output)

          (Nothing,_) -> error "readCreateProcess: Failed to get a stdin handle."
          (_,Nothing) -> error "readCreateProcess: Failed to get a stdout handle."

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> processFailed "readCreateProcess" (cmdspec cp) r

-- | 'System.Process.readProcessWithExitCode' for 'OsString'.
--
-- @since 1.6.31.0
readProcessWithExitCode
    :: OsPath                   -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [OsString]               -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args =
    readCreateProcessWithExitCode $ proc cmd args

-- | 'System.Process.readCreateProcessWithExitCode' for 'OsString'.
--
-- @since 1.6.31.0
readCreateProcessWithExitCode
    :: CreateProcess
    -> String                      -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    -- The following implementation is the same as the one of
    -- 'System.Process.readCreateProcessWithExitCode'.
    withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
      \mb_inh mb_outh mb_errh ph ->
        case (mb_inh, mb_outh, mb_errh) of
          (Just inh, Just outh, Just errh) -> do

            out <- hGetContents outh
            err <- hGetContents errh

            -- fork off threads to start consuming stdout & stderr
            withForkWait  (C.evaluate $ rnf out) $ \waitOut ->
             withForkWait (C.evaluate $ rnf err) $ \waitErr -> do

              -- now write any input
              unless (null input) $
                ignoreSigPipe $ hPutStr inh input
              -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
              ignoreSigPipe $ hClose inh

              -- wait on the output
              waitOut
              waitErr

              hClose outh
              hClose errh

            -- wait on the process
            ex <- waitForProcess ph
            return (ex, out, err)

          (Nothing,_,_) -> error "readCreateProcessWithExitCode: Failed to get a stdin handle."
          (_,Nothing,_) -> error "readCreateProcessWithExitCode: Failed to get a stdout handle."
          (_,_,Nothing) -> error "readCreateProcessWithExitCode: Failed to get a stderr handle."

-- ----------------------------------------------------------------------------
-- showCommandForUser

-- | 'System.Process.showCommandForUser' for 'OsString'.
--
-- The result is an 'OsString', so that it is still correct when the program
-- or an argument is not valid text. To show it to a user, decode it, for
-- example with 'System.OsString.decodeUtf'.
--
-- @since 1.6.31.0
showCommandForUser :: OsPath -> [OsString] -> OsString
showCommandForUser cmd args =
    encodeLenient (F.showCommandForUser (decodeLenient cmd) (map decodeLenient args))

-- ----------------------------------------------------------------------------
-- Old functions

-- | 'System.Process.runCommand' for 'OsString'.
--
-- @since 1.6.31.0
runCommand
  :: OsString
  -> IO ProcessHandle
runCommand string = do
  (_,_,_,ph) <- createProcess_ "runCommand" (shell string)
  return ph

-- | 'System.Process.runProcess' for 'OsString'.
--
-- @since 1.6.31.0
runProcess
  :: OsPath                         -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [OsString]                     -- ^ Arguments to pass to the executable
  -> Maybe OsPath                   -- ^ Optional path to the working directory
  -> Maybe [(OsString, OsString)]   -- ^ Optional environment (otherwise inherit)
  -> Maybe Handle                   -- ^ Handle to use for @stdin@ (Nothing => use existing @stdin@)
  -> Maybe Handle                   -- ^ Handle to use for @stdout@ (Nothing => use existing @stdout@)
  -> Maybe Handle                   -- ^ Handle to use for @stderr@ (Nothing => use existing @stderr@)
  -> IO ProcessHandle
runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr = do
  (_,_,_,ph) <-
      createProcess_ "runProcess"
         (proc cmd args){ cwd = mb_cwd,
                          env = mb_env,
                          std_in  = mbToStd mb_stdin,
                          std_out = mbToStd mb_stdout,
                          std_err = mbToStd mb_stderr }
  maybeClose mb_stdin
  maybeClose mb_stdout
  maybeClose mb_stderr
  return ph
 where
  maybeClose :: Maybe Handle -> IO ()
  maybeClose (Just  hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeClose _ = return ()

  mbToStd :: Maybe Handle -> StdStream
  mbToStd Nothing    = Inherit
  mbToStd (Just hdl) = UseHandle hdl

-- | 'System.Process.runInteractiveCommand' for 'OsString'.
--
-- @since 1.6.31.0
runInteractiveCommand
  :: OsString
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveCommand string =
  runInteractiveProcess1 "runInteractiveCommand" (shell string)

-- | 'System.Process.runInteractiveProcess' for 'OsString'.
--
-- @since 1.6.31.0
runInteractiveProcess
  :: OsPath                         -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [OsString]                     -- ^ Arguments to pass to the executable
  -> Maybe OsPath                   -- ^ Optional path to the working directory
  -> Maybe [(OsString, OsString)]   -- ^ Optional environment (otherwise inherit)
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveProcess cmd args mb_cwd mb_env = do
  runInteractiveProcess1 "runInteractiveProcess"
        (proc cmd args){ cwd = mb_cwd, env = mb_env }

runInteractiveProcess1
  :: String
  -> CreateProcess
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveProcess1 fun cmd = do
  (mb_in, mb_out, mb_err, p) <-
      createProcess_ fun
           cmd{ std_in  = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe }
  return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)

-- | 'System.Process.system' for 'OsString'.
--
-- @since 1.6.31.0
system :: OsString -> IO ExitCode
system str
  | str == mempty = ioException (ioeSetErrorString (mkIOError InvalidArgument "system" Nothing Nothing) "null command")
  | otherwise = do
      (_,_,_,p) <- createProcess_ "system" (shell str) { delegate_ctlc = True }
      waitForProcess p

-- | 'System.Process.rawSystem' for 'OsString'.
--
-- @since 1.6.31.0
rawSystem :: OsPath -> [OsString] -> IO ExitCode
rawSystem cmd args = do
  (_,_,_,p) <- createProcess_ "rawSystem" (proc cmd args) { delegate_ctlc = True }
  waitForProcess p
