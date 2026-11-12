{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- | The types of "System.Process.OsString", and the one function of that
-- module that depends on the platform.
module System.Process.OsString.Internal
    ( CreateProcess(..)
    , CmdSpec(..)
    , createProcess_
    , toFilePathCreateProcess
    , decodeLenient
    , encodeLenient
    ) where

import GHC.IO.Encoding.Failure (CodingFailureMode(RoundtripFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.IO (Handle)
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath
import System.OsPath.Encoding (utf16le_b)
#if MIN_VERSION_filepath(1, 5, 0)
import "os-string" System.OsString.Internal.Types (OsString(..))
#else
import "filepath" System.OsString.Internal.Types (OsString(..))
#endif

import System.Process.Common
  ( GroupID, ProcessHandle, StdStream, UserID )
import qualified System.Process.Common as F

#if defined(mingw32_HOST_OS) || defined(javascript_HOST_ARCH)

import qualified System.Process.Internals as F (createProcess_)

#else

import Control.Exception (SomeException, handle, try)
import Foreign.C.String (CString)
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (maybeWith, withMany)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Error (ioeSetFileName)
import System.Posix.PosixPath.FilePath (withFilePath)
#if MIN_VERSION_filepath(1, 5, 0)
import "os-string" System.OsString.Data.ByteString.Short (useAsCString)
import "os-string" System.OsString.Internal.Types (PosixString(..))
#else
import "filepath" System.OsPath.Data.ByteString.Short (useAsCString)
import "filepath" System.OsString.Internal.Types (PosixString(..))
#endif

import System.Process.Internals (unwrapHandles)
import System.Process.Posix (ProcessOptions(..), createProcess_Raw)

#endif

-- ----------------------------------------------------------------------------
-- Types

-- | 'System.Process.CreateProcess' for 'OsString'.
--
-- The fields have the same names and the same meaning as the fields of
-- 'System.Process.CreateProcess'. The documentation of each field is there.
--
-- @since 1.6.31.0
data CreateProcess = CreateProcess{
  cmdspec      :: CmdSpec,                     -- ^ Executable & arguments, or shell command.
  cwd          :: Maybe OsPath,                -- ^ Optional path to the working directory for the new process
  env          :: Maybe [(OsString, OsString)],
                                               -- ^ Optional environment (otherwise inherit from the current process).
                                               --
                                               --   'System.Process.Environment.OsString.getEnvironment' returns
                                               --   the environment of the current process in this form.
  std_in       :: StdStream,                   -- ^ How to determine stdin
  std_out      :: StdStream,                   -- ^ How to determine stdout
  std_err      :: StdStream,                   -- ^ How to determine stderr
  close_fds    :: Bool,                        -- ^ Close all file descriptors except stdin, stdout and stderr in the new process
  create_group :: Bool,                        -- ^ Create a new process group
  delegate_ctlc:: Bool,                        -- ^ Delegate control-C handling
  detach_console :: Bool,                      -- ^ Use the windows DETACHED_PROCESS flag when creating the process; does nothing on other platforms.
  create_new_console :: Bool,                  -- ^ Use the windows CREATE_NEW_CONSOLE flag when creating the process; does nothing on other platforms.
  new_session :: Bool,                         -- ^ Use posix setsid to start the new process in a new session; does nothing on other platforms.
  child_group :: Maybe GroupID,                -- ^ Use posix setgid to set child process's group id; does nothing on other platforms.
  child_user :: Maybe UserID,                  -- ^ Use posix setuid to set child process's user id; does nothing on other platforms.
  use_process_jobs :: Bool                     -- ^ On Windows systems this flag indicates that we should wait for the entire process tree
                                               --   to finish before unblocking. On POSIX systems this flag is ignored.
 } deriving (Show, Eq)

-- | 'System.Process.CmdSpec' for 'OsString'.
--
-- @since 1.6.31.0
data CmdSpec
  = ShellCommand OsString
      -- ^ A command line to execute using the shell
  | RawCommand OsPath [OsString]
      -- ^ The name of an executable with a list of arguments.
      --
      -- See 'System.Process.RawCommand' for how the platform searches for
      -- the executable.
  deriving (Show, Eq)

-- ----------------------------------------------------------------------------
-- Conversion to and from String

-- | Convert to the 'FilePath'-based record of "System.Process".
--
-- Each string is decoded in the same way as the @base@ library decodes a
-- string that it gets from the operating system. On Windows, encoding the
-- result again gives back the original UTF-16 code units, so this conversion
-- loses nothing there.
toFilePathCreateProcess :: CreateProcess -> IO F.CreateProcess
toFilePathCreateProcess cp = do
  cmdspec' <- case cmdspec cp of
    ShellCommand cmd    -> F.ShellCommand <$> OsPath.decodeFS cmd
    RawCommand cmd args -> F.RawCommand <$> OsPath.decodeFS cmd
                                        <*> mapM OsPath.decodeFS args
  cwd' <- traverse OsPath.decodeFS (cwd cp)
  env' <- traverse (mapM decodeVar) (env cp)
  return F.CreateProcess
    { F.cmdspec            = cmdspec'
    , F.cwd                = cwd'
    , F.env                = env'
    , F.std_in             = std_in cp
    , F.std_out            = std_out cp
    , F.std_err            = std_err cp
    , F.close_fds          = close_fds cp
    , F.create_group       = create_group cp
    , F.delegate_ctlc      = delegate_ctlc cp
    , F.detach_console     = detach_console cp
    , F.create_new_console = create_new_console cp
    , F.new_session        = new_session cp
    , F.child_group        = child_group cp
    , F.child_user         = child_user cp
    , F.use_process_jobs   = use_process_jobs cp
    }
  where
    decodeVar (name, val) = (,) <$> OsPath.decodeFS name <*> OsPath.decodeFS val

-- | Decode an 'OsString' without failing and without using the locale.
--
-- A part of the input that is not valid text becomes surrogate code points.
-- 'encodeLenient' turns these back into the original input, so
-- @'encodeLenient' ('decodeLenient' s) == s@ for every @s@.
decodeLenient :: OsString -> String
decodeLenient s =
  either (const (map OsPath.toChar (OsPath.unpack s))) id $
    OsPath.decodeWith (mkUTF8 RoundtripFailure) utf16le_b s

-- | The inverse of 'decodeLenient'.
encodeLenient :: String -> OsString
encodeLenient s =
  either (const (OsPath.pack (map OsPath.unsafeFromChar s))) id $
    OsPath.encodeWith (mkUTF8 RoundtripFailure) utf16le_b s

-- ----------------------------------------------------------------------------
-- createProcess_

-- | 'System.Process.createProcess_' for 'OsString'.
--
-- @since 1.6.31.0
createProcess_
  :: String
       -- ^ Function name (for error messages).
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

#if defined(mingw32_HOST_OS) || defined(javascript_HOST_ARCH)

-- On Windows, the base library turns a String into UTF-16 code units and back
-- without losing anything, and 'toFilePathCreateProcess' uses the same
-- conversion. So we can go through the FilePath-based implementation, and
-- keep only one copy of the code that escapes a Windows command line.
--
-- The JavaScript backend gives strings to node.js as JavaScript strings, so
-- it has to decode an OsString at some point anyway.
createProcess_ fun cp = toFilePathCreateProcess cp >>= F.createProcess_ fun

#else

-- On POSIX an OsString is a sequence of bytes. We give these bytes to the
-- operating system as they are. The FilePath-based implementation cannot do
-- this: it encodes every String with the encoding of the current locale.
createProcess_ fun cp =
  withOsPathException cmd $
   maybeWith withCEnvironment (env cp) $ \pEnv ->
   maybeWith withOsPath (cwd cp) $ \pWorkDir ->
   withOsPath cmd $ \cmdstr ->
   withMany withOsString args $ \argstrs ->
   withArray0 nullPtr (cmdstr : argstrs) $ \pargs ->
     unwrapHandles <$> createProcess_Raw fun opts pargs pWorkDir pEnv
  where
    (cmd, args) = commandToProcess (cmdspec cp)
    opts = ProcessOptions
      { po_std_in             = std_in cp
      , po_std_out            = std_out cp
      , po_std_err            = std_err cp
      , po_close_fds          = close_fds cp
      , po_create_group       = create_group cp
      , po_delegate_ctlc      = delegate_ctlc cp
      , po_detach_console     = detach_console cp
      , po_create_new_console = create_new_console cp
      , po_new_session        = new_session cp
      , po_child_group        = child_group cp
      , po_child_user         = child_user cp
      }

commandToProcess :: CmdSpec -> (OsPath, [OsString])
commandToProcess (ShellCommand string) = (ascii "/bin/sh", [ascii "-c", string])
commandToProcess (RawCommand cmd args) = (cmd, args)

-- | An 'OsString' from a string literal that has only ASCII characters.
ascii :: String -> OsString
ascii = OsPath.pack . map OsPath.unsafeFromChar

-- | Marshal a path. Like 'System.Posix.Internals.withFilePath', this fails
-- if the path has a NUL byte in it (with @unix@ 2.8.2 or later).
withOsPath :: OsPath -> (CString -> IO a) -> IO a
withOsPath = withFilePath . getOsString

-- | Marshal a string that is not a path. Like 'Foreign.C.String.withCString',
-- this does not look for NUL bytes: the operating system sees the string up
-- to its first NUL byte.
withOsString :: OsString -> (CString -> IO a) -> IO a
withOsString = useAsCString . getPosixString . getOsString

withCEnvironment :: [(OsString, OsString)] -> (Ptr CString -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name <> ascii "=" <> val) envir
  in withMany withOsString env' (\pEnv -> withArray0 nullPtr pEnv act)

-- | 'System.Process.Internals.withFilePathException' for 'OsPath'.
withOsPathException :: OsPath -> IO a -> IO a
withOsPathException path = handle mapEx
  where
    mapEx ex = do
      -- This is the file name that the base library would show.
      r <- try (OsPath.decodeFS path)
      let fpath = case r of
            Left e   -> const (decodeLenient path) (e :: SomeException)
            Right fp -> fp
      ioError (ioeSetFileName ex fpath)

#endif
