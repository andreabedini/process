-- | The parts of the 'OsString' API that are the same on every platform.
--
-- The parts that differ are in "System.Process.OsString.Platform". That module
-- exists once for each platform, in the directories @posix@, @windows@ and
-- @javascript@, and @process.cabal@ picks one of them.
module System.Process.Common.OsString
    ( CreateProcess(..)
    , CmdSpec(..)
    , withFilePathException
    , translate
    , toFilePathCreateProcess
    , decodeEnvironment
    , decodeLenient
    , encodeLenient
    ) where

import Control.Exception (SomeException, handle, try)
import GHC.IO.Encoding.Failure (CodingFailureMode(RoundtripFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.IO.Error (ioeSetFileName)
import System.OsPath (OsPath, OsString)
import qualified System.OsPath as OsPath
import System.OsPath.Encoding (utf16le_b)

import System.Process.Common (GroupID, StdStream, UserID)
import qualified System.Process.Common as F
import qualified System.Process.Internals as F (translate)

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
  env' <- traverse decodeEnvironment (env cp)
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

-- | Decode an environment in the same way as 'toFilePathCreateProcess'.
decodeEnvironment :: [(OsString, OsString)] -> IO [(String, String)]
decodeEnvironment = mapM decodeVar
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

-- | 'System.Process.Internals.translate' for 'OsString'.
--
-- @since 1.6.31.0
translate :: OsString -> OsString
translate = encodeLenient . F.translate . decodeLenient

-- | 'System.Process.Internals.withFilePathException' for 'OsPath'.
--
-- @since 1.6.31.0
withFilePathException :: OsPath -> IO a -> IO a
withFilePathException path = handle mapEx
  where
    mapEx ex = do
      -- This is the file name that the base library would show.
      r <- try (OsPath.decodeFS path)
      let fpath = case r of
            Left e   -> const (decodeLenient path) (e :: SomeException)
            Right fp -> fp
      ioError (ioeSetFileName ex fpath)
