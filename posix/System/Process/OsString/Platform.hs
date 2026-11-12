-- | The parts of the 'OsString' API that differ between platforms. This is
-- the module for POSIX.
--
-- On POSIX an 'OsString' is a sequence of bytes. We give these bytes to the
-- operating system as they are. The 'FilePath'-based implementation cannot
-- do this: it encodes every 'String' with the encoding of the current locale.
module System.Process.OsString.Platform
    ( createProcess_
    , withCEnvironment
    ) where

import Data.ByteString.Short (useAsCString)
import Foreign.C.String (CString)
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (maybeWith, withMany)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO (Handle)
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath
import System.OsString.Internal.Types (OsString(..), PosixString(..))
import System.Posix.PosixPath.FilePath (withFilePath)

import System.Process.Common (ProcessHandle)
import System.Process.Common.OsString
import System.Process.Internals (unwrapHandles)
import System.Process.Posix (ProcessOptions(..), createProcess_Raw)

-- | 'System.Process.createProcess_' for 'OsString'.
--
-- @since 1.6.31.0
createProcess_
  :: String
       -- ^ Function name (for error messages).
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ fun cp =
  withFilePathException cmd $
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

-- | 'System.Process.Internals.withCEnvironment' for 'OsString'.
--
-- @since 1.6.31.0
withCEnvironment :: [(OsString, OsString)] -> (Ptr CString -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name <> ascii "=" <> val) envir
  in withMany withOsString env' (\pEnv -> withArray0 nullPtr pEnv act)
