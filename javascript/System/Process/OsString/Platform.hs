-- | The parts of the 'OsString' API that differ between platforms. This is
-- the module for the JavaScript backend.
--
-- The JavaScript backend gives strings to node.js as JavaScript strings, so
-- it has to decode an 'OsString' at some point anyway. We decode it first
-- and go through the 'FilePath'-based implementation.
module System.Process.OsString.Platform
    ( createProcess_
    , withCEnvironment
    ) where

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import System.IO (Handle)
import System.OsPath (OsString)

import System.Process.Common (ProcessHandle)
import System.Process.Common.OsString
import qualified System.Process.Internals as F

-- | 'System.Process.createProcess_' for 'OsString'.
--
-- @since 1.6.31.0
createProcess_
  :: String
       -- ^ Function name (for error messages).
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ fun cp = toFilePathCreateProcess cp >>= F.createProcess_ fun

-- | 'System.Process.Internals.withCEnvironment' for 'OsString'.
--
-- @since 1.6.31.0
withCEnvironment :: [(OsString, OsString)] -> (Ptr CString -> IO a) -> IO a
withCEnvironment envir act = do
  envir' <- decodeEnvironment envir
  F.withCEnvironment envir' act
