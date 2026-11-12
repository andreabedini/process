-- | The parts of the 'OsString' API that differ between platforms. This is
-- the module for Windows.
--
-- On Windows, the base library turns a 'String' into UTF-16 code units and
-- back without losing anything, and 'toFilePathCreateProcess' uses the same
-- conversion. So we can go through the 'FilePath'-based implementation, and
-- keep only one copy of the code that escapes a Windows command line.
module System.Process.OsString.Platform
    ( createProcess_
    , withCEnvironment
    ) where

import Foreign.C.String (CWString)
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
withCEnvironment :: [(OsString, OsString)] -> (Ptr CWString -> IO a) -> IO a
withCEnvironment envir act = do
  envir' <- decodeEnvironment envir
  F.withCEnvironment envir' act
