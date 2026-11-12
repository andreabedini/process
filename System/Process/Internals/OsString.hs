{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Internals.OsString
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- __Note:__ This module exports internal implementation details that may
-- change anytime.  If you want a more stable API, use
-- "System.Process.OsString" instead.
--
-- This module has the same interface as "System.Process.Internals", for
-- 'System.OsPath.OsPath' and 'System.OsString.OsString'. Everything that
-- does not have a string in its type is the same as in
-- "System.Process.Internals".
--
-- The deprecated 'System.Process.Internals.runGenProcess_' is not here.
--
-- @since 1.6.31.0
--
-----------------------------------------------------------------------------

module System.Process.Internals.OsString (
    ProcessHandle(..), ProcessHandle__(..),
    PHANDLE, closePHANDLE, mkProcessHandle,
#if defined(mingw32_HOST_OS)
    CGid(..),
#else
    CGid,
#endif
    GroupID,
    UserID,
    modifyProcessHandle, withProcessHandle,
    CreateProcess(..),
    CmdSpec(..), StdStream(..), ProcRetHandles (..),
    createProcess_,
    fdToHandle,
    startDelegateControlC,
    endDelegateControlC,
    stopDelegateControlC,
    unwrapHandles,
#if defined(mingw32_HOST_OS)
    terminateJob,
    terminateJobUnsafe,
    waitForJobCompletion,
    timeout_Infinite,
#else
#if !defined(javascript_HOST_ARCH)
    pPrPr_disableITimers, c_execvpe,
    runInteractiveProcess_lock,
#endif
    ignoreSignal, defaultSignal,
#endif
    withFilePathException, withCEnvironment,
    translate,
    createPipe,
    createPipeFd,
    interruptProcessGroupOf,
    withForkWait,
    ignoreSigPipe,
    ) where

import System.Process.Common.OsString
  ( CreateProcess(..)
  , CmdSpec(..)
  , createProcess_
  , translate
  , withCEnvironment
  , withFilePathException
  )
import System.Process.Internals hiding
  ( CreateProcess(..)
  , CmdSpec(..)
  , createProcess_
  , runGenProcess_
  , translate
  , withCEnvironment
  , withFilePathException
  )
