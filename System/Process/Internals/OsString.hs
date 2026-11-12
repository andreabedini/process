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
    -- * For 'System.OsString.OsString'
    CreateProcess(..),
    CmdSpec(..),
    createProcess_,
    withFilePathException,
    withCEnvironment,
    translate,
    -- * The same as in "System.Process.Internals"
    module System.Process.Internals,
    ) where

import System.Process.Common.OsString
  ( CreateProcess(..)
  , CmdSpec(..)
  , translate
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
import System.Process.OsString.Platform (createProcess_, withCEnvironment)
