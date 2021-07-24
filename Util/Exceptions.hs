-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities for exceptions.
module Util.Exceptions where
import qualified Control.Exception as Exception
import           Control.Monad (guard, void)
import qualified System.IO.Error as IO.Error


-- | If @op@ raised ENOENT, return Nothing.
ignoreEnoent :: IO a -> IO (Maybe a)
ignoreEnoent = ignoreError IO.Error.isDoesNotExistError

ignoreEnoent_ :: IO a -> IO ()
ignoreEnoent_ = void . ignoreEnoent

ignoreEOF :: IO a -> IO (Maybe a)
ignoreEOF = ignoreError IO.Error.isEOFError

-- | Ignore all IO errors.  This is useful when you want to see if a file
-- exists, because some-file/x will not give ENOENT, but ENOTDIR, which is
-- probably isIllegalOperation.
ignoreIOError :: IO a -> IO (Maybe a)
ignoreIOError = ignoreError (\(_ :: IO.Error.IOError) -> True)

ignoreError :: Exception.Exception e => (e -> Bool) -> IO a -> IO (Maybe a)
ignoreError ignore action = Exception.handleJust (guard . ignore)
    (const (return Nothing)) (fmap Just action)

-- | 'Exception.try' specialized to IOError.
tryIO :: IO a -> IO (Either IO.Error.IOError a)
tryIO = Exception.try
