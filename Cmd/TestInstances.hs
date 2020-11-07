-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP, StandaloneDeriving #-}
-- | Instances that are not safe but very useful for testing.
module Cmd.TestInstances where

#ifndef TESTING
#error "don't import testing modules from non-test code"
#endif

#ifdef TESTING

import qualified Control.Monad.Fail as Fail

import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd

instance Eq Cmd.Status where
    Cmd.Done == Cmd.Done = True
    Cmd.Continue == Cmd.Continue = True
    Cmd.Quit == Cmd.Quit = True
    Cmd.PlayMidi _ == Cmd.PlayMidi _ = True
    _ == _ = False

-- I want to leave them out of MonadFail so I don't get unexpected errors,
-- but it's useful for tests where a pattern match error is just fine.
instance Monad m => Fail.MonadFail (Ui.StateT m) where fail = error
instance Monad m => Fail.MonadFail (Cmd.CmdT m) where fail = error

#endif
