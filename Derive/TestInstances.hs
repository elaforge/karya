-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP, StandaloneDeriving #-}
-- | Instances that are not safe but very useful for testing.
module Derive.TestInstances where

-- I should be able to just trust this to not be linked in, but it turns out
-- the main app actually does import testing modules for some util functions.
-- Eventually that should stop, but meanwhile...
#ifdef TESTING

import qualified Control.Monad.Fail as Fail

import qualified Ui.Ui as Ui
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr -- needed for standalone deriving
import qualified Derive.PSignal as PSignal
import qualified Cmd.Cmd as Cmd

-- Normally Vals aren't comparable for equality because of the pesky VPitch,
-- but it's too convenient for testing to lose.
deriving instance {-# OVERLAPPING #-} Eq DeriveT.Val
deriving instance {-# OVERLAPPING #-} Eq DeriveT.Call
deriving instance {-# OVERLAPPING #-} Eq DeriveT.Term
deriving instance Eq DeriveT.Quoted

instance Eq PSignal.PSignal where
    sig1 == sig2 = PSignal.to_pairs sig1 == PSignal.to_pairs sig2
instance Eq (PSignal.RawPitch a) where
    p1 == p2 = PSignal.pitch_nn (PSignal.coerce p1)
        == PSignal.pitch_nn (PSignal.coerce p2)

instance Eq Cmd.Status where
    Cmd.Done == Cmd.Done = True
    Cmd.Continue == Cmd.Continue = True
    Cmd.Quit == Cmd.Quit = True
    Cmd.PlayMidi _ == Cmd.PlayMidi _ = True
    _ == _ = False

instance Eq DeriveT.ControlFunction where
    _ == _ = False

-- I want to leave them out of MonadFail so I don't get unexpected errors,
-- but it's useful for tests where a pattern match error is just fine.
instance Monad m => Fail.MonadFail (Ui.StateT m) where fail = error
instance Monad m => Fail.MonadFail (Cmd.CmdT m) where fail = error

#endif
