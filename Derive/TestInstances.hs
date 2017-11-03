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

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Expr as Expr -- needed for standalone deriving
import qualified Derive.PSignal as PSignal
import qualified Cmd.Cmd as Cmd

-- Normally Vals aren't comparable for equality because of the pesky VPitch,
-- but it's too convenient for testing to lose.
deriving instance {-# OVERLAPPING #-} Eq BaseTypes.Val
deriving instance {-# OVERLAPPING #-} Eq BaseTypes.Call
deriving instance {-# OVERLAPPING #-} Eq BaseTypes.Term
deriving instance Eq BaseTypes.Quoted

instance Eq PSignal.PSignal where
    sig1 == sig2 = PSignal.unsignal sig1 == PSignal.unsignal sig2
instance Eq (PSignal.RawPitch a) where
    p1 == p2 = PSignal.pitch_nn (PSignal.coerce p1)
        == PSignal.pitch_nn (PSignal.coerce p2)

instance Eq Cmd.Status where
    Cmd.Done == Cmd.Done = True
    Cmd.Continue == Cmd.Continue = True
    Cmd.Quit == Cmd.Quit = True
    Cmd.PlayMidi _ == Cmd.PlayMidi _ = True
    _ == _ = False

instance Eq BaseTypes.ControlFunction where
    _ == _ = False

#endif
