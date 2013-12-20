-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Instances that are not safe but very useful for testing.
module Derive.TestInstances where

-- I should be able to just trust this to not be linked in, but it turns out
-- the main app actually does import testing modules for some util functions.
-- Eventually that should stop, but meanwhile...
#ifdef TESTING

import qualified Derive.TrackLang as TrackLang
import qualified Derive.PitchSignal as PitchSignal
import qualified Cmd.Cmd as Cmd

-- Normally Vals aren't comparable for equality because of the pesky VPitch,
-- but it's too convenient for testing to lose.
deriving instance Eq TrackLang.Val
deriving instance Eq TrackLang.RawVal
deriving instance Eq TrackLang.Call
deriving instance Eq TrackLang.Term
deriving instance Eq TrackLang.Quoted

instance Eq PitchSignal.Signal where
    sig1 == sig2 = PitchSignal.unsignal sig1 == PitchSignal.unsignal sig2
instance Eq PitchSignal.Pitch where
    p1 == p2 = PitchSignal.pitch_nn p1 == PitchSignal.pitch_nn p2

instance Eq Cmd.Status where
    Cmd.Done == Cmd.Done = True
    Cmd.Continue == Cmd.Continue = True
    Cmd.Quit == Cmd.Quit = True
    Cmd.PlayMidi _ == Cmd.PlayMidi _ = True
    _ == _ = False

#endif
