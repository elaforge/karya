-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | 'Signal' implementation.
module Synth.Shared.Signal (Signal, Y, module Util.TimeVector) where
import Util.TimeVector


-- | A time series signal.  It should be sorted by 'x'.  There is implicit
-- linear interpolation between each sample, so a discontinuity requires two
-- samples with the same 'x'.
--
-- TODO This is different from karya's Signal, which doesn't interpolate.  The
-- reason is that I didn't want to have doubled samples for common
-- discontinuities.  But I need interpolation here because it's audio-level and
-- otherwise I get clicks, while karya emits MIDI which relies on the
-- synthesizer doing the same interpolation, just with a hardcoded latency.
-- But then there is a problem, how do I turn a karya Signal into a synth
-- signal?  Maybe I should switch karya back to interpolation?  Or maybe I just
-- have to be careful to make karya emit doubled samples for explicit
-- discontinuities.  Or I could emulate MIDI and do a fixed latency
-- interpolation.
type Signal = Unboxed
    -- TODO I should be able to get a more efficient deserialize with
    -- vector-mmap

type Y = UnboxedY
