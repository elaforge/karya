-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is a piecewise-constant signal, with a subset of the TimeVector
-- operations, as used by "Perform.Midi.Perform".
--
-- Unlike the signals built on "Util.Segment', this signal shouldn't have
-- samples with the same 'X'.
module Perform.Midi.MSignal (
    module Perform.Midi.MSignal, module Util.TimeVector
) where
import Prelude hiding (head, last)
import qualified Util.Num as Num
import qualified Util.TimeVector as TimeVector
import Util.TimeVector
       (Sample(..), constant, drop_before, drop_at_after, within, map_y,
        map_err)

import qualified Midi.Midi as Midi
import Global
import Types


type Signal = TimeVector.Unboxed
type Y = TimeVector.UnboxedY
type X = RealTime


-- * re-exports

head, last :: Signal -> Maybe (X, Y)
head = fmap TimeVector.to_pair . TimeVector.head
last = fmap TimeVector.to_pair . TimeVector.last

at :: X -> Signal -> Y
at x = fromMaybe 0 . TimeVector.at x

from_pairs :: [(X, Y)] -> Signal
from_pairs = TimeVector.from_pairs

to_pairs :: Signal -> [(X, Y)]
to_pairs = TimeVector.to_pairs

scalar_add :: Y -> Signal -> Signal
scalar_add y = TimeVector.map_y (+y)

-- * clip_bounds

-- | Clip the signal's Y values to lie between (0, 1), inclusive.  Return the
-- half-open ranges during which the Y was out of range, if any.
--
-- TODO return Y value too, maybe just each (X, Y) where it goes out of range,
-- suppressing adjacent samples.
clip_bounds :: Y -> Y -> Signal -> (Signal, [(X, X)])
clip_bounds low high sig = (clipped, reverse out_of_range)
    where
    clipped = if Prelude.null out_of_range then sig
        else TimeVector.map_y (Num.clamp low high) sig
    (ranges, in_clip) = TimeVector.foldl' go ([], Nothing) sig
    out_of_range = case (in_clip, TimeVector.last sig) of
        (Just start, Just (TimeVector.Sample end _)) -> (start, end) : ranges
        _ -> ranges
    go state@(accum, Nothing) (TimeVector.Sample x y)
        | y < low || y > high = (accum, Just x)
        | otherwise = state
    go state@(accum, Just start) (TimeVector.Sample x y)
        | y < low || y > high = state
        | otherwise = ((start, x) : accum, Nothing)

-- ** pitches_share

{- | Can the pitch signals share a channel within the given range?

    Pitch is complicated.  Like other controls, if the pitch curves are
    different they may not share a channel.  However, if the pitch curves
    are integral transpositions of each other, and the transposition is not
    0, they should share.  Unless the overlap occurs during the decay of one or
    both notes, at which point 0 transposition is ok.
-}
pitches_share :: Bool -> X -> X
    -> Midi.Key -> Signal -> Midi.Key -> Signal -> Bool
pitches_share in_decay start end initial1 sig1 initial2 sig2
    | not in_decay && initial1 == initial2 = False
    | otherwise = pitch_eq (sig1 ! start) (sig2 ! start)
        && pitch_eq (sig1 ! end) (sig2 ! end)
        && signals_share pitch_eq start in1 in2
    where
    in1 = TimeVector.within start end sig1
    in2 = TimeVector.within start end sig2
    pitch_eq = nns_share initial1 initial2
    (!) sig x = fromMaybe 0 $ TimeVector.at x sig

-- | I need to sample points from start to end, including the start and the
-- end.  Unfortunately it's not as simple as it seems it should be, especially
-- since this function is a hotspot and must be efficient.
--
-- Segment.within may return samples before start to get the proper value so
-- I ignore samples before the start.  Start itself is tested explicitly above.
{-# INLINE signals_share #-}
signals_share :: (Y -> Y -> Bool) -> X -> TimeVector.Unboxed
    -> TimeVector.Unboxed -> Bool
signals_share eq start vec1 vec2 = go 0 0 0 0
    where
    go prev_ay prev_by i1 i2 =
        case TimeVector.resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> True
            Just (x, ay, by, i1, i2) ->
                (x <= start || eq ay by) && go ay by i1 i2
    len1 = TimeVector.length vec1
    len2 = TimeVector.length vec2

nns_share :: Midi.Key -> Midi.Key -> Y -> Y -> Bool
nns_share initial1 initial2 nn1 nn2 =
    floor ((nn1 - Midi.from_key initial1) * 1000)
        == floor ((nn2 - Midi.from_key initial2) * 1000)
