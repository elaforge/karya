-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on control and pitch signals.
module Derive.Call.SignalTransform where
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Sig as Sig
import Derive.Sig (required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * pitch

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.call_maps [] [("sh", c_sh_pitch)]

c_sh_pitch :: Derive.Transformer Derive.Pitch
c_sh_pitch = Derive.transformer "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.pitch_range deriver
        starts <- Speed.starts speed (start, end)
        return $ LEvent.Event (sample_hold_pitch starts sig)
            : map LEvent.Log logs

sample_hold_pitch :: [RealTime] -> PitchSignal.Signal -> PitchSignal.Signal
sample_hold_pitch points sig = PitchSignal.unfoldr go (Nothing, points, sig)
    where
    go (_, [], _) = Nothing
    go (prev, x : xs, sig_) = case PitchSignal.head sig of
            Just (_, y) -> Just ((x, y), (Just y, xs, sig))
            Nothing -> case prev of
                Nothing -> go (prev, xs, sig)
                Just p -> Just ((x, p), (prev, xs, sig))
        where sig = PitchSignal.drop_before x sig_


-- * control

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps []
    [ ("quantize", c_quantize)
    , ("sh", c_sh_control)
    , ("slew", c_slew)
    , ("smooth-i", c_smooth_linear)
    ]

c_sh_control :: Derive.Transformer Derive.Control
c_sh_control = Derive.transformer "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.control_range deriver
        starts <- Speed.starts speed (start, end)
        return $ LEvent.Event (sample_hold_control starts sig)
            : map LEvent.Log logs

sample_hold_control :: [RealTime] -> Signal.Control -> Signal.Control
sample_hold_control points sig = Signal.unfoldr go (0, points, sig)
    where
    go (_, [], _) = Nothing
    go (prev, x : xs, sig_) = case Signal.head sig of
            Just (_, y) -> Just ((x, y), (y, xs, sig))
            Nothing -> Just ((x, prev), (prev, xs, sig))
        where sig = Signal.drop_before x sig_

c_quantize :: Derive.Transformer Derive.Control
c_quantize = Derive.transformer "quantize" mempty
    "Quantize a control signal."
    $ Sig.callt (required "val" "Quantize to multiples of this value.") $
    \val _args -> Post.signal (quantize val)

quantize :: Signal.Y -> Signal.Control -> Signal.Control
quantize val
    | val == 0 = id
    | otherwise = Signal.map_y $ \y -> fromIntegral (round (y / val)) * val

c_slew :: Derive.Transformer Derive.Control
c_slew = Derive.transformer "slew" mempty
    "Smooth a signal by interpolating such that it doesn't exceed the given\
    \ slope."
    $ Sig.callt (required "slope" "Maximum allowed slope, per second.")
    $ \slope _args deriver -> do
        srate <- Util.get_srate
        Post.signal (slew_limiter srate slope) deriver

-- | Smooth the signal by not allowing the signal to change faster than the
-- given slope.
slew_limiter :: RealTime -> Signal.Y -> Signal.Control -> Signal.Control
slew_limiter srate slope =
    Signal.concat . snd . List.mapAccumL go Nothing . Seq.zip_next
        . Signal.unsignal
    where
    -- TODO I tried to think of a way to do this without converting the signal
    -- to a list, and allocating a separate vector for each chunk.  But vector
    -- doesn't have a builder.  unfoldr could do it, but awkwardly because
    -- of the need to emit multiple samples.
    go state ((x, y), next) = case state of
        Nothing -> (Just y, Signal.signal [(x, y)])
        Just prev_y -> ((snd <$> Signal.last segment) `mplus` Just y, segment)
            where
            segment = slope_segment srate srate_slope prev_y (x, y)
                (fst <$> next)
    srate_slope = slope * RealTime.to_seconds srate

-- | Produce a segment up to but not including the next sample.
slope_segment :: RealTime -> Signal.Y -> Signal.Y -> (RealTime, Signal.Y)
    -> Maybe RealTime -> Signal.Control
slope_segment srate slope prev_y (x, y) next = Signal.signal $ zip xs ys
    where
    xs = maybe id (\n -> takeWhile (<n)) next $ Seq.range_ x srate
    -- Since the value is already prev_y, I can start moving immediately.
    ys = drop 1 $ Seq.range_end prev_y y (if y >= prev_y then slope else -slope)

c_smooth_linear :: Derive.Transformer Derive.Control
c_smooth_linear = Derive.transformer "smooth-i" mempty
    "Smooth a signal by replacing each each sample pair with a linear segment."
    $ Sig.callt (required "time" "Amount of time to reach to the next sample.\
        \ If negative, it will end on the destination sample rather than\
        \ start on it. The time will be compressed if the samples are too\
        \ close, so unlike `slew`, this will always reach the samples in the\
        \ source.")
    $ \(TrackLang.DefaultReal time) args deriver -> do
        srate <- Util.get_srate
        time <- Util.real_dur' (Args.start args) time
        Post.signal (smooth id srate time) deriver

-- | Use the function to create a segment between each point in the signal.
-- The only tricky part is when points are too close together.
smooth :: (Double -> Double) -> RealTime -> RealTime -> Signal.Control
    -> Signal.Control
smooth f srate time =
    Signal.concat . snd . List.mapAccumL go Nothing . Seq.zip_next
        . Signal.unsignal
    where
    go state ((x, y), next) = case state of
        Nothing -> (Just (x, y), Signal.signal [(x, y)])
        Just (x0, y0) -> (Signal.last segment `mplus` Just (x, y), segment)
            where
            segment = Signal.drop 1 $ Control.interpolate_segment True srate f
                (max x0 (min x (x+time))) y0
                (maybe id (min . fst) next (max x (x+time))) y
