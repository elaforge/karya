-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on control and pitch signals.
module Derive.Call.Prelude.SignalTransform where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- * note

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map [("cf-sample", c_cf_sample)]

-- * pitch

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.transformer_call_map [("sh", c_sh_pitch)]

c_sh_pitch :: Derive.Transformer Derive.Pitch
c_sh_pitch = Derive.transformer Module.prelude "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.pitch_range deriver
        starts <- Speed.starts speed (start, end) True
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
control_calls = Derive.transformer_call_map
    [ ("quantize", c_quantize)
    , ("sh", c_sh_control)
    , ("slew", c_slew)
    , ("smooth", c_smooth)
    , ("->", c_redirect Nothing "the default operator for the control")
    -- TODO should I set to 1 at start and end, like Control.multiply_signal?
    , ("->+", c_redirect (Just $ Derive.Merge Derive.op_add) "addition")
    ]

c_sh_control :: Derive.Transformer Derive.Control
c_sh_control = Derive.transformer Module.prelude "sh" mempty
    "Sample & hold. Hold values at the given speed."
    $ Sig.callt Speed.arg $ \speed _args deriver -> do
        (sig, (start, end), logs) <- Post.control_range deriver
        starts <- Speed.starts speed (start, end) True
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
c_quantize = Derive.transformer Module.prelude "quantize" mempty
    "Quantize a control signal."
    $ Sig.callt (required "val" "Quantize to multiples of this value.") $
    \val _args -> Post.signal (quantize val)

quantize :: Signal.Y -> Signal.Control -> Signal.Control
quantize val
    | val == 0 = id
    | otherwise = Signal.map_y $ \y -> fromIntegral (round (y / val)) * val

c_slew :: Derive.Transformer Derive.Control
c_slew = Derive.transformer Module.prelude "slew" mempty
    "Smooth a signal by interpolating such that it doesn't exceed the given\
    \ slope."
    $ Sig.callt (required "slope" "Maximum allowed slope, per second.")
    $ \slope _args deriver -> do
        srate <- Call.get_srate
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

c_smooth :: Derive.Transformer Derive.Control
c_smooth = Derive.transformer Module.prelude "smooth" mempty
    "Smooth a signal by interpolating between each sample."
    $ Sig.callt ((,)
    <$> required "time" "Amount of time to reach to the next sample.\
        \ If negative, it will end on the destination sample rather than\
        \ start on it. The time will be compressed if the samples are too\
        \ close, so unlike `slew`, this will always reach the samples in the\
        \ source."
    <*> defaulted "curve" "i" "Curve."
    ) $ \(TrackLang.DefaultReal time, curve) args deriver -> do
        srate <- Call.get_srate
        time <- Call.real_duration (Args.start args) time
        f <- Derive.require "curve" (curve_function curve)
        Post.signal (smooth f srate time) deriver

curve_function :: Text -> Maybe (Double -> Double)
curve_function curve = case untxt curve of
    "i" -> Just id
    ['e', n] | Just d <- digit n -> Just $ ControlUtil.expon (fromIntegral (-d))
    [n, 'e'] | Just d <- digit n -> Just $ ControlUtil.expon (fromIntegral d)
    [n1, 'e', n2] | Just d1 <- digit n1, Just d2 <- digit n2 ->
        Just $ ControlUtil.expon2 (fromIntegral d1) (fromIntegral d2)
    -- Overshoot the destination by a certain amount.
    -- ['o', 'v', 'e', 'r', n] | Just d <- digit n -> undefined
    _ -> Nothing
    where
    digit = Num.read_digit

-- | Use the function to create a segment between each point in the signal.
smooth :: (Double -> Double) -> RealTime -> RealTime
    -- ^ If negative, each segment is from this much before the original sample
    -- until the sample.  If positive, it starts on the sample.  If samples are
    -- too close, the segments are shortened correspondingly.
    -> Signal.Control -> Signal.Control
smooth f srate time =
    Signal.concat . snd . List.mapAccumL go Nothing . Seq.zip_next
        . Signal.unsignal
    where
    go state ((x, y), next) = case state of
        Nothing -> (Just (x, y), Signal.signal [(x, y)])
        Just (x0, y0) -> (Signal.last segment `mplus` Just (x, y), segment)
            where
            segment = drop1 $ ControlUtil.segment srate True True f
                (max x0 (min x (x+time))) y0
                (maybe id (min . fst) next (max x (x+time))) y
    -- If the segment length is non-zero, then the first sample is a duplicate
    -- of the previous segment's final one.
    drop1 sig
        | Signal.length sig > 1 = Signal.drop 1 sig
        | otherwise = sig

c_redirect :: Maybe Derive.Merge -> Text -> Derive.Transformer Derive.Control
c_redirect maybe_merge op_name =
    Derive.transformer Module.prelude "redirect" Tags.cmod
    ("Redirect a signal to another control, using the control modifier hack.\
    \ The control is combined with " <> op_name <> ".")
    $ Sig.callt (required "control" "Redirect to this control.")
    $ \control _args deriver -> do
        (sig, logs) <- Post.derive_signal deriver
        merge <- maybe (Derive.get_default_merge control) return maybe_merge
        Derive.modify_control merge control sig
        return $ map LEvent.Log logs

c_cf_sample :: Derive.Transformer Derive.Note
c_cf_sample = Derive.transformer Module.prelude "cf-sample"
    Tags.control_function
    "Sample the given control functions and insert them as constants in the\
    \ control map. The default note call expects continuous signals, so it\
    \ takes slices out of the control map. This transfers control functions\
    \ to the control map, so you can e.g. use randomized controls."
    $ Sig.callt (Sig.many1 "control" "Sample these control functions.")
    $ \controls args deriver -> do
        start <- Args.real_start args
        vals <- mapM (flip Derive.control_at start) (NonEmpty.toList controls)
        foldr (uncurry Call.with_constant) deriver
            [ (c, Score.typed_val v)
            | (c, Just v) <- zip (NonEmpty.toList controls) vals
            ]
