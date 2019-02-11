-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Low level rendering of 'Sample.Sample's.
module Synth.Sampler.RenderSample (
    render
    , envelopeDur
    , predictFileDuration
#ifdef TESTING
    , module Synth.Sampler.RenderSample
#endif
) where
import qualified Sound.File.Sndfile as Sndfile

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import qualified Util.Num as Num
import qualified Util.Segment as Segment
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Perform.RealTime as RealTime
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Lib.Global


render :: Resample.Config -> RealTime -> Sample.Sample -> Audio
render config start (Sample.Sample filename offset envelope ratio) =
    applyEnvelope (AUtil.toSeconds now) envelope $
    resample config ratio start $
    File.readFrom (Audio.Frames offset) filename
    where
    now = Resample._now config

resample :: Resample.Config -> Signal.Signal -> RealTime -> Audio -> Audio
resample config ratio start audio
    -- Don't do any work if it's close enough to 1.  This is likely to be
    -- common, so worth optimizing.
    | Just val <- Signal.constant_val_from start ratio,
            ApproxEq.eq closeEnough val 1 =
        Audio.assertIn (state == Nothing)
            ("expected no state for un-resampled, got " <> pretty state) $
        -- resampleBy synchronizes, but File.readFrom doesn't.
        Audio.synchronizeToSize (Resample._now config)
            (Resample._blockSize config) (silence <> audio)
    | otherwise = silence
        <> Resample.resampleBy (addNow silenceF config)
            (Signal.shift (-start) ratio) audio
        -- The resample always starts at 0 in the ratio, so shift it back to
        -- account for when the sample starts.
    where
    silence = Audio.take (Audio.Frames silenceF) Audio.silence2
    silenceF = max 0 (AUtil.toFrame start - Resample._now config)
    state = Resample._state config
    -- More or less a semitone / 100 cents / 10.  Anything narrower than this
    -- probably isn't perceptible.
    closeEnough = 1.05 / 1000

addNow :: Audio.Frame -> Resample.Config -> Resample.Config
addNow frames config = config { Resample._now = frames + Resample._now config }

applyEnvelope :: RealTime -> Signal.Signal -> Audio -> Audio
applyEnvelope start sig
    | Just val <- Signal.constant_val_from start sig =
        if ApproxEq.eq 0.01 val 1 then id
            else Audio.gain (AUtil.dbToLinear (Num.d2f val))
    | otherwise = AUtil.volume $ clipEnd $ Audio.linear $
        map (first (RealTime.to_seconds . subtract start)) $
        Signal.clip_before_pairs start sig
    where
    clipEnd = maybe id (Audio.take . Audio.Seconds . RealTime.to_seconds)
        (envelopeDur start sig)

-- | If the envelope ends on a 0, I can clip the sample short.  Not just for
-- performance, but because checkpoints rely on the note durations being
-- accurate.
envelopeDur :: RealTime -> Signal.Signal -> Maybe RealTime
envelopeDur start sig = case Signal.last sig of
    Just (x, 0) -> Just $ x - start
    _ -> Nothing

-- * duration

-- | Predict how long a sample will be if resampled with the given ratio
-- signal.
predictFileDuration :: Signal.Signal -> FilePath -> IO Audio.Frame
predictFileDuration ratio =
    fmap (predictDuration ratio . Audio.Frame . Sndfile.frames) . File.getInfo

type FrameF = Double

toFrameF :: Audio.Frame -> FrameF
toFrameF = fromIntegral

predictDuration :: Signal.Signal -> Audio.Frame -> Audio.Frame
predictDuration ratio sampleDur = case Signal.constant_val_from 0 ratio of
    -- I can also do this optimization if it's constant over the duration of
    -- the sample.  But to know if that's the case I have to do an integral
    -- intersection and I think that's the same as the non-optimized case.
    Just y -> toFrame $ toFrameF sampleDur * y
    Nothing -> toFrame $
        go (toFrameF sampleDur) 0 (Signal.clip_before_segments 0 ratio)
    where
    toFrame = Audio.Frame . ceiling
    go !input !output segments@(Segment.Segment x1 y1 x2 y2 : rest)
        | input <= 0 = output
        | y1 <= 0 || y2 <= 0 =
            error $ "ratio went to 0: " <> prettys (head segments)
        -- TODO verify this is the same
        --  | y1 == y2 = go (input - consumed) (output + consumed * y1) segments
        | otherwise = go (input - consumed) (output + generated)
            (if now >= x2 then rest else segments)
        where
        now = -- Debug.tracesp "now"
              --   (input, output, output+generated, head segments) $
            frameToSeconds (output + generated)
        -- The number of samples consumed by a ratio segment is the area under
        -- the curve.  But if I run out of input, I need to find the place
        -- where the integral intersects the amount of time left.

        -- The min of where the curve crosess y (runs out of samples), or
        -- just the integral at (x2-x1).
        consumed =
            -- Debug.trace_ret "consumed" (input, lineIntegral n k delta) $
            min input (lineIntegral n k delta)
        generated
            | isNaN cross =
                -- Debug.trace_ret "generated" ((n, k, input), (delta, cross)) $
                    delta
            | otherwise =
                -- Debug.trace_ret "generated" ((n, k, input), (delta, cross)) $
                min delta cross
            where cross = integralCrossesAt n k input
        -- The ratio multiplies the length of the output.  So it winds up being
        -- the integral of the inverse of the curve.  This is the same thing
        -- that happens for tempo curves.  It's because ratio 2 means I consume
        -- input at 1/2 rate, which leads to output*2.
        n = (1/y2 - 1/y1) / delta
        k = 1 / y1
        delta = toFrameF $ AUtil.toFrame (x2 - x1)

    -- Unreached, because the last segment extends to RealTime.large.
    go _ _ [] = error "ran out of segments"

frameToSeconds :: FrameF -> RealTime
frameToSeconds frames =
    RealTime.seconds $ frames / fromIntegral Config.samplingRate

-- | Given a line with slope @n@ and offset @k@, find where its integral
-- crosses a @y@ value.  NaN means it reaches that value.
integralCrossesAt :: Double -> Double -> Double -> Double
integralCrossesAt n k y
    | n == 0 = y / k
    {-
        nx^2/2 + kx = y
        nx^2 + 2kx = 2y
        nx^2 + 2kx - 2y = 0
        -- quadratic formula, where a = n, b = 2k, c = -2y
        x = (-2k ± sqrt ((2k)^2 - 4n(-2y))) / 2n
        -- I want the right side, so take + only:
        x = (-2k + sqrt ((2k)^2 + 8ny)) / 2n
    -}
    | otherwise = (-2*k + sqrt ((2*k)^2 + 8*n*y)) / (2*n)

-- | Integral of a line with slope @n@ and offset @k@ at @x@.
lineIntegral :: Double -> Double -> Double -> Double
lineIntegral n k x = (n * x^2) / 2 + k*x
