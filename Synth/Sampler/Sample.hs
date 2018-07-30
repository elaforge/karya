-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric #-}
-- | The 'Sample' type and support.
module Synth.Sampler.Sample where
import qualified Sound.File.Sndfile as Sndfile

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import qualified Util.Debug as Debug
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Segment as Segment
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Perform.RealTime as RealTime
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Lib.Global


-- | Path to a sample, relative to the instrument db root.
type SamplePath = FilePath

-- | Low level representation of a note.  This corresponds to a single sample
-- played.
data Sample = Sample {
    start :: !RealTime
    -- | Relative to 'Config.instrumentDbDir'.
    , filename :: !SamplePath
    -- | Sample start offset.
    , offset :: !RealTime
    -- | The sample ends when it runs out of samples, or when envelope ends
    -- on 0.
    , envelope :: !Signal.Signal
    -- | Sample rate conversion ratio.  This controls the pitch.
    , ratio :: !Signal.Signal
    } deriving (Show)

instance Pretty Sample where
    format (Sample start filename offset envelope ratio) =
        Pretty.record "Sample"
            [ ("start", Pretty.format start)
            , ("filename", Pretty.format filename)
            , ("offset", Pretty.format offset)
            , ("envelope", Pretty.format envelope)
            , ("ratio", Pretty.format ratio)
            ]

render :: Resample.Config -> Sample -> Audio
render config (Sample start filename offset envelope ratio) =
    resample2 config ratio start $
    applyEnvelope start envelope $
    File.readFrom (Audio.Frames readFrom) filename
    where
    readFrom = AUtil.toFrame offset + max 0 (now - AUtil.toFrame start)
    now = Resample._now config

resample2 :: Resample.Config -> Signal.Signal -> RealTime -> Audio -> Audio
resample2 config ratio start audio
    -- Don't do any work if it's close enough to 1.  This is likely to be
    -- common, so worth optimizing.
    | Just val <- Signal.constant_val_from start ratio,
            ApproxEq.eq closeEnough val 1 =
        Audio.assertIn (state == Nothing)
            ("expected no state for un-resampled, got " <> showt state) $
        -- resampleBy synchronizes, but File.readFrom doesn't.
        Audio.synchronizeToSize (Resample._now config)
            (Resample._chunkSize config) (silence <> audio)
    | otherwise = silence
        <> Resample.resampleBy2 config (Signal.shift (-start) ratio) audio
        -- The resample always starts at 0 in the ratio, so shift it back to
        -- account for when the sample starts.
    where
    silence = Audio.take (Audio.Frames silenceF) Audio.silence2
    silenceF = max 0 (AUtil.toFrame start - Resample._now config)
    state = Resample._state config
    -- More or less a semitone / 100 cents / 10.  Anything narrower than this
    -- probably isn't perceptible.
    closeEnough = 1.05 / 1000

-- * duration

predictFileDuration :: Signal.Signal -> FilePath -> IO Audio.Frame
predictFileDuration ratio =
    predictDuration ratio . Audio.Frame . Sndfile.frames <$< File.getInfo

type FrameF = Double

toFrameF :: Audio.Frame -> FrameF
toFrameF = fromIntegral

predictDuration :: Signal.Signal -> Audio.Frame -> Audio.Frame
predictDuration ratio sampleDur = case Signal.constant_val_from 0 ratio of
    -- TODO I can also do this optimization if it's constant over the duration
    -- of the sample.  But to know if that's the case I have to do an integral
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
        -- | y1 == y2 = go (input - consumed) (output + consumed * y1) segments
        | otherwise = go (input - consumed) (output + generated)
            (if now >= x2 then rest else segments)
        where
        now = Debug.tracesp "now"
                (input, output, output+generated, head segments) $
            frameToSeconds (output + generated)
        -- The number of samples consumed by a ratio segment is the area under
        -- the curve.  But if I run out of input, I need to find the place
        -- where the integral intersects the amount of time left.

        -- The min of where the curve crosess y (runs out of samples), or
        -- just the integral at (x2-x1).

        consumed = Debug.trace_ret "consumed" (input, integralAt n k delta) $
            min input (integralAt n k delta)
        generated
            | isNaN cross =
                Debug.trace_ret "generated" ((n, k, input), (delta, cross)) $
                    delta
            | otherwise =
                Debug.trace_ret "generated" ((n, k, input), (delta, cross)) $
                min delta cross
            where cross = integralCrossesAt n k input
        -- The ratio multiplies the length of the output.  So it winds up being
        -- the integral of the inverse of the curve.  This is the same thing
        -- that happens for tempo curves.  It's because ratio 2 means I consume
        -- input at 1/2 rate, which leads to output*2.
        n = (1/y2 - 1/y1) / delta
        k = 1 / y1
        delta = toFrameF $ AUtil.toFrame (x2 - x1)

        -- So take the integral, solve for y=input, take min with (x2-x1).
        -- Integral of nx + k = nx^2 / 2 + kx
        -- integral x = n*x^2 / 2 + k*x
        --     where
        --     n = (y2 - y1) / (x2 - x1)
        --     k = min y1 y2
        -- consumed = min input (RealTime.to_seconds (x2 - x1))
        -- generated = consumed * min y1 y2
        --     + consumed * abs (y1-y2) / 2

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

-- integralCrossesAt' :: Double -> Double -> Double -> (Double, Double)
integralCrossesAt' n k y
    | n == 0 = (y / k, y/k)
    {-
        nx^2/2 + kx = y
        nx^2 + 2kx = 2y
        nx^2 + 2kx - 2y = 0
        -- quadratic formula, where a = n, b = 2k, c = -2y
        x = (-2k ± sqrt ((2k)^2 - 4n(-2y))) / 2n

        n=2 k=0
        x = (0 + sqrt (0 - 4*2(-2y))) / 2*2
        x = (sqrt (16y)) / 4

        (-2*k + sqrt ((2*k)^2 + 8*n*y)) / 2*n
        (sqrt (8*n*y)) / 2*n
        (sqrt (8*2*y)) / 2*2
        (sqrt (16*y)) / 4

        -- I want the right side, so take + only:
        x = (-2k + sqrt ((2k)^2 + 8ny)) / 2n
    -}
    | otherwise =
        ( (-2*k + sqrt ((2*k)^2 + 8*n*y)) / (2*n)
        , (-2*k - sqrt ((2*k)^2 + 8*n*y)) / (2*n)
        )

t0 = x0 9
x0 = integralCrossesAt' n k
x1 = integralAt n k

-- n = -1
-- k = 4
n = 2
k = 0

c0 = integralCrossesAt 1 0 3
c1 = integralCrossesAt 1 2 3
c2 = integralCrossesAt 0 2 3

-- | Integral of a line with slope @n@ and offset @k@ at @x@.
integralAt :: Double -> Double -> Double -> Double
integralAt n k x = (n * x^2) / 2 + k*x

-- t0 = quadratic 1 1 (-3)
-- x0 = head t0
-- fx x = x^2 + x - 3
-- quadratic a b c =
--     [ (-b + sqrt (b^2 - 4*a*c)) / 2*a
--     , (-b - sqrt (b^2 - 4*a*c)) / 2*a
--     ]
-- q0 = (-1 + sqrt (1^2 - 4*1*(-3))) / 2*1

{-
A constant segment means progress through sample at that rate.
Iteratively, emit that much output, consume that much input.

If I move to NN breakpoints, this won't work.

-}

-- * old

-- | Evaluating the Audio could probably produce more exceptions...
realize :: Resample.Quality -> Sample -> (RealTime, Audio)
    -- ^ sample start time, and audio to render
realize quality (Sample start filename offset envelope ratio) = (start,) $
    resample quality ratio start $
    applyEnvelope start envelope $
    File.readFrom (Audio.Seconds (RealTime.to_seconds offset)) filename

resample :: Resample.Quality -> Signal.Signal -> RealTime -> Audio -> Audio
resample quality ratio start audio
    -- Don't do any work if it's close enough to 1.  This is likely to be
    -- common, so worth optimizing.
    | Just val <- Signal.constant_val_from start ratio,
            ApproxEq.eq closeEnough val 1 =
        audio
    | otherwise =
        Resample.resampleBy quality (Signal.shift (-start) ratio) audio
    where
    -- More or less a semitone / 100 cents / 10.  Anything narrower than this
    -- probably isn't perceptible.
    closeEnough = 1.05 / 1000

applyEnvelope :: RealTime -> Signal.Signal -> Audio -> Audio
applyEnvelope start sig
    | Just val <- Signal.constant_val_from start sig =
        if ApproxEq.eq 0.01 val 1 then id
            else Audio.gain (AUtil.dbToLinear (Num.d2f val))
    | otherwise = AUtil.volume $ Audio.linear $
        map (first RealTime.to_seconds) $ Signal.to_pairs sig


(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <$< g = fmap f . g
infixl 4 <$<
