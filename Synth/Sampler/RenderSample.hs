-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeApplications #-}
-- | Low level rendering of 'Sample.Sample's.
module Synth.Sampler.RenderSample (
    render
    , ratioCloseEnough
    , envelopeDuration
    , predictFileDuration
#ifdef TESTING
    , module Synth.Sampler.RenderSample
#endif
) where
import           GHC.TypeLits (KnownNat)

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import qualified Util.Audio.Rubberband as Rubberband
import qualified Util.Num as Num
import qualified Util.Segment as Segment
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Perform.RealTime as RealTime
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


render :: Resample.Config -> RealTime -> Sample.Sample -> IO AUtil.Audio
render config start (Sample.Sample filename offset envelope pan ratios
        stretch) = do
    (close, audio) <- File.readFromClose offset filename
    return $
        applyPan nowS pan $
        applyEnvelope close nowS envelope $
        resample config ratios start $
        -- This has to go right after file read, since it doesn't stream and
        -- doesn't have saveable state.  TODO I don't restore rubberband's
        -- internal state though, so it seems like this could yield artifacts.
        applyRubberband stretch $
        audio
    where
    nowS = AUtil.toSeconds now
    -- The sample stream thinks it's starting at a relative frame 0, so I need
    -- to shift all the signals back so they also line up to frame 0.
    now = Resample._now config

-- | This is polymorphic in chan, though it's only used with 2.  I experimented
-- reading mono files as 1, and expanding the channel after resample and
-- envelope, but the time improvement was tiny, so I dropped it.  It seems like
-- even fancy resampling is cheap compared to read and write time.
resample :: (KnownNat rate, KnownNat chan)
    => Resample.Config -> Signal.Signal -> RealTime
    -> Audio.AudioIO rate chan -> Audio.AudioIO rate chan
resample config ratiosUnshifted start audio
    -- Don't do any work if it's close enough to 1.  This is likely to be
    -- common, so worth optimizing.
    | Just val <- Signal.constant_val_from 0 ratios, ratioCloseEnough val =
        Audio.assertIn (state == Nothing)
            ("expected no state for un-resampled, got " <> pretty state) $
        -- resampleBy synchronizes, but File.readFrom doesn't.
        Audio.synchronizeToSize (Resample._now config)
            (Resample._blockSize config) (silence <> audio)
    | otherwise =
        -- If I'm prepending silence, tweak resample's idea of _now.  It uses
        -- that to align its output blocks.  Also, since that's the real sample
        -- start time, I use it to shift ratios to 0, as mentioned in 'render'.
        let config2 = addNow silenceF config
        in silence <> Resample.resampleBy config2 ratios audio
    where
    sampleStart = AUtil.toSeconds $ Resample._now config + silenceF
    -- The resample always starts at 0 in the ratios, so shift it back to
    -- account for when the sample starts.
    ratios = Signal.shift (-sampleStart) ratiosUnshifted
    silence = Audio.take silenceF Audio.silence
    silenceF = max 0 (AUtil.toFrames start - Resample._now config)
    state = Resample._state config

-- | More or less a semitone / 100 cents / 10.  Anything narrower than this
-- probably isn't perceptible.
ratioCloseEnough :: Signal.Y -> Bool
ratioCloseEnough val = ApproxEq.eq close val 1
    where
    close = 1.05 / 1000

addNow :: Audio.Frames -> Resample.Config -> Resample.Config
addNow frames config = config { Resample._now = frames + Resample._now config }

applyEnvelope :: IO () -> RealTime -> Signal.Signal -> AUtil.Audio
    -> AUtil.Audio
applyEnvelope close start sig
    | Just val <- Signal.constant_val_from start sig =
        if ApproxEq.eq 0.01 val 1 then id
            else Audio.gain (AUtil.dbToLinear (Num.d2f val))
    | otherwise = AUtil.volume $ clipEnd $ realizeSignal start sig
    where
    clipEnd = maybe id
        (Audio.takeCloseS (liftIO close) . RealTime.to_seconds)
        (envelopeDuration start sig)

applyPan :: RealTime -> Signal.Signal -> AUtil.Audio -> AUtil.Audio
applyPan start sig
    | Just val <- Signal.constant_val_from start sig =
        Audio.panConstant (Num.d2f val)
    | otherwise = Audio.pan (realizeSignal start sig)

realizeSignal :: RealTime -> Signal.Signal -> AUtil.Audio1
realizeSignal start sig = Audio.linear True $
    map (first (RealTime.to_seconds . subtract start)) $
    Signal.clip_before_pairs start sig

-- | Get the duration implied by the envelope, or Nothing if the envelope won't
-- shorten the duration.
--
-- If the envelope ends on a 0, I can clip the sample short.  Not just for
-- performance, but because checkpoints rely on the note durations being
-- accurate.
envelopeDuration :: RealTime -> Signal.Signal -> Maybe RealTime
envelopeDuration start = go . Signal.to_pairs_desc
    where
    go [(_, 0)] = Just 0
    go ((_, 0) : xys@((_, 0) : _)) = go xys
    go ((x, 0) : _) = Just $ max 0 (x - start)
    go _ = Nothing

-- * rubberband

applyRubberband :: Sample.Stretch -> AUtil.Audio -> AUtil.Audio
applyRubberband (Sample.Stretch mode timeRatio pitchRatio) audio
    | stretchThreshold timeRatio && stretchThreshold pitchRatio = audio
    | otherwise = Rubberband.offline config audio
    where
    config = Rubberband.config
        { Rubberband._options = modeToOptions mode
        , Rubberband._timeRatio = timeRatio
        , Rubberband._pitchRatio = pitchRatio
        }

modeToOptions :: Sample.StretchMode -> [Rubberband.Option]
modeToOptions = \case
    Sample.StretchDefault -> []
    Sample.StretchPercussive -> Rubberband.percussiveOptions

stretchThreshold :: Double -> Bool
stretchThreshold = ApproxEq.eq 0.01 1

-- * duration

-- | Predict how long a sample will be if resampled with the given ratio
-- signal.
predictFileDuration :: Double -> Signal.Signal -> FilePath -> IO Audio.Frames
predictFileDuration timeRatio ratios fname =
    fmap (stretch . predictDuration ratios) $
        File.throwEnoent fname =<< File.duration fname
    where
    stretch
        | stretchThreshold timeRatio = id
        | otherwise = round . (*timeRatio) . fromIntegral

type FramesF = Double

toFramesF :: Audio.Frames -> FramesF
toFramesF = fromIntegral

predictDuration :: Signal.Signal -> Audio.Frames -> Audio.Frames
predictDuration ratios sampleDur = case Signal.constant_val_from 0 ratios of
    -- I can also do this optimization if it's constant over the duration of
    -- the sample.  But to know if that's the case I have to do an integral
    -- intersection and I think that's the same as the non-optimized case.
    Just y -> toFrames $ toFramesF sampleDur * y
    Nothing -> toFrames $
        go (toFramesF sampleDur) 0 (Signal.clip_before_segments 0 ratios)
    where
    toFrames = Audio.Frames . ceiling
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
        delta = toFramesF $ AUtil.toFrames (x2 - x1)

    -- Unreached, because the last segment extends to RealTime.large.
    go _ _ [] = error "ran out of segments"

frameToSeconds :: FramesF -> RealTime
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
