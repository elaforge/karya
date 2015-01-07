-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that emit 'PitchSignal.Signal's.
module Derive.Call.PitchUtil where
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import Derive.Call.ControlUtil (Curve, SRate)
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types
import Global


type PitchOrTranspose = Either PitchSignal.Pitch Pitch.Transpose

resolve_pitch_transpose :: PitchSignal.Pitch -> PitchOrTranspose
    -> PitchSignal.Pitch
resolve_pitch_transpose pitch = either id (flip Pitches.transpose pitch)

-- * interpolator call

interpolator_call :: Text
    -> (Sig.Parser arg, (arg -> Curve))
    -> ControlUtil.InterpolatorTime Derive.Pitch
    -> Derive.Generator Derive.Pitch
interpolator_call name (get_arg, curve) interpolator_time =
    Derive.generator1 Module.prelude name Tags.prev doc
    $ Sig.call ((,,,)
    <$> pitch_arg
    <*> either id (const $ pure $ TrackLang.Real 0) interpolator_time
    <*> get_arg <*> from_env
    ) $ \(to, time, curve_arg, from) args -> do
        time <- if Args.duration args == 0
            then case interpolator_time of
                Left _ -> return time
                Right (get_time, _) -> get_time args
            else TrackLang.Real <$> Args.real_duration args
        interpolate_from_start (curve curve_arg) args (prev_val from args)
            time to
    where
    doc = "Interpolate from the previous value to the given one."
        <> either (const "") ((" "<>) . snd) interpolator_time
    -- The only difference between this and ControlUtil.interpolator_call is
    -- the 'interpolate' call and 'pitch_arg'.

pitch_arg :: Sig.Parser PitchOrTranspose
pitch_arg = Sig.required "pitch"
    "Destination pitch, or a transposition from the previous one."

-- | Use this for calls that start from the previous value, to give a way
-- to override that behaviour.
from_env :: Sig.Parser (Maybe PitchSignal.Pitch)
from_env = Sig.environ "from" Sig.Both Nothing
    "Start from this pitch. If unset, use the previous pitch."

prev_val :: Maybe PitchSignal.Pitch -> Derive.PitchArgs
    -> Maybe PitchSignal.Pitch
prev_val from args = from `mplus` (snd <$> Args.prev_pitch args)

-- | Pitch version of 'ControlUtil.interpolator_variations'.
interpolator_variations :: Text -> Text -> (Sig.Parser arg, arg -> Curve)
    -> [(TrackLang.CallId, Derive.Generator Derive.Pitch)]
interpolator_variations = ControlUtil.interpolator_variations_ interpolator_call


-- * interpolate

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate_from_start :: Curve -> Derive.PitchArgs
    -> Maybe PitchSignal.Pitch -> TrackLang.Duration -> PitchOrTranspose
    -> Derive.Deriver PitchSignal.Signal
interpolate_from_start f args from time to = do
    (start, end) <- Util.duration_from_start args time
    case from of
        Nothing -> return $ case to of
            Left pitch -> PitchSignal.signal [(start, pitch)]
            Right _ -> PitchSignal.signal []
        Just from ->
            -- I always set include_initial.  It might be redundant, but if the
            -- previous call was sliced off, it won't be.
            make_segment f (min start end) from (max start end) $
                resolve_pitch_transpose from to

make_segment :: Curve -> RealTime -> PitchSignal.Pitch -> RealTime
    -> PitchSignal.Pitch -> Derive.Deriver PitchSignal.Signal
make_segment = make_segment_ True True

make_segment_ :: Bool -> Bool -> Curve -> RealTime -> PitchSignal.Pitch
    -> RealTime -> PitchSignal.Pitch -> Derive.Deriver PitchSignal.Signal
make_segment_ include_initial include_end f x1 y1 x2 y2 = do
    srate <- Util.get_srate
    return $ segment srate include_initial include_end f x1 y1 x2 y2

type Interpolate = Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PitchSignal.Signal

-- | This defaults some arguments to 'segment' so its more convenient to pass
-- around as a standalone creator of segments.
interpolate_segment :: SRate -> Curve -> Interpolate
interpolate_segment srate f include_initial =
    segment srate include_initial True f

-- | Interpolate between the given points.
segment :: SRate -> Bool -- ^ include the initial sample
    -> Bool -- ^ add a sample at end time if one doesn't naturally land there
    -> Curve -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> PitchSignal.Signal
segment srate include_initial include_end f x1 y1 x2 y2 =
    PitchSignal.unfoldr go $
        (if include_initial then id else drop 1) (Seq.range_ x1 srate)
    where
    go [] = Nothing
    go (x:xs)
        | x >= x2 = if include_end then Just ((x2, y2), []) else Nothing
        | otherwise = Just ((x, y_of x), xs)
    y_of = Pitches.interpolated y1 y2
        . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds

-- * breakpoints

-- | Create line segments between the given breakpoints.
breakpoints :: SRate -> Curve -> [(RealTime, PitchSignal.Pitch)]
    -> PitchSignal.Signal
breakpoints srate f =
    ControlUtil.signal_breakpoints PitchSignal.signal
        (segment srate True False f)
