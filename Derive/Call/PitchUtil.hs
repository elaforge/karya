-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that emit 'PitchSignal.Signal's.
module Derive.Call.PitchUtil where
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import Derive.Call.ControlUtil (Function, SRate)
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
    -> (Sig.Parser arg, (arg -> Function))
    -> ControlUtil.InterpolatorTime Derive.Pitch
    -> Derive.Generator Derive.Pitch
interpolator_call name (get_arg, function) interpolator_time =
    Derive.generator1 Module.prelude name Tags.prev doc
    $ Sig.call ((,,,)
    <$> pitch_arg
    <*> either id (const $ pure $ TrackLang.Real 0) interpolator_time
    <*> get_arg <*> from_env
    ) $ \(to, time, interpolator_arg, from_) args -> do
        let from = from_ `mplus` (snd <$> Args.prev_pitch args)
        time <- if Args.duration args == 0
            then case interpolator_time of
                Left _ -> return time
                Right (get_time, _) -> get_time args
            else TrackLang.Real <$> Args.real_duration args
        interpolate_from_start (function interpolator_arg) args from time to
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

-- | Pitch version of 'ControlUtil.interpolator_variations'.
interpolator_variations :: Text -> Text -> (Sig.Parser arg, arg -> Function)
    -> [(TrackLang.CallId, Derive.Generator Derive.Pitch)]
interpolator_variations = ControlUtil.interpolator_variations_ interpolator_call


-- * interpolate

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PitchSignal.Signal

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate_from_start :: Function -> Derive.PitchArgs
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
            make_interpolator f True (min start end) from (max start end) $
                resolve_pitch_transpose from to

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
make_interpolator :: Function
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> Derive.Deriver PitchSignal.Signal
make_interpolator f include_initial x1 y1 x2 y2 = do
    srate <- Util.get_srate
    return $ (if include_initial then id else PitchSignal.drop 1)
        (interpolate_segment True srate f x1 y1 x2 y2)

-- | This is bundled into 'make_interpolator', but calls still use this to
-- create interpolations.
interpolator :: SRate -> Function -> Interpolator
interpolator srate f include_initial x1 y1 x2 y2 =
    (if include_initial then id else PitchSignal.drop 1)
        (interpolate_segment True srate f x1 y1 x2 y2)

-- | Interpolate between the given points.
interpolate_segment :: Bool -> SRate -> Function
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> PitchSignal.Signal
interpolate_segment include_end srate f x1 y1 x2 y2 =
    PitchSignal.unfoldr go (Seq.range_ x1 srate)
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
breakpoints :: SRate -> Function -> [(RealTime, PitchSignal.Pitch)]
    -> PitchSignal.Signal
breakpoints srate f =
    ControlUtil.signal_breakpoints PitchSignal.signal
        (interpolate_segment False srate f)
