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
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


type PitchOrTranspose = Either PitchSignal.Pitch Pitch.Transpose

resolve_pitch_transpose :: PitchSignal.Pitch -> PitchOrTranspose
    -> PitchSignal.Pitch
resolve_pitch_transpose pitch = either id (flip Pitches.transpose pitch)

-- * interpolate

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PitchSignal.Signal

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate :: Function -> Derive.PitchArgs
    -> PitchOrTranspose -> TrackLang.Duration
    -> Derive.Deriver PitchSignal.Signal
interpolate f args pitch_transpose dur = do
    (start, end) <- Util.duration_from_start args dur
    case Args.prev_pitch args of
        Nothing -> return $ case pitch_transpose of
            Left pitch -> PitchSignal.signal [(start, pitch)]
            Right _ -> PitchSignal.signal []
        Just (_, prev) -> do
            -- I always set include_initial.  It might be redundant, but if the
            -- previous call was sliced off, it won't be.
            make_interpolator f True (min start end) prev (max start end) $
                resolve_pitch_transpose prev pitch_transpose

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
