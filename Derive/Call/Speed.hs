-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities dealing with speeds.
module Derive.Call.Speed where
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType
import qualified Derive.Warp as Warp

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


data Speed = Score !ScoreTime | Real !RealTime
    deriving (Show)

-- TODO this is a lot of boilerplate just to participate in Typecheck.
instance Typecheck.Typecheck Speed where
    from_val = Typecheck.num_to_scalar $ \(Score.Typed typ val) -> case typ of
        Score.Untyped -> Just $ Real (RealTime.seconds val)
        Score.Real -> Just $ Real (RealTime.seconds val)
        Score.Score -> Just $ Score (ScoreTime.double val)
        _ -> Nothing
    to_type = Typecheck.num_to_type
instance Typecheck.ToVal Speed where
    to_val (Score a) = Typecheck.to_val a
    to_val (Real a) = Typecheck.to_val a
instance Typecheck.TypecheckNum Speed where
    num_type _ = ValType.TTime
instance ShowVal.ShowVal Speed where
    show_val (Score s) = ShowVal.show_val s
    show_val (Real s) = ShowVal.show_val s

arg :: Sig.Parser BaseTypes.ControlRef
arg = Sig.defaulted "speed" (Sig.typed_control "speed" 10 Score.Real)
    "Repeat at this speed.  If it's a RealTime, the value is the number of\
    \ repeats per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of repeats per ScoreTime\
    \ unit, and will stretch along with tempo changes."

-- | Get start times until the end of the range, at the given speed.
starts :: Derive.Time t => BaseTypes.ControlRef -> (t, t)
    -> Bool -- ^ If True, include a sample at the end time.
    -> Derive.Deriver [RealTime]
starts speed (start_, end_) include_end = do
    (speed_sig, time_type) <- Call.to_time_function Typecheck.Real speed
    let take_until e = if include_end then id else takeWhile (<e)
    case time_type of
        Typecheck.Real -> do
            (start, end) <- (,) <$> Derive.real start_ <*> Derive.real end_
            take_until end <$> real_starts speed_sig start end
        Typecheck.Score -> do
            (start, end) <- (,) <$> Derive.score start_ <*> Derive.score end_
            mapM Derive.real . take_until end =<<
                score_starts speed_sig start end

-- | Get start times for a changing speed.  The difference with 'starts' is
-- that the start and end speeds can be different types.
starts_curve :: ControlUtil.CurveF -> Speed -> Speed -> (RealTime, RealTime)
    -> Bool -- ^ If True, include a sample at the end time.
    -> Derive.Deriver [RealTime]
starts_curve curve start_speed end_speed (start, end) include_end = do
    start_dur <- Call.real_duration start (speed_to_duration start_speed)
    end_dur <- Call.real_duration end (speed_to_duration end_speed)
    let dur_at = RealTime.seconds . ControlUtil.make_function curve
            start (RealTime.to_seconds start_dur)
            end (RealTime.to_seconds end_dur)
    let take_until e = if include_end then id else takeWhile (<e)
    Derive.require_right id $
        take_until end <$> duration_starts dur_at start end

speed_to_duration :: Speed -> BaseTypes.Duration
speed_to_duration (Score t) = BaseTypes.ScoreDuration (1/t)
speed_to_duration (Real t) = BaseTypes.RealDuration (1/t)

-- | Emit RealTimes at the given speed, which may change over time.  The speed
-- is taken as hertz in real time, and must be >0.
--
-- This returns samples up to and including the end.
real_starts :: (RealTime -> Signal.Y) -> RealTime -> RealTime
    -> Derive.Deriver [RealTime]
real_starts speed_sig start end = Derive.require_right id $
    duration_starts (RealTime.seconds . (1/) . speed_sig) start end

-- | Emit ScoreTimes at the given speed, which may change over time.  The
-- ScoreTimes are emitted as the reciprocal of the signal at the given point
-- in time, so it must be >0.
--
-- The result is that the speed of the emitted samples should depend on the
-- tempo in effect.
--
-- This returns samples up to and including the end.
score_starts :: (RealTime -> Signal.Y) -> ScoreTime -> ScoreTime
    -> Derive.Deriver [ScoreTime]
score_starts speed_sig start end = do
    dur_sig <- convert_score_signal speed_sig
    Derive.require_right id $
        duration_starts dur_sig start (end + ScoreTime.eta)

-- | Convert a function from RealTime to a ScoreTime duration to a function
-- from ScoreTime to ScoreTime duration.
convert_score_signal :: Typecheck.Function
    -> Derive.Deriver (ScoreTime -> ScoreTime)
convert_score_signal f = do
    warp <- Internal.get_warp
    return $ ScoreTime.double . (1/) . f . Warp.warp warp

duration_starts :: (Num a, Ord a, Show a) => (a -> a) -> a -> a
    -> Either Text [a]
duration_starts dur_at start end = sample (dur_at start) 0 start
    where
    sample prev_dur n t0
        | t > end = return []
        | dur <= 0 = Left $ "duration <= 0: " <> showt dur <> " at " <> showt t
        -- Avoid loss of precision for the common case of a constant speed
        -- signal.
        | dur == prev_dur = (t:) <$> sample prev_dur (n+1) t0
        | otherwise = (t:) <$> sample dur 0 (t+dur)
            where
            t = t0 + prev_dur * n
            dur = dur_at t
