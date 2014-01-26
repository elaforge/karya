-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities dealing with speeds.
module Derive.Call.Speed where
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, typed_control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


arg :: Sig.Parser TrackLang.ValControl
arg = defaulted "speed" (typed_control "speed" 10 Score.Real)
    "Repeat at this speed.  If it's a RealTime, the value is the number of\
    \ repeats per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of repeats per ScoreTime\
    \ unit, and will stretch along with tempo changes."

-- | Get start times until the end of the range, at the given speed.
starts :: (Derive.Time t) => TrackLang.ValControl -> (t, t)
    -> Bool -- ^ If True, include a sample at the end time.
    -> Derive.Deriver [RealTime]
starts speed (start, end) include_end = do
    (speed_sig, time_type) <- Util.to_time_function Util.Real speed
    let take_until end = if include_end then id else takeWhile (<end)
    case time_type of
        Util.Real -> do
            (start, end) <- (,)  <$> Derive.real start <*> Derive.real end
            take_until end <$> real_starts speed_sig start end
        Util.Score -> do
            (start, end) <- (,)  <$> Derive.score start <*> Derive.score end
            starts <- score_starts speed_sig start end
            mapM Derive.real $ take_until end starts

-- | Emit RealTimes at the given speed, which may change over time.  The speed
-- is taken as hertz in real time, and must be >0.
--
-- This returns samples up to and including the end.
real_starts :: (RealTime -> Signal.Y) -> RealTime -> RealTime
    -> Derive.Deriver [RealTime]
real_starts sig start end = get_starts speed_at start (end + RealTime.eta)
    where
    speed_at t = do
        let speed = Signal.y_to_real (sig t)
        when (speed <= 0) $
            Derive.throw $ "Speed.real_starts: speed <= 0: "
                <> Pretty.pretty speed
        return speed

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
score_starts sig start end = get_starts speed_at start (end + ScoreTime.eta)
    where
    speed_at t = do
        speed <- Signal.y_to_score . sig <$> Derive.real t
        when (speed <= 0) $
            Derive.throw $ "Speed.score_starts: speed <= 0: "
                <> Pretty.pretty speed
        return speed

get_starts :: (Ord t, Fractional t, Monad m, Functor m) => (t -> m t) -> t -> t
    -> m [t]
get_starts speed_at start end = do
    first <- speed_at start
    constant first 0
    where
    -- This is a hack to try to avoid loss of precision for the common case of
    -- a constant speed signal.
    constant speed n
        | t > end = return []
        | otherwise = do
            new_speed <- speed_at t
            if speed == new_speed then (t:) <$> constant speed (n+1)
                else nonconstant t
        where t = start + n/speed
    nonconstant t
        | t > end = return []
        | otherwise = do
            speed <- speed_at t
            (t:) <$> nonconstant (t + 1/speed)
