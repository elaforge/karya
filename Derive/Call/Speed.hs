-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities dealing with speeds.
module Derive.Call.Speed where
import Util.Control
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, typed_control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


arg :: Sig.Parser TrackLang.ValControl
arg = defaulted "speed" (typed_control "speed" 10 Score.Real)
    "Repeat at this speed.  If it's a RealTime, the value is the number of\
    \ repeats per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of repeats per ScoreTime\
    \ unit, and will stretch along with tempo changes."

-- | Get start times before the end of the range, at the given speed.
starts :: (Derive.Time t) => TrackLang.ValControl -> (t, t)
    -> Derive.Deriver [RealTime]
starts speed (start, end) = do
    (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
    case time_type of
        Util.Real -> do
            (start, end) <- (,)  <$> Derive.real start <*> Derive.real end
            return $ takeWhile (<=end) $ real_starts speed_sig start
        Util.Score -> do
            (start, end) <- (,)  <$> Derive.score start <*> Derive.score end
            starts <- score_starts speed_sig start end
            mapM Derive.real $ takeWhile (<=end) starts

-- | Emit ScoreTimes at the given speed, which may change over time.  The
-- ScoreTimes are emitted as the reciprocal of the signal at the given point
-- in time.
--
-- The result is that the speed of the emitted samples should depend on the
-- tempo in effect.
score_starts :: Signal.Control -> ScoreTime -> ScoreTime
    -> Derive.Deriver [ScoreTime]
score_starts sig start end
    | start > end = return []
    | otherwise = do
        real <- Derive.real start
        let speed = Signal.y_to_score (Signal.at real sig)
        rest <- score_starts sig (start + recip speed) end
        return (start : rest)

-- | Emit an infinite list of RealTimes at the given speed, which may change
-- over time.  The speed is taken as hertz in real time.
real_starts :: Signal.Control -> RealTime -> [RealTime]
real_starts sig start =
    start : real_starts sig (start + Signal.y_to_real (recip speed))
    where speed = Signal.at start sig
