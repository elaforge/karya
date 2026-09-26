-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities dealing with speeds.
module Derive.Call.Speed where
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType
import qualified Derive.Warp as Warp

import qualified Perform.RealTime as RealTime
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


data Speed = Score !ScoreTime | Real !RealTime
    deriving (Show)

-- TODO this is a lot of boilerplate just to participate in Typecheck.
instance Typecheck.Typecheck Speed where
    fromVal = Typecheck.coerceToScalar $ \(ScoreT.Typed typ val) ->
        case typ of
            ScoreT.Untyped -> Just $ Real (RealTime.seconds val)
            ScoreT.Real -> Just $ Real (RealTime.seconds val)
            ScoreT.Score -> Just $ Score (ScoreTime.fromDouble val)
            _ -> Nothing
    toType = Typecheck.numToType
instance Typecheck.ToVal Speed where
    toVal (Score a) = Typecheck.toVal a
    toVal (Real a) = Typecheck.toVal a
instance Typecheck.TypecheckNum Speed where
    numType _ = ValType.TTime
instance ShowVal.ShowVal Speed where
    showVal (Score s) = ShowVal.showVal s
    showVal (Real s) = ShowVal.showVal s

arg :: Sig.Parser Typecheck.RealTimeFunctionT
arg = Sig.defaultedEnv "speed" Derive.Both (10 :: Int)
    "Repeat at this speed.  If it's a RealTime, the value is the number of\
    \ repeats per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of repeats per ScoreTime\
    \ unit, and will stretch along with tempo changes."

-- | Get start times until the end of the range, at the given speed.
starts :: Derive.Time t => Typecheck.RealTimeFunctionT -> (t, t)
    -> Bool -- ^ If True, include a sample at the end time.
    -> Derive.Deriver [RealTime]
starts (Typecheck.RealTimeFunctionT ttype speed) (start_, end_) includeEnd = do
    let takeUntil e = if includeEnd then id else takeWhile (<e)
    case ttype of
        ScoreT.TReal -> do
            (start, end) <- (,) <$> Derive.real start_ <*> Derive.real end_
            takeUntil end <$> realStarts speed start end
        ScoreT.TScore -> do
            (start, end) <- (,) <$> Derive.score start_ <*> Derive.score end_
            mapM Derive.real . takeUntil end =<<
                scoreStarts speed start end

-- | Get start times for a changing speed.  The difference with 'starts' is
-- that the start and end speeds can be different types.
startsCurve :: ControlUtil.CurveF -> Speed -> Speed -> (RealTime, RealTime)
    -> Bool -- ^ If True, include a sample at the end time.
    -> Derive.Deriver [RealTime]
startsCurve curve startSpeed endSpeed (start, end) includeEnd = do
    startDur <- Call.realDuration start (speedToDuration startSpeed)
    endDur <- Call.realDuration end (speedToDuration endSpeed)
    let durAt = RealTime.seconds . ControlUtil.makeFunction curve
            start (RealTime.toSeconds startDur)
            end (RealTime.toSeconds endDur)
    let takeUntil e = if includeEnd then id else takeWhile (<e)
    Derive.requireRight id $
        takeUntil end <$> durationStarts durAt start end

speedToDuration :: Speed -> DeriveT.Duration
speedToDuration (Score t) = DeriveT.ScoreDuration (1/t)
speedToDuration (Real t) = DeriveT.RealDuration (1/t)

-- | Emit RealTimes at the given speed, which may change over time.  The speed
-- is taken as hertz in real time, and must be >0.
--
-- This returns samples up to and including the end.
realStarts :: ScoreT.Function -> RealTime -> RealTime
    -> Derive.Deriver [RealTime]
realStarts speedSig start end = Derive.requireRight id $
    durationStarts (RealTime.seconds . (1/) . speedSig) start end

-- | Emit ScoreTimes at the given speed, which may change over time.  The
-- ScoreTimes are emitted as the reciprocal of the signal at the given point
-- in time, so it must be >0.
--
-- The result is that the speed of the emitted samples should depend on the
-- tempo in effect.
--
-- This returns samples up to and including the end.
scoreStarts :: ScoreT.Function -> ScoreTime -> ScoreTime
    -> Derive.Deriver [ScoreTime]
scoreStarts speedSig start end = do
    durSig <- convertScoreSignal speedSig
    Derive.requireRight id $
        durationStarts durSig start (end + ScoreTime.eta)

-- | Convert a function from RealTime to a ScoreTime duration to a function
-- from ScoreTime to ScoreTime duration.
convertScoreSignal :: ScoreT.Function
    -> Derive.Deriver (ScoreTime -> ScoreTime)
convertScoreSignal f = do
    warp <- Internal.getWarp
    pure $ ScoreTime.fromDouble . (1/) . f . Warp.warp warp

durationStarts :: (Num a, Ord a, Show a) => (a -> a) -> a -> a
    -> Either Text [a]
durationStarts durAt start end = sample (durAt start) 0 start
    where
    sample prevDur n t0
        | t > end = pure []
        | dur <= 0 = Left $ "duration <= 0: " <> showt dur <> " at " <> showt t
        -- Avoid loss of precision for the common case of a constant speed
        -- signal.
        | dur == prevDur = (t:) <$> sample prevDur (n+1) t0
        | otherwise = (t:) <$> sample dur 0 (t+dur)
            where
            t = t0 + prevDur * n
            dur = durAt t
