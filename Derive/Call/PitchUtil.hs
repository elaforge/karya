-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that emit 'PSignal.PSignal's.
module Derive.Call.PitchUtil where
import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Lists as Lists

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import           Derive.Call.ControlUtil (Curve, SRate)
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import           Global
import           Types


type PitchOrTranspose = Either PSignal.Pitch Pitch.Transpose

resolvePitchTranspose :: PSignal.Pitch -> PitchOrTranspose -> PSignal.Pitch
resolvePitchTranspose pitch = either id (flip Pitches.transpose pitch)

-- * interpolator call

interpolatorCall :: Text -> ControlUtil.CurveD
    -> ControlUtil.InterpolatorTime Derive.Pitch
    -> Derive.Generator Derive.Pitch
interpolatorCall nameSuffix (ControlUtil.CurveD name getArg curve)
        interpolatorTime =
    Derive.generator1 Module.prelude (Derive.CallName (name <> nameSuffix))
        Tags.prev doc
    $ Sig.call ((,,,)
    <$> pitchArg
    <*> either id (const $ pure $ DeriveT.RealDuration 0) interpolatorTime
    <*> getArg <*> fromEnv
    ) $ \(to, time, curveArg, from) args -> do
        time <- if Args.duration args == 0
            then case interpolatorTime of
                Left _ -> pure time
                Right (getTime, _) -> getTime args
            else DeriveT.RealDuration <$> Args.realDuration args
        (start, end) <- Call.durationFromStart args time
        makeSegmentFrom (curve curveArg)
            (min start end) (prevVal from args) (max start end) to
    where
    doc = Doc.Doc $ "Interpolate from the previous value to the given one."
        <> either (const "") ((" "<>) . snd) interpolatorTime
    -- The only difference between this and ControlUtil.interpolator_call is
    -- the 'interpolate' call and 'pitch_arg'.

pitchArg :: Sig.Parser PitchOrTranspose
pitchArg = Sig.required "pitch"
    "Destination pitch, or a transposition from the previous one."

-- | Use this for calls that start from the previous value, to give a way
-- to override that behaviour.
fromEnv :: Sig.Parser (Maybe PSignal.Pitch)
fromEnv = Sig.environ "from" Sig.Both (Nothing :: Maybe Sig.Dummy)
    "Start from this pitch. If unset, use the previous pitch."

prevVal :: Maybe PSignal.Pitch -> Derive.PitchArgs -> Maybe PSignal.Pitch
prevVal from args = from <|> (snd <$> Args.prevPitch args)

-- | Pitch version of 'ControlUtil.interpolator_variations'.
interpolatorVariations :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
interpolatorVariations = concat
    [ ControlUtil.interpolatorVariations_ interpolatorCall sym curve
    | (sym, curve) <- ControlUtil.standardCurves
    ]


-- * interpolate

-- | Make a curve segment from the previous value, if there was one.
makeSegmentFrom :: Curve -> RealTime -> Maybe PSignal.Pitch -> RealTime
    -> PitchOrTranspose -> Derive.Deriver PSignal.PSignal
makeSegmentFrom curve start maybeFrom end to = case maybeFrom of
    Nothing -> pure $ case to of
        Left to -> PSignal.fromSample start to
        Right _ -> mempty
    Just from -> makeSegment curve start from end
        (resolvePitchTranspose from to)

makeSegment :: Curve -> RealTime -> PSignal.Pitch -> RealTime
    -> PSignal.Pitch -> Derive.Deriver PSignal.PSignal
makeSegment curve x1 y1 x2 y2 = do
    srate <- Call.getSrate
    pure $ segment srate curve x1 y1 x2 y2

type Interpolate = RealTime -> PSignal.Pitch -> RealTime -> PSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PSignal.PSignal

-- | Interpolate between the given points.
-- TODO(polymorphic-signals) same as ControlUtil.segment, well except Eq use
segment :: SRate -> Curve -> RealTime -> PSignal.Pitch -> RealTime
    -> PSignal.Pitch -> PSignal.PSignal
segment srate curve x1 y1 x2 y2
    | x1 > x2 = mempty -- if x1 == x2 I still need to make a vertical segment
    -- I can't optimize y1==y2, which means flat breakpoints on a nonlinear
    -- curve get redundant samples.
    | otherwise = case curve of
        ControlUtil.Linear -> PSignal.fromPairs [(x1, y1), (x2, y2)]
        ControlUtil.Function curvef -> PSignal.fromPairs $ map (make curvef) $
            Lists.rangeEnd x1 x2 (1/srate)
    where
    make curvef x
        -- Otherwise if x1==x2 then I get y1.
        | x >= x2 = (x2, y2)
        | otherwise = (x, yAt curvef x)
    yAt curvef = Pitches.interpolated y1 y2
        . curvef . Num.normalize (secs x1) (secs x2) . secs
        where secs = RealTime.toSeconds

-- * breakpoints

-- | Create line segments between the given breakpoints.
breakpoints :: SRate -> Curve -> [(RealTime, PSignal.Pitch)] -> PSignal.PSignal
breakpoints _ ControlUtil.Linear = PSignal.fromPairs
breakpoints srate curve =
    ControlUtil.signalBreakpoints PSignal.fromSample (segment srate curve)
