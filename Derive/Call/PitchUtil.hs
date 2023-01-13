-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that emit 'PSignal.PSignal's.
module Derive.Call.PitchUtil where
import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

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

resolve_pitch_transpose :: PSignal.Pitch -> PitchOrTranspose -> PSignal.Pitch
resolve_pitch_transpose pitch = either id (flip Pitches.transpose pitch)

-- * interpolator call

interpolator_call :: Text -> ControlUtil.CurveD
    -> ControlUtil.InterpolatorTime Derive.Pitch
    -> Derive.Generator Derive.Pitch
interpolator_call name_suffix (ControlUtil.CurveD name get_arg curve)
        interpolator_time =
    Derive.generator1 Module.prelude (Derive.CallName (name <> name_suffix))
        Tags.prev doc
    $ Sig.call ((,,,)
    <$> pitch_arg
    <*> either id (const $ pure $ DeriveT.RealDuration 0) interpolator_time
    <*> get_arg <*> from_env
    ) $ \(to, time, curve_arg, from) args -> do
        time <- if Args.duration args == 0
            then case interpolator_time of
                Left _ -> return time
                Right (get_time, _) -> get_time args
            else DeriveT.RealDuration <$> Args.real_duration args
        (start, end) <- Call.duration_from_start args time
        make_segment_from (curve curve_arg)
            (min start end) (prev_val from args) (max start end) to
    where
    doc = Doc.Doc $ "Interpolate from the previous value to the given one."
        <> either (const "") ((" "<>) . snd) interpolator_time
    -- The only difference between this and ControlUtil.interpolator_call is
    -- the 'interpolate' call and 'pitch_arg'.

pitch_arg :: Sig.Parser PitchOrTranspose
pitch_arg = Sig.required "pitch"
    "Destination pitch, or a transposition from the previous one."

-- | Use this for calls that start from the previous value, to give a way
-- to override that behaviour.
from_env :: Sig.Parser (Maybe PSignal.Pitch)
from_env = Sig.environ "from" Sig.Both (Nothing :: Maybe Sig.Dummy)
    "Start from this pitch. If unset, use the previous pitch."

prev_val :: Maybe PSignal.Pitch -> Derive.PitchArgs -> Maybe PSignal.Pitch
prev_val from args = from <|> (snd <$> Args.prev_pitch args)

-- | Pitch version of 'ControlUtil.interpolator_variations'.
interpolator_variations :: [(Expr.Symbol, Derive.Generator Derive.Pitch)]
interpolator_variations = concat
    [ ControlUtil.interpolator_variations_ interpolator_call sym curve
    | (sym, curve) <- ControlUtil.standard_curves
    ]


-- * interpolate

-- | Make a curve segment from the previous value, if there was one.
make_segment_from :: Curve -> RealTime -> Maybe PSignal.Pitch -> RealTime
    -> PitchOrTranspose -> Derive.Deriver PSignal.PSignal
make_segment_from curve start maybe_from end to = case maybe_from of
    Nothing -> return $ case to of
        Left to -> PSignal.from_sample start to
        Right _ -> mempty
    Just from -> make_segment curve start from end
        (resolve_pitch_transpose from to)

make_segment :: Curve -> RealTime -> PSignal.Pitch -> RealTime
    -> PSignal.Pitch -> Derive.Deriver PSignal.PSignal
make_segment curve x1 y1 x2 y2 = do
    srate <- Call.get_srate
    return $ segment srate curve x1 y1 x2 y2

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
        ControlUtil.Linear -> PSignal.from_pairs [(x1, y1), (x2, y2)]
        ControlUtil.Function curvef -> PSignal.from_pairs $ map (make curvef) $
            Seq.range_end x1 x2 (1/srate)
    where
    make curvef x
        -- Otherwise if x1==x2 then I get y1.
        | x >= x2 = (x2, y2)
        | otherwise = (x, y_at curvef x)
    y_at curvef = Pitches.interpolated y1 y2
        . curvef . Num.normalize (secs x1) (secs x2) . secs
        where secs = RealTime.to_seconds

-- * breakpoints

-- | Create line segments between the given breakpoints.
breakpoints :: SRate -> Curve -> [(RealTime, PSignal.Pitch)] -> PSignal.PSignal
breakpoints _ ControlUtil.Linear = PSignal.from_pairs
breakpoints srate curve =
    ControlUtil.signal_breakpoints PSignal.from_sample (segment srate curve)
