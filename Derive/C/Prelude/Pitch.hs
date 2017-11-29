-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic low level pitch calls.
--
-- Low level calls should do simple orthogonal things and their names are
-- generally just one or two characters.
module Derive.C.Prelude.Pitch (
    library
    , approach
) where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Global
import Types


library :: Derive.Library
library = mconcat
    [ Library.generators $
        [ ("set", c_set)
        , ("'", c_set_prev)
        , ("*", c_multiply)

        -- interpolating
        , ("n", c_neighbor)
        , ("a", c_approach)
        , ("u", c_up)
        , ("d", c_down)
        , ("p", c_porta)
        ] ++
        ControlUtil.standard_interpolators PitchUtil.interpolator_variations
    , Library.transformers [("set", c_set_transformer)]
    ]

-- * pitch

c_set :: Derive.Generator Derive.Pitch
c_set = generator1 "set" mempty "Emit a pitch with no interpolation." $
    -- This could take a transpose too, but then set has to be in
    -- 'require_previous', it gets shadowed for "" because of scales that use
    -- numbers, and it's not clearly useful.
    Sig.call (required "pitch" "Set this pitch.") $ \pitch_ args -> do
        let pitch = either PSignal.nn_pitch id pitch_
        pos <- Args.real_start args
        return $ PSignal.set (snd <$> Args.prev_pitch args) pos pitch

c_set_transformer :: Derive.Transformer Derive.Pitch
c_set_transformer = Derive.transformer Module.prelude "set" mempty
    "Prepend a pitch to a signal. This is useful to create a discontinuity,\
    \ e.g. interpolate to a pitch and then jump to another one."
    $ Sig.callt (required "pitch" "Set this pitch.")
    $ \pitch_ args deriver -> do
        let pitch = either PSignal.nn_pitch id pitch_
        pos <- Args.real_start args
        let sig = PSignal.signal [(pos, pitch)]
        Post.signal (PSignal.interleave sig) deriver

-- | Re-set the previous val.  This can be used to extend a breakpoint.
c_set_prev :: Derive.Generator Derive.Pitch
c_set_prev = Derive.generator Module.prelude "set-prev" Tags.prev
    "Re-set the previous pitch.  This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> do
        start <- Args.real_start args
        return $ case Args.prev_pitch args of
            Just (x, y) | start > x ->
                Stream.from_event $ PSignal.signal [(start, y)]
            _ -> Stream.empty

c_multiply :: Derive.Generator Derive.Pitch
c_multiply = generator1 "multiply" mempty
    "Emit the given pitch multiplied by a factor."
    $ Sig.call ((,)
    <$> required "pitch" "Source pitch."
    <*> defaulted "interval" (Left 0)
        (ScaleDegree.interval_arg_doc intervals)
    ) $ \(pitch, interval) args -> do
        interval <- ScaleDegree.resolve_intervals intervals [interval]
        scale <- Call.get_scale
        let transposed = Pitches.modify_hz (Pitches.scale scale) (*interval)
                pitch
        start <- Args.real_start args
        return $ PSignal.signal [(start, transposed)]
    where
    intervals = JustScales.named_intervals

-- * interpolating

c_neighbor :: Derive.Generator Derive.Pitch
c_neighbor = generator1 "neighbor" mempty
    "Emit a slide from a neighboring pitch to the given one."
    $ Sig.call ((,,,)
    <$> required "pitch" "Destination pitch."
    <*> defaulted "neighbor" (Pitch.Chromatic 1) "Neighobr interval."
    <*> defaulted "time" (Typecheck.real 0.1) "Time to get to destination."
    <*> ControlUtil.curve_env
    ) $ \(pitch, neighbor, Typecheck.DefaultReal time, curve) args -> do
        (start, end) <- Call.duration_from_start args time
        let pitch1 = Pitches.transpose neighbor pitch
        PitchUtil.make_segment curve start pitch1 end pitch

c_approach :: Derive.Generator Derive.Pitch
c_approach = generator1 "approach" Tags.next
    "Slide to the next pitch." $ Sig.call ((,)
    <$> defaulted "time" (Typecheck.real 0.2) "Time to get to destination."
    <*> ControlUtil.curve_env
    ) $ \(Typecheck.DefaultReal time, curve) args -> do
        (start, end) <- Call.duration_from_start args time
        approach args curve start end

approach :: Derive.PitchArgs -> ControlUtil.Curve -> RealTime -> RealTime
    -> Derive.Deriver PSignal.PSignal
approach args curve start end = do
    maybe_next <- Args.eval_next_pitch args
    case (Args.prev_pitch args, maybe_next) of
        (Just (_, prev), Just next) ->
            PitchUtil.make_segment curve start prev end next
        _ -> return mempty

c_up :: Derive.Generator Derive.Pitch
c_up = generator1 "up" Tags.prev
    "Ascend at the given speed until the next event." $ slope "Ascend" 1

c_down :: Derive.Generator Derive.Pitch
c_down = generator1 "down" Tags.prev
    "Descend at the given speed until the next event." $ slope "Descend" (-1)

slope :: Doc.Doc -> Double -> Derive.WithArgDoc
    (Derive.PitchArgs -> Derive.Deriver PSignal.PSignal)
slope word sign = Sig.call ((,,)
    <$> defaulted "slope" (Pitch.Chromatic 1)
        (word <> " this many steps per second.")
    <*> PitchUtil.from_env <*> ControlUtil.curve_env
    ) $ \(slope, from, curve) args -> case PitchUtil.prev_val from args of
        Nothing -> return mempty
        Just prev_pitch -> do
            start <- Args.real_start args
            next <- Derive.real (Args.next args)
            let dest = Pitches.transpose transpose prev_pitch
                transpose = Pitch.modify_transpose
                    (* (RealTime.to_seconds (next - start) * sign)) slope
            PitchUtil.make_segment curve start prev_pitch next dest

c_porta :: Derive.Generator Derive.Pitch
c_porta = generator1 "porta" mempty
    "Interpolate between two pitches. This is similar to `i>>`,  but intended\
    \ to be higher level, in that instruments or scores can override it to\
    \ represent an idiomatic portamento."
    $ Sig.call ((,,,,)
    <$> PitchUtil.pitch_arg
    <*> defaulted "time" ControlUtil.default_interpolation_time
        "Time to reach destination."
    <*> Sig.defaulted_env "place" Sig.Both (Typecheck.Normalized 0.5)
        "Placement, from before to after the call."
    <*> PitchUtil.from_env <*> ControlUtil.curve_env
    ) $ \(to, Typecheck.DefaultReal time, place, from, curve) args -> do
        let maybe_from = from <|> (snd <$> Args.prev_pitch args)
        time <- if Args.duration args == 0
            then return time
            else BaseTypes.RealDuration <$> Args.real_duration args
        (start, end) <- ControlUtil.place_range place (Args.start args) time
        PitchUtil.make_segment_from curve start maybe_from end to

-- * util

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 Module.prelude
