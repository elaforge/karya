-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic low level pitch calls.
--
-- Low level calls should do simple orthogonal things and their names are
-- generally just one or two characters.
module Derive.Call.Pitch (
    pitch_calls
    , approach
) where
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Global
import Types


-- * pitch

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map
    [ ("", c_set)
    , ("set", c_set)
    , ("'", c_set_prev)

    , ("n", c_neighbor)
    , ("a", c_approach)
    , ("u", c_up)
    , ("d", c_down)
    , ("p", c_porta)
    ]
    <> ControlUtil.standard_interpolators PitchUtil.interpolator_variations

c_set :: Derive.Generator Derive.Pitch
c_set = generator1 "set" mempty "Emit a pitch with no interpolation." $
    -- This could take a transpose too, but then set has to be in
    -- 'require_previous', it gets shadowed for "" because of scales that use
    -- numbers, and it's not clearly useful.
    Sig.call (required "pitch" "Destination pitch or nn.") $ \pitch_ args -> do
        let pitch = either Pitches.nn_pitch id pitch_
        pos <- Args.real_start args
        return $ PitchSignal.signal [(pos, pitch)]

-- | Re-set the previous val.  This can be used to extend a breakpoint.
c_set_prev :: Derive.Generator Derive.Pitch
c_set_prev = Derive.generator Module.prelude "set-prev" Tags.prev
    "Re-set the previous pitch.  This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> do
        start <- Args.real_start args
        return $ case Args.prev_pitch args of
            Nothing -> []
            Just (x, y) -> [PitchSignal.signal [(start, y)] | start > x]

-- * misc

c_neighbor :: Derive.Generator Derive.Pitch
c_neighbor = generator1 "neighbor" mempty
    ("Emit a slide from a neighboring pitch to the given one."
    ) $ Sig.call ((,,)
    <$> required "pitch" "Destination pitch."
    <*> defaulted "neighbor" (Pitch.Chromatic 1) "Neighobr interval."
    <*> defaulted "time" (TrackLang.real 0.1)
        "Time to get to destination pitch."
    ) $ \(pitch, neighbor, TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        let pitch1 = Pitches.transpose neighbor pitch
        PitchUtil.make_interpolator id True start pitch1 end pitch

c_approach :: Derive.Generator Derive.Pitch
c_approach = generator1 "approach" Tags.next
    "Slide to the next pitch." $ Sig.call
    ( defaulted "time" (TrackLang.real 0.2) "Time to get to destination pitch."
    ) $ \(TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        approach args start end

approach :: Derive.PitchArgs -> RealTime -> RealTime
    -> Derive.Deriver PitchSignal.Signal
approach args start end = do
    maybe_next <- next_pitch args
    case (Args.prev_pitch args, maybe_next) of
        (Just (_, prev), Just next) ->
            PitchUtil.make_interpolator id True start prev end next
        _ -> return mempty

next_pitch :: Derive.PassedArgs d -> Derive.Deriver (Maybe PitchSignal.Pitch)
next_pitch = maybe (return Nothing) eval_pitch . Seq.head . Args.next_events

eval_pitch :: Event.Event -> Derive.Deriver (Maybe PitchSignal.Pitch)
eval_pitch event =
    justm (either (const Nothing) Just <$> Eval.eval_event event) $ \strm -> do
    start <- Derive.real (Event.start event)
    return $ PitchSignal.at start $ mconcat $ LEvent.events_of strm

c_up :: Derive.Generator Derive.Pitch
c_up = generator1 "up" Tags.prev
    "Ascend at the given speed until the next event." $ slope "Ascend" 1

c_down :: Derive.Generator Derive.Pitch
c_down = generator1 "down" Tags.prev
    "Descend at the given speed until the next event." $ slope "Descend" (-1)

slope :: Text -> Double -> Derive.WithArgDoc
    (Derive.PitchArgs -> Derive.Deriver PitchSignal.Signal)
slope word sign =
    Sig.call (defaulted "slope" (Pitch.Chromatic 1)
        (word <> " this many steps per second.")) $
    \slope args -> do
        case Args.prev_pitch args of
            Nothing -> return mempty
            Just (_, prev_pitch) -> do
                start <- Args.real_start args
                next <- Derive.real (Args.next args)
                let dest = Pitches.transpose transpose prev_pitch
                    transpose = Pitch.modify_transpose
                        (* (RealTime.to_seconds (next - start) * sign)) slope
                PitchUtil.make_interpolator id True start prev_pitch next dest

c_porta :: Derive.Generator Derive.Pitch
c_porta = linear_interpolation "porta" (TrackLang.real 0.1)
    "Emit a linear slide from the previous pitch to the given one.\
    \ This is the same as i>>, but intended to be higher level, in that\
    \ instruments or scores can override it to represent an idiomatic\
    \ portamento." $
    \_ -> return . TrackLang.default_real

-- TODO obsolete, remove when I can
-- | Linear interpolation, with different start times.
linear_interpolation :: TrackLang.Typecheck time => Text -> time -> Text
    -> (Derive.PitchArgs -> time -> Derive.Deriver TrackLang.Duration)
    -> Derive.Generator Derive.Pitch
linear_interpolation name time_default time_default_doc get_time =
    generator1 name Tags.prev doc $ Sig.call
        ((,) <$> PitchUtil.pitch_arg <*> defaulted "time" time_default time_doc)
    $ \(pitch, time) args ->
        PitchUtil.interpolate id args pitch =<< get_time args time
    where
    doc = "Interpolate from the previous pitch to the given one in a straight\
        \ line."
    time_doc = "Time to reach destination. " <> time_default_doc

-- * util

generator1 :: Text -> Tags.Tags -> Text
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 Module.prelude
