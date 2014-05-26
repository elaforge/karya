-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for control tracks.
module Derive.Call.Control (control_calls) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (required, defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.generator_call_map
    [ ("set", c_set)
    , ("'", c_set_prev)
    , ("abs", c_abs)
    , ("pp", c_dynamic "pp" 0.05)
    , ("p", c_dynamic "p" 0.25)
    , ("mf", c_dynamic "mf" 0.5)
    , ("f", c_dynamic "f" 0.75)
    , ("ff", c_dynamic "ff" 0.95)

    , ("i", c_linear_prev)
    , ("i<<", c_linear_prev_const)
    , ("i>", c_linear_next)
    , ("i>>", c_linear_next_const)
    , ("e", c_exp_prev)
    , ("e<<", c_exp_prev_const)
    , ("e>", c_exp_next)
    , ("e>>", c_exp_next_const)

    -- misc
    , ("bp>", c_breakpoint_next)
    , ("n", c_neighbor)
    , ("d", c_down)
    , ("u", c_up)
    , ("sd", c_set_drop)
    , ("si", c_set_linear)

    -- not sure which one I'll like better
    , ("`ped`", c_pedal)
    , ("h", c_pedal)
    ]
    <> Derive.CallMaps [lookup_number] []

-- | This is a special lookup for control tracks that lets you directly type
-- a number, and have that be interpreted as setting the control to that value.
-- In addition, it allows a special hex syntax
--
-- Formerly, control tracks used a slightly different parser to enable the same
-- thing, but that turned out to be awkward when I wanted to implement
-- 'Call.eval_event'.
lookup_number :: Derive.LookupCall (Derive.Generator Derive.Control)
lookup_number = Derive.LookupPattern "numbers and hex" doc $
    \(TrackLang.Symbol sym) -> return $! case Parse.parse_num sym of
        Left _ -> Nothing
        Right val -> Just $ set val
    where
    set :: Signal.Y -> Derive.Generator Derive.Control
    set val = generator1 "self-eval" mempty
        "Emit a sample with no interpolation. This accepts either decimal\
        \ numbers or hex numbers that look like `\\`0x\\`xx`.  The hex\
        \ is divided by 255, so they represent a number between 0 and 1.\n\
        \ Setting a control called `<controlname>-rnd` will cause the set value\
        \ to be randomized by the given number." $
        Sig.call0 $ \args -> do
            pos <- Args.real_start args
            maybe_control <- Derive.lookup_val Environ.control
            rnd <- case maybe_control of
                Nothing -> return 0
                Just cname -> do
                    rnd_max <- fromMaybe 0 <$> Derive.untyped_control_at
                        (Score.control $ cname <> "-rnd") pos
                    Util.random_in 0 rnd_max
            return $! Signal.signal [(pos, val + rnd)]
    doc = Derive.extract_doc (set 0)

c_set :: Derive.Generator Derive.Control
c_set = generator1 "set" mempty
    "Emit a sample with no interpolation." $
    Sig.call (required "val" "Destination value.") $ \val args -> do
        pos <- Args.real_start args
        return $! Signal.signal [(pos, val)]

c_set_prev :: Derive.Generator Derive.Control
c_set_prev = Derive.generator Module.prelude "set-prev" Tags.prev
    "Re-set the previous value. This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> case Args.prev_control args of
        Nothing -> return []
        Just (x, y) -> do
            start <- Args.real_start args
            return [Signal.signal [(start, y)] | start > x]

c_abs :: Derive.Generator Derive.Control
c_abs = Derive.generator1 Module.prelude "abs" mempty
    "Set the control to an absolute value, provided this control is combined\
    \ via multiplication."
    $ Sig.call (required "val" "Set to this value.") $ \val args ->
        set_absolute val =<< Args.real_start args

c_dynamic :: Text -> Signal.Y -> Derive.Generator Derive.Control
c_dynamic name val = Derive.generator1 Module.prelude name mempty
    "Set the control to an absolute value. This is useful for the `dyn`\
    \ control, so a part can override the dynamic in scope."
    $ Sig.call (defaulted "val" val "Set to this value.") $ \val args ->
        set_absolute val =<< Args.real_start args

set_absolute :: Signal.Y -> RealTime -> Derive.Deriver Signal.Control
set_absolute val pos = do
    control <- Derive.lookup_val Environ.control
    merge <- Derive.lookup_val Environ.merge
    out <- set control merge
    return $ Signal.signal [(pos, out)]
    where
    set Nothing _ = return val
    set (Just control) Nothing =
        Derive.throw $ "merge not set for " <> pretty control
    set (Just control) (Just merge) =
        maybe (return val) (Derive.require_right id . invert_merge merge val)
            =<< Derive.untyped_control_at (Score.control control) pos

invert_merge :: Text -> Signal.Y -> Signal.Y -> Either String Signal.Y
invert_merge merge val current_val = case Map.lookup merge inverters of
    Nothing -> Left $ "no way to invert merge type: " <> untxt merge
    Just f -> Right $ f current_val val
    where
    inverters = Map.fromList
        [ ("set", \_ new -> new)
        , (n Derive.op_add, \old new -> new - old)
        , (n Derive.op_sub, \old new -> old - new)
        , (n Derive.op_mul, \old new -> if old == 0 then 0 else new / old)
        , (n Derive.op_scale, Signal.scale_invert)
        ]
    n (Derive.ControlOp name _) = name

-- * linear

linear_interpolation :: TrackLang.Typecheck time =>
    Text -> Tags.Tags -> time -- ^ arg for duration function
    -> Text -- ^ doc for time arg
    -> (Derive.ControlArgs -> time -> Derive.Deriver TrackLang.Duration)
    -- ^ function from the time arg to the desired duration
    -> Derive.Generator Derive.Control
linear_interpolation name tags time_default time_default_doc get_time =
    generator1 name tags doc $ Sig.call ((,)
    <$> required "val" "Destination value."
    <*> defaulted "time" time_default time_doc
    ) $ \(val, time) args ->
        ControlUtil.interpolate id args val =<< get_time args time
    where
    doc = "Interpolate from the previous sample to the given one in a straight\
        \ line."
    time_doc = "Time to reach destination. " <> time_default_doc

c_linear_prev :: Derive.Generator Derive.Control
c_linear_prev = linear_interpolation "linear" Tags.prev Nothing
    "If not given, start from the previous sample." default_prev

default_prev :: Derive.ControlArgs -> Maybe TrackLang.DefaultReal
    -> Derive.Deriver TrackLang.Duration
default_prev args Nothing = do
    start <- Args.real_start args
    return $ TrackLang.Real $ case Args.prev_control args of
        -- It's likely the callee won't use the duration if there's no prev
        -- val.
        Nothing -> 0
        Just (prev, _) -> prev - start
default_prev _ (Just (TrackLang.DefaultReal t)) = return t

c_linear_prev_const :: Derive.Generator Derive.Control
c_linear_prev_const =
    linear_interpolation "linear-prev-const" mempty (TrackLang.real (-0.1)) "" $
        \_ -> return . TrackLang.default_real

c_linear_next :: Derive.Generator Derive.Control
c_linear_next = linear_interpolation "linear-next" mempty Nothing
        "If not given, default to the start of the next event." $
    \args maybe_time ->
        return $ maybe (next_dur args) TrackLang.default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_linear_next_const :: Derive.Generator Derive.Control
c_linear_next_const =
    linear_interpolation "linear-next-const" mempty (TrackLang.real 0.1) "" $
        \_ -> return . TrackLang.default_real


-- * exponential

-- | Exponential interpolation, with different start times.
exponential_interpolation :: (TrackLang.Typecheck time) =>
    Text -> Tags.Tags -> time -> Text
    -> (Derive.ControlArgs -> time -> Derive.Deriver TrackLang.Duration)
    -> Derive.Generator Derive.Control
exponential_interpolation name tags time_default time_default_doc get_time =
    generator1 name tags doc $ Sig.call ((,,)
    <$> required "val" "Destination value."
    <*> defaulted "exp" 2 ControlUtil.exp_doc
    <*> defaulted "time" time_default time_doc
    ) $ \(pitch, exp, time) args ->
        ControlUtil.interpolate (ControlUtil.expon exp) args pitch
            =<< get_time args time
    where
    doc = "Interpolate from the previous value to the given one in a curve."
    time_doc = "Time to reach destination. " <> time_default_doc

c_exp_prev :: Derive.Generator Derive.Control
c_exp_prev = exponential_interpolation "exp-prev" Tags.prev Nothing
    "If not given, start from the previous sample." default_prev

c_exp_prev_const :: Derive.Generator Derive.Control
c_exp_prev_const = exponential_interpolation "exp-prev-const" mempty
    (TrackLang.real (-0.1)) "" $ \_ -> return . TrackLang.default_real

c_exp_next :: Derive.Generator Derive.Control
c_exp_next = exponential_interpolation "exp-next" mempty Nothing
        "If not given default to the start of the next event." $
    \args maybe_time ->
        return $ maybe (next_dur args) TrackLang.default_real maybe_time
    where next_dur args = TrackLang.Score $ Args.next args - Args.start args

c_exp_next_const :: Derive.Generator Derive.Control
c_exp_next_const = exponential_interpolation "exp-next-const" mempty
    (TrackLang.real 0.1) "" $ \_ -> return . TrackLang.default_real


-- * misc

-- TODO it's linear for now, but I could add an env val to set interpolation
c_breakpoint_next :: Derive.Generator Derive.Control
c_breakpoint_next = generator1 "breakpoint" mempty
    "Interpolate between the given values. Breakpoints start at this event and\
    \ end at the next one."
    $ Sig.call (Sig.many1 "val" "Breakpoints are distributed evenly between\
        \ this event and the next event.")
    $ \vals args -> do
        (start, end) <- Args.real_range_or_next args
        srate <- Util.get_srate
        return $ ControlUtil.breakpoints srate id start end
            (NonEmpty.toList vals)

c_neighbor :: Derive.Generator Derive.Control
c_neighbor = generator1 "neighbor" mempty
    ("Emit a slide from a value to 0 in absolute time. This is the control\
    \ equivalent of the neighbor pitch call."
    ) $ Sig.call ((,)
    <$> defaulted "neighbor" 1 "Start at this value."
    <*> defaulted "time" (TrackLang.real 0.1) "Time taken to get to 0."
    ) $ \(neighbor, TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        srate <- Util.get_srate
        return $! ControlUtil.interpolator srate id True start neighbor end 0

c_down :: Derive.Generator Derive.Control
c_down = generator1 "down" Tags.prev
    "Descend at the given speed until the value reaches 0 or the next event."
    $ Sig.call (defaulted "speed" 1 "Descend this amount per second.") $
    \speed args -> slope args $ \start next prev_y ->
        slope_down speed start next prev_y

c_up :: Derive.Generator Derive.Control
c_up = generator1 "up" Tags.prev
    "Ascend at the given speed until the value reaches 1 or the next event."
    $ Sig.call (defaulted "speed" 1 "Ascend this amount per second.") $
    \speed args -> slope args $ \start next prev_y ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds (1-prev_y) / RealTime.seconds speed)
        in (end, min 1 (prev_y + diff))

-- | Given a start X and maximum end X and start Y, return the end X and end Y.
slope_down :: Double -> RealTime -> RealTime -> Signal.Y -> (RealTime, Signal.Y)
slope_down speed x1 x2 y1 = (end, max 0 (y1 - diff))
    where
    diff = RealTime.to_seconds (x2 - x1) * speed
    end = min x2 $ x1 + RealTime.seconds y1 / RealTime.seconds speed

slope :: Derive.ControlArgs
    -> (RealTime -> RealTime -> Signal.Y -> (RealTime, Signal.Y))
    -> Derive.Deriver Signal.Control
slope args f = case Args.prev_control args of
    Nothing -> return Signal.empty
    Just (_, prev_y) -> do
        (start, next) <- Args.real_range_or_next args
        srate <- Util.get_srate
        let (end, dest) = f start next prev_y
        return $ ControlUtil.interpolator srate id True start prev_y end dest

c_set_drop :: Derive.Generator Derive.Control
c_set_drop = generator1 "sd" mempty
    "A combination of `set` and `down`: set to the given value, and descend."
    $ Sig.call ((,)
    <$> defaulted "val" 1 "Start at this value."
    <*> defaulted "speed" 1 "Descend this amount per second."
    ) $ \(val, speed) args -> do
        (x1, x2) <- Args.real_range_or_next args
        srate <- Util.get_srate
        let (end, dest) = slope_down speed x1 x2 val
        return $ ControlUtil.interpolator srate id True x1 val end dest

c_set_linear :: Derive.Generator Derive.Control
c_set_linear = generator1 "si" mempty "A combination of `set` and `i>`: set to\
    \ one value and interpolate to another."
    $ Sig.call ((,,)
    <$> defaulted "start" 1 "Start at this value."
    <*> defaulted "end" 0 "Interpolate to this value."
    <*> defaulted "time" Nothing "Take this much time to reach `dest`.\
        \ If not given, default to the next event."
    ) $ \(start, end, maybe_time) args -> do
        time <- Derive.real $
            maybe (TrackLang.Score $ Args.next args - Args.start args)
            TrackLang.default_real maybe_time
        start_t <- Args.real_start args
        srate <- Util.get_srate
        return $ ControlUtil.interpolator srate id True
            start_t start (start_t + time) end

c_pedal :: Derive.Generator Derive.Control
c_pedal = generator1 "pedal" mempty
    ("Unlike most control events, this uses a duration. Set the control to\
    \ the given value for the event's duration, and reset to the old\
    \ value afterwards."
    ) $ Sig.call (defaulted "val" 1 "Set to this value.") $
    \val args -> do
        (start, end) <- Args.real_range args
        let prev = maybe 0 snd $ Args.prev_control args
        return $ Signal.signal [(start, val), (end, prev)]

generator1 :: Functor m => Text -> Tags.Tags -> Text
    -> Derive.WithArgDoc (a -> m d) -> Derive.Call (a -> m [LEvent.LEvent d])
generator1 = Derive.generator1 Module.prelude
