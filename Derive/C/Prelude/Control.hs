-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic calls for control tracks.
module Derive.C.Prelude.Control (control_calls) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.generator_call_map
    [ ("set", c_set)
    , ("'", c_set_prev)
    , ("p", c_porta)
    , ("abs", c_abs)
    , ("dyn-pp", c_dynamic "pp" 0.05)
    , ("dyn-p", c_dynamic "p" 0.25)
    , ("dyn-mf", c_dynamic "mf" 0.5)
    , ("dyn-f", c_dynamic "f" 0.75)
    , ("dyn-ff", c_dynamic "ff" 0.95)

    -- misc
    , ("bp>", c_breakpoint_next)
    , ("n", c_neighbor)
    , ("d", c_down)
    , ("df", c_down_from)
    , ("u", c_up)

    -- not sure which one I'll like better
    , ("`ped`", c_pedal)
    , ("ped", c_pedal)
    , ("swell", c_swell)
    ]
    <> ControlUtil.standard_interpolators ControlUtil.interpolator_variations
    <> (mempty :: Derive.CallMaps Derive.Control)
        { Derive.scopes_generator = [lookup_generator]
        , Derive.scopes_transformer = [lookup_transformer]
        }

-- | This is a special lookup for control tracks that lets you directly type
-- a number, and have that be interpreted as setting the control to that value.
-- In addition, it allows a special hex syntax
--
-- Formerly, control tracks used a slightly different parser to enable the same
-- thing, but that turned out to be awkward when I wanted to implement
-- 'Call.eval_event'.
lookup_generator :: Derive.LookupCall (Derive.Generator Derive.Control)
lookup_generator = lookup_call $ \val -> generator1 "set" mempty
    "Emit a sample with no interpolation. This accepts either decimal\
    \ numbers or hex numbers that look like `\\`0x\\`xx`.  The hex\
    \ is divided by 255, so they represent a number between 0 and 1.\n\
    \ Setting a control called `<controlname>-rnd` will cause the set value\
    \ to be randomized by the given number." $
    Sig.call0 $ \args -> do
        pos <- Args.real_start args
        return $! Signal.signal [(pos, val)]

lookup_transformer :: Derive.LookupCall (Derive.Transformer Derive.Control)
lookup_transformer = lookup_call $ \val ->
    Derive.transformer Module.prelude "set" mempty
    "Prepend a sample to a signal. This is useful to create a discontinuity,\
    \ e.g. interpolate to a value and then jump to another one."
    $ Sig.call0t $ \args deriver -> do
        pos <- Args.real_start args
        let sig = Signal.signal [(pos, val)]
        Post.signal (Signal.interleave sig) deriver

lookup_call :: (Signal.Y -> Derive.Call d) -> Derive.LookupCall (Derive.Call d)
lookup_call call = Derive.LookupPattern "numbers and hex" doc $
    \(Expr.Symbol sym) -> return $! case Parse.parse_num sym of
        Left _ -> Nothing
        Right val -> Just $ call val
    where doc = Derive.extract_doc (call 0)

c_set :: Derive.Generator Derive.Control
c_set = generator1 "set" mempty
    "Emit a sample with no interpolation." $
    Sig.call (Sig.required "to" "Destination value.") $ \to args -> do
        pos <- Args.real_start args
        return $! Signal.signal [(pos, to)]

c_set_prev :: Derive.Generator Derive.Control
c_set_prev = Derive.generator Module.prelude "set-prev" Tags.prev
    "Re-set the previous value. This can be used to extend a breakpoint."
    $ Sig.call0 $ \args -> case Args.prev_control args of
        Nothing -> return Stream.empty
        Just (x, y) -> do
            start <- Args.real_start args
            return $ if start > x
                then Stream.from_event $ Signal.signal [(start, y)]
                else Stream.empty

c_porta :: Derive.Generator Derive.Control
c_porta = generator1 "porta" mempty
    "Interpolate between two values. This is similar to `i>>`,  but intended\
    \ to be higher level, in that instruments or scores can override it to\
    \ represent an idiomatic portamento."
    $ Sig.call ((,,,,)
    <$> Sig.required "to" "Destination value."
    <*> (Typecheck._real <$> Sig.defaulted "time"
        ControlUtil.default_interpolation_time "Time to reach destination.")
    <*> Sig.defaulted_env "place" Sig.Both (Typecheck.Normalized 0.5)
        "Placement, from before to after the call."
    <*> ControlUtil.from_env <*> ControlUtil.curve_env
    ) $ \(to, time, place, from, curve) args -> do
        let maybe_from = from <|> (snd <$> Args.prev_control args)
        time <- if Args.duration args == 0
            then return time
            else BaseTypes.RealDuration <$> Args.real_duration args
        (start, end) <- ControlUtil.place_range place (Args.start args) time
        ControlUtil.make_segment_from curve start maybe_from end to

c_abs :: Derive.Generator Derive.Control
c_abs = Derive.generator1 Module.prelude "abs" mempty
    "Set the control to an absolute value, provided this control is combined\
    \ via multiplication."
    $ Sig.call (Sig.required "val" "Set to this value.") $ \val args ->
        set_absolute val =<< Args.real_start args

c_dynamic :: Derive.CallName -> Signal.Y -> Derive.Generator Derive.Control
c_dynamic name val = Derive.generator1 Module.prelude name mempty
    "Set the control to an absolute value. This is useful for the `dyn`\
    \ control, so a part can override the dynamic in scope."
    $ Sig.call (Sig.defaulted "val" val "Set to this value.") $ \val args ->
        set_absolute val =<< Args.real_start args

set_absolute :: Signal.Y -> RealTime -> Derive.Deriver Signal.Control
set_absolute val pos = do
    control <- Derive.lookup_val EnvKey.control
    merge <- Derive.lookup_val EnvKey.merge
    out <- set control merge
    return $ Signal.signal [(pos, out)]
    where
    set Nothing _ = return val
    set (Just control) Nothing =
        Derive.throw $ "merge not set for " <> pretty control
    set (Just control) (Just merge) =
        maybe (return val) (Derive.require_right id . invert_merge merge val)
            =<< Derive.untyped_control_at (Score.unchecked_control control) pos

-- | Figure out what value to emit which, once merged with the signal in scope,
-- will become the given absolute value.  TODO this is kind of a crazy hack and
-- I'm not sure if I'm in love with it.
invert_merge :: Text -> Signal.Y -> Signal.Y -> Either Text Signal.Y
invert_merge merge val current_val = case Map.lookup merge inverters of
    Nothing -> Left $ "no way to invert merge type: " <> merge
    Just f -> Right $ f current_val val
    where
    inverters = Map.fromList
        [ ("set", \_ new -> new)
        , (n Derive.merge_add, \old new -> new - old)
        , (n Derive.merge_sub, \old new -> old - new)
        , (n Derive.merge_mul, \old new -> if old == 0 then 0 else new / old)
        , (n Derive.merge_scale, Signal.scale_invert)
        ]
    n (Derive.Merger name _ _) = name
    n Derive.Set = "set"


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
        srate <- Call.get_srate
        return $ ControlUtil.breakpoints srate id $
            ControlUtil.distribute start end (NonEmpty.toList vals)

c_neighbor :: Derive.Generator Derive.Control
c_neighbor = generator1 "neighbor" mempty
    ("Emit a slide from a value to 0 in absolute time. This is the control\
    \ equivalent of the neighbor pitch call."
    ) $ Sig.call ((,,)
    <$> Sig.defaulted "neighbor" 1 "Start at this value."
    <*> Sig.defaulted "time" (Typecheck.real 0.1) "Time taken to get to 0."
    <*> ControlUtil.curve_env
    ) $ \(neighbor, Typecheck.DefaultReal time, curve) args -> do
        (start, end) <- Call.duration_from_start args time
        ControlUtil.make_segment curve start neighbor end 0

c_up :: Derive.Generator Derive.Control
c_up = generator1 "u" Tags.prev
    "Ascend at the given speed until the value reaches 1 or the next event."
    $ Sig.call ((,,)
    <$> Sig.defaulted "speed" 1 "Ascend this amount per second."
    <*> Sig.defaulted "limit" Nothing "Stop at this value."
    <*> ControlUtil.from_env
    ) $ \(speed, limit, from) args -> make_slope args Nothing limit from speed

c_down :: Derive.Generator Derive.Control
c_down = generator1 "d" Tags.prev
    "Descend at the given speed until the value reaches 0 or the next event."
    $ Sig.call ((,,)
    <$> Sig.defaulted "speed" 1 "Descend this amount per second."
    <*> Sig.defaulted "limit" Nothing "Stop at this value."
    <*> ControlUtil.from_env
    ) $ \(speed, limit, from) args ->
        make_slope args limit Nothing from (-speed)

c_down_from :: Derive.Generator Derive.Control
c_down_from = generator1 "df" mempty
    "Drop from a certain value. This is like `d` with `from`, but more\
    \ convenient to write."
    $ Sig.call ((,,)
    <$> Sig.defaulted "from" 1 "Start at this value."
    <*> Sig.defaulted "speed" 1 "Descend this amount per second."
    <*> Sig.defaulted "limit" Nothing "Stop at this value."
    ) $ \(from, speed, limit) args ->
        make_slope args limit Nothing (Just from) (-speed)

make_slope :: Derive.ControlArgs -> Maybe Signal.Y -> Maybe Signal.Y
    -> Maybe Signal.Y -> Double -> Derive.Deriver Signal.Control
make_slope args low high maybe_from slope =
    case maybe_from <|> ControlUtil.prev_val maybe_from args of
        Nothing -> return Signal.empty
        Just from -> do
            srate <- Call.get_srate
            (start, end) <- Args.real_range_or_next args
            return $ ControlUtil.limited_slope srate low high from slope
                start end

c_pedal :: Derive.Generator Derive.Control
c_pedal = generator1 "pedal" mempty
    ("Unlike most control events, this uses a duration. Set the control to\
    \ the given value for the event's duration, and reset to the old\
    \ value afterwards."
    ) $ Sig.call ((,)
    <$> Sig.defaulted "val" 1 "Set to this value."
    <*> Sig.environ "dur" Sig.Prefixed 0.05
        "Use this duration if the event duration is 0."
    ) $ \(val, dur) args -> do
        (start, end) <- Args.real_range args
        end <- return $ if start == end then end + dur else end
        let prev = maybe 0 snd $ Args.prev_control args
        return $ Signal.signal [(start, val), (end, prev)]

c_swell :: Derive.Generator Derive.Control
c_swell = generator1 "swell" mempty
    "Start at the given value, interpolate to a peak, then back to the\
    \ original value. Uses duration."
    $ Sig.call ((,,)
    <$> Sig.required "val" "Start value."
    <*> Sig.defaulted "peak" 1 "Interpolate to this value."
    <*> Sig.defaulted "bias" 0.5 "0 puts the peak at the start, 1 at the end."
    ) $ \(val, peak, bias) args -> do
        (start, end) <- Args.real_range args
        let middle = Num.clamp start end $
                Num.scale start end (RealTime.seconds bias)
        srate <- Call.get_srate
        return $ ControlUtil.breakpoints srate id
            [(start, val), (middle, peak), (end, val)]

generator1 :: Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Derive.WithArgDoc (Derive.PassedArgs d -> Derive.Deriver d)
    -> Derive.Call (Derive.GeneratorFunc d)
generator1 = Derive.generator1 Module.prelude
