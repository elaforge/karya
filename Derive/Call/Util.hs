{-# LANGUAGE ViewPatterns #-}
{- | Utilities for calls.

    The convention for calls is that there is a function @c_something@ which
    is type NoteCall or ControlCall or whatever.  It then extracts what is
    needed from the PassedArgs and passes those values to a function
    @something@ which is of type EventDeriver or ControlDeriver or whatever.
    The idea is that PassedArgs is a large dependency and it should be reduced
    immediately to what is needed.
-}
module Derive.Call.Util where
import qualified Data.FixedList as FixedList
import Data.FixedList (Nil(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Traversable as Traversable

import qualified System.Random.Mersenne.Pure64 as Pure64

import Util.Control
import qualified Util.Num as Num
import qualified Util.Parse as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Random as Random

import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


-- * signals

-- TODO There are four types that divide into two kinds.  Then I have
-- every possible combination:
-- any type: Score.Real
-- time type without value: Real
-- time type with value: TrackLang.Real
--
-- This means I wind up with a lot of duplication here to handle time types and
-- transpose types.  Surely there's a better way?  Maybe put the two kinds into
-- a typeclass?

data TransposeType = Diatonic | Chromatic deriving (Show)
data TimeType = Real | Score deriving (Eq, Show)

instance Pretty.Pretty TransposeType where pretty = show
instance Pretty.Pretty TimeType where pretty = show

split_transpose :: Pitch.Transpose -> (Double, TransposeType)
split_transpose (Pitch.Chromatic c) = (c, Chromatic)
split_transpose (Pitch.Diatonic c) = (c, Diatonic)

join_transpose :: Double -> TransposeType -> Pitch.Transpose
join_transpose c Chromatic = Pitch.Chromatic c
join_transpose c Diatonic = Pitch.Diatonic c

-- | This is just a shorthand way to look up a bunch of controls at once.
-- TODO Now that lots of controls are typed it may not be so useful, unless
-- I add type annotations to the controls list.
with_controls :: (FixedList.FixedList list) => Derive.PassedArgs d
    -> list TrackLang.ValControl -> (list Signal.Y -> Derive.Deriver a)
    -> Derive.Deriver a
with_controls args controls f = do
    now <- Args.real_start args
    f =<< Traversable.mapM (flip control_at now) controls

-- | To accomodate both normal calls, which are in score time, and post
-- processing calls, which are in real time, these functions take RealTimes.
control_at :: TrackLang.ValControl -> RealTime -> Derive.Deriver Signal.Y
control_at control pos = Score.typed_val <$> typed_control_at control pos

real_time_at :: TrackLang.ValControl -> RealTime -> Derive.Deriver RealTime
real_time_at control pos = RealTime.seconds <$> control_at control pos

typed_control_at :: TrackLang.ValControl -> RealTime
    -> Derive.Deriver Score.TypedVal
typed_control_at control pos = case control of
    TrackLang.ConstantControl deflt -> return deflt
    TrackLang.DefaultedControl cont deflt ->
        fromMaybe deflt <$> Derive.control_at cont pos
    TrackLang.LiteralControl cont ->
        maybe (Derive.throw $ "not found and no default: "
                <> untxt (TrackLang.show_val cont)) return
            =<< Derive.control_at cont pos

time_control_at :: TimeType -> TrackLang.ValControl -> RealTime
    -> Derive.Deriver TrackLang.RealOrScore
time_control_at default_type control pos = do
    Score.Typed typ val <- typed_control_at control pos
    time_type <- case typ of
        Score.Untyped -> return default_type
        Score.Score -> return Score
        Score.Real -> return Real
        _ -> Derive.throw $ "expected time type for "
            <> untxt (TrackLang.show_val control) <> " but got "
            <> Pretty.pretty typ
    return $ case time_type of
        Real -> TrackLang.Real (RealTime.seconds val)
        Score -> TrackLang.Score (ScoreTime.double val)

transpose_control_at :: TransposeType -> TrackLang.ValControl -> RealTime
    -> Derive.Deriver (Signal.Y, TransposeType)
transpose_control_at default_type control pos = do
    Score.Typed typ val <- typed_control_at control pos
    transpose_type <- case typ of
        Score.Untyped -> return default_type
        Score.Chromatic -> return Chromatic
        Score.Diatonic -> return Diatonic
        _ -> Derive.throw $ "expected transpose type for "
            <> untxt (TrackLang.show_val control) <> " but got "
            <> Pretty.pretty typ
    return (val, transpose_type)

-- | Convert a 'TrackLang.ValControl' to a signal.
--
-- If a signal exists but doesn't have a type, the type will be inherited from
-- the default.  This way a call can cause a signal parameter to default to
-- a certain type.
to_signal :: TrackLang.ValControl -> Derive.Deriver Score.TypedSignal
to_signal control = case control of
    TrackLang.ConstantControl deflt -> return $ fmap Signal.constant deflt
    TrackLang.DefaultedControl cont deflt -> do
        maybe_sig <- Derive.get_control cont
        return $ case maybe_sig of
            Nothing -> Signal.constant <$> deflt
            Just sig -> sig
                { Score.type_of = Score.type_of sig <> Score.type_of deflt }
    TrackLang.LiteralControl cont ->
        maybe (Derive.throw $ "not found: " ++ show cont) return
            =<< Derive.get_control cont

to_untyped_signal :: TrackLang.ValControl -> Derive.Deriver Signal.Control
to_untyped_signal = fmap Score.typed_val . to_signal

-- | Version of 'to_signal' specialized for transpose signals.  Throws if
-- the signal had a non-transpose type.
to_transpose_signal :: TransposeType -> TrackLang.ValControl
    -> Derive.Deriver (Signal.Control, Score.Control)
    -- ^ (signal, appropriate transpose control)
to_transpose_signal default_type control = do
    Score.Typed typ sig <- to_signal control
    case typ of
        Score.Untyped -> return (sig, case default_type of
            Diatonic -> Score.c_diatonic; Chromatic -> Score.c_chromatic)
        Score.Chromatic -> return (sig, Score.c_chromatic)
        Score.Diatonic -> return (sig, Score.c_diatonic)
        _ -> Derive.throw $ "expected transpose type for "
            <> untxt (TrackLang.show_val control) <> " but got "
            <> Pretty.pretty typ

-- | Version of 'to_signal' that will complain if the control isn't a time
-- type.
to_time_signal :: TimeType -> TrackLang.ValControl
    -> Derive.Deriver (Signal.Control, TimeType)
to_time_signal default_type control = do
    Score.Typed typ sig <- to_signal control
    case typ of
        Score.Untyped -> return (sig, default_type)
        Score.Score -> return (sig, Score)
        Score.Real -> return (sig, Real)
        _ -> Derive.throw $ "expected time type for "
            <> untxt (TrackLang.show_val control) <> " but got "
            <> Pretty.pretty typ

-- TODO maybe pos should be be ScoreTime so I can pass it to eval_pitch?
pitch_at :: RealTime -> TrackLang.PitchControl
    -> Derive.Deriver PitchSignal.Pitch
pitch_at pos control = case control of
    TrackLang.ConstantControl deflt -> Call.eval_pitch 0 deflt
    TrackLang.DefaultedControl cont deflt -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (Call.eval_pitch 0 deflt) return maybe_pitch
    TrackLang.LiteralControl cont -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (Derive.throw $ "pitch not found and no default given: "
            ++ show cont) return maybe_pitch

to_pitch_signal :: TrackLang.PitchControl -> Derive.Deriver PitchSignal.Signal
to_pitch_signal control = case control of
    TrackLang.ConstantControl deflt -> constant deflt
    TrackLang.DefaultedControl cont deflt -> do
        sig <- Derive.get_named_pitch cont
        maybe (constant deflt) return sig
    TrackLang.LiteralControl cont ->
        maybe (Derive.throw $ "not found: " ++ show cont) return
            =<< Derive.get_named_pitch cont
    where
    constant note = do
        scale <- get_scale
        constant_pitch scale <$> Call.eval_pitch 0 note

nn_at :: RealTime -> TrackLang.PitchControl
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
nn_at pos control = -- TODO throw exception?
    Derive.logged_pitch_nn ("Util.nn_at " ++ Pretty.pretty (pos, control))
        =<< pitch_at pos control

pitch_signal :: [(RealTime, PitchSignal.Pitch)]
    -> Derive.Deriver PitchSignal.Signal
pitch_signal xs = do
    scale <- get_scale
    return $ signal scale xs

-- | More convenient constructors for PitchSignals.
constant_pitch :: Scale.Scale -> PitchSignal.Pitch -> PitchSignal.Signal
constant_pitch = PitchSignal.constant . Derive.pitch_signal_scale

signal :: Scale.Scale -> [(RealTime, PitchSignal.Pitch)] -> PitchSignal.Signal
signal = PitchSignal.signal . Derive.pitch_signal_scale

-- * note

-- | Get the Pitch at the particular point in time in the default pitch
-- signal.  As per 'Derive.pitch_at', the transposition controls have not been
-- applied.
pitch :: RealTime -> Derive.Deriver (Maybe PitchSignal.Pitch)
pitch = Derive.pitch_at

dynamic :: RealTime -> Derive.Deriver Signal.Y
dynamic pos = maybe Derive.default_dynamic Score.typed_val <$>
    Derive.control_at Score.c_dynamic pos

with_pitch :: PitchSignal.Pitch -> Derive.Deriver a -> Derive.Deriver a
with_pitch pitch deriver = do
    scale <- get_scale
    Derive.with_constant_pitch Nothing scale pitch deriver

with_symbolic_pitch :: TrackLang.Note -> ScoreTime -> Derive.Deriver a
    -> Derive.Deriver a
with_symbolic_pitch note pos deriver = do
    pitch <- Call.eval_pitch pos note
    with_pitch pitch deriver

with_dynamic :: Signal.Y -> Derive.Deriver a -> Derive.Deriver a
with_dynamic =
    Derive.with_control Score.c_dynamic . Score.untyped . Signal.constant

-- | Generate a single note, from 0 to 1.
note :: Derive.EventDeriver
note = Call.eval_one_call $ TrackLang.call "" []

-- | Override the pitch signal and generate a single note.
pitched_note :: PitchSignal.Pitch -> Signal.Y -> Derive.EventDeriver
pitched_note pitch dynamic = with_pitch pitch $ with_dynamic dynamic note

-- | Add an attribute and generate a single note.
attr_note :: Score.Attributes -> Derive.EventDeriver
attr_note attrs = add_attrs attrs note

-- | A zero-duration 'note'.
triggered_note :: Derive.EventDeriver
triggered_note = Call.eval_one_at 0 0 $ TrackLang.call "" [] :| []

place :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
place = uncurry Derive.d_place . Args.extent

placed_note :: Derive.PassedArgs d -> Derive.EventDeriver
placed_note args = place args note

-- * call transformers

-- | Derive with transformed Attributes.
with_attrs :: (Score.Attributes -> Score.Attributes) -> Derive.Deriver d
    -> Derive.Deriver d
with_attrs f deriver = do
    attrs <- get_attrs
    Derive.with_val TrackLang.v_attributes (f attrs) deriver

add_attrs :: Score.Attributes -> Derive.Deriver d -> Derive.Deriver d
add_attrs attrs
    | attrs == mempty = id
    | otherwise = with_attrs (<> attrs)

-- * environ

get_srate :: Derive.Deriver RealTime
get_srate = RealTime.seconds <$> Derive.get_val TrackLang.v_srate

get_scale :: Derive.Deriver Scale.Scale
get_scale = Derive.get_scale =<< get_scale_id

lookup_scale :: Derive.Deriver (Maybe Scale.Scale)
lookup_scale = Derive.lookup_scale =<< get_scale_id

get_scale_id :: Derive.Deriver Pitch.ScaleId
get_scale_id = Derive.get_val TrackLang.v_scale

lookup_key :: Derive.Deriver (Maybe Pitch.Key)
lookup_key = fmap Pitch.Key <$> Derive.lookup_val TrackLang.v_key

lookup_instrument :: Derive.Deriver (Maybe Score.Instrument)
lookup_instrument = Derive.lookup_val TrackLang.v_instrument

get_attrs :: Derive.Deriver Score.Attributes
get_attrs = fromMaybe mempty <$> Derive.lookup_val TrackLang.v_attributes

-- * random

-- | Get an infinite list of random numbers.  These are deterministic in that
-- they depend only on the random seed, but the random seed is hashed with
-- each stack entry.  So if you fix the random seed at a certain point, you
-- should get consistent results below it.
--
-- It's a class because both Doubles and Ints are useful and I'd like to use
-- the same function name for both.
class Random a where
    -- | Infinite list of random numbers.  These are deterministic in that
    -- they depend on the current track, current call position, and the random
    -- seed.
    randoms :: Derive.Deriver [a]
    -- | Infinite list of random numbers in the given range.
    randoms_in :: a -> a -> Derive.Deriver [a]

instance Random Double where
    -- Random numbers between 0 and 1.
    randoms = _make_randoms Pure64.randomDouble
    randoms_in low high = map (Num.scale low high) <$> randoms

instance Random Int where
    -- Random numbers between INT_MIN and INT_MAX.
    randoms = _make_randoms Pure64.randomInt
    randoms_in low high = map (Num.restrict low high) <$> randoms

random :: (Random a) => Derive.Deriver a
random = head <$> randoms

random_in :: (Random a, Real a) => a -> a -> Derive.Deriver a
random_in low high = head <$> randoms_in low high

-- | If the chance is 1, return true all the time, if it's 0.5, return it half
-- of the time.
chance :: Double -> Derive.Deriver Bool
chance v
    | v >= 1 = return True
    | v <= 0 = return False
    | otherwise = do
        r <- random_in 0 1
        return $ r <= v

shuffle :: [a] -> Derive.Deriver [a]
shuffle xs = Random.shuffle xs <$> randoms

pick :: NonEmpty a -> Derive.Deriver a
pick xs = shuffle (NonEmpty.toList xs) >>= \x -> case x of
    [] -> Derive.throw "Derive.Call.Util.pick expected non-null list"
    x : _ -> return x

_make_randoms :: (Pure64.PureMT -> (a, Pure64.PureMT)) -> Derive.Deriver [a]
_make_randoms f = List.unfoldr (Just . f) <$> _random_generator

_random_generator :: Derive.Deriver Pure64.PureMT
_random_generator = do
    seed <- fromMaybe 0 <$> Derive.lookup_val TrackLang.v_seed
    return $ Pure64.pureMT (floor (seed :: Double))

-- * time

real_time :: TrackLang.RealOrScore -> Derive.Deriver RealTime
real_time (TrackLang.Score t) = Derive.real t
real_time (TrackLang.Real t) = return t

score_time :: TrackLang.RealOrScore -> Derive.Deriver ScoreTime
score_time (TrackLang.Score t) = return t
score_time (TrackLang.Real t) = Derive.score t

-- | A time range from the event start until a given duration.
duration_from_start :: Derive.PassedArgs d -> TrackLang.RealOrScore
    -> Derive.Deriver (RealTime, RealTime)
duration_from_start args duration = do
    start <- Args.real_start args
    case duration of
        TrackLang.Real t -> return (start, start + t)
        TrackLang.Score t -> (,) start <$> Derive.real (Args.start args + t)

-- | This is 'real_dur', but fancied up to take a TypedVal.
real_duration :: TimeType -> ScoreTime -> Score.TypedVal
    -> Derive.Deriver RealTime
real_duration default_type from (Score.Typed typ val)
    | typ == Score.Real || typ == Score.Untyped && default_type == Real =
        return (RealTime.seconds val)
    | typ == Score.Score || typ == Score.Untyped && default_type == Score =
        real_dur from (ScoreTime.double val)
    | otherwise = Derive.throw $ "expected time type for "
        <> untxt (TrackLang.show_val (Score.Typed typ val))

-- | Get the real duration of a typed time val at the given point of score
-- time.  RealTime is linear, so 1 second is always 1 second no matter where
-- it is, but ScoreTime will map to different amounts of RealTime depending
-- on where it is.
real_dur :: ScoreTime -> ScoreTime -> Derive.Deriver RealTime
real_dur from dur = do
    start <- Derive.real from
    end <- Derive.real (from + dur)
    return (end - start)

-- | Add a RealTime to a ScoreTime.
delay :: ScoreTime -> RealTime -> Derive.Deriver ScoreTime
delay start time = do
    real_start <- Derive.real start
    Derive.score (real_start + time)

-- | Get the amount of ScoreTime to add to a given ScoreTime to delay it
-- by a certain amount.  If the delay amount is already ScoreTime then
-- it's trivial, but if it's RealTime then the returned ScoreTime has to
-- be relative to the given start time.
duration_from :: ScoreTime -> TrackLang.RealOrScore -> Derive.Deriver ScoreTime
duration_from _ (TrackLang.Score t) = return t
duration_from start (TrackLang.Real t) = do
    score_t <- delay start t
    return $ score_t - start

-- ** timestep

-- | Take the given number of steps.  Negative means step back.
timestep :: ScoreTime -> TimeStep.TimeStep -> Int -> Derive.Deriver ScoreTime
timestep start ts steps = do
    (block_id, tracknum) <- Internal.get_current_tracknum
    Derive.require ("valid timestep from " <> untxt (ShowVal.show_val start))
        =<< Derive.eval_ui "c_timestep"
            (TimeStep.step_from steps ts block_id tracknum start)

meter_duration :: ScoreTime -> Ruler.Rank -> Int -> Derive.Deriver ScoreTime
meter_duration start rank steps = do
    let ts = TimeStep.step $ TimeStep.RelativeMark TimeStep.match_meter rank
    end <- timestep start ts steps
    return $ end - start

parsed_meter_duration :: ScoreTime -> Text -> Int -> Derive.Deriver ScoreTime
parsed_meter_duration start rank steps = do
    rank <- Derive.require_right ("parsing timestep: "++) $
        Parse.parse TimeStep.parse_rank rank
    meter_duration start rank steps

-- * c_equal

c_equal :: (Derive.Derived derived) => Derive.Call derived
c_equal = Derive.transformer "equal" Tags.prelude equal_doc
    (Sig.parsed_manually equal_arg_doc equal_transformer)

equal_arg_doc :: Text
equal_arg_doc =
    "The left hand side can be a symbol, `%control-name`, or\
    \ `#pitch-control-name`. The right hand side is anything when binding\
    \ a symbol, a number or `%control-name` when binding a `%control`, or\
    \ a pitch or `#pitch-name` when binding a `#pitch`."

equal_doc :: Text
equal_doc =
    "Evaluate the deriver with a value set. Set environ vals with `x = 42`, \
    \or set a control or pitch signal with `%c = .5` or \
    \`#pitch = (4c)`.\
    \\nA special parsing rule means that this call can be written infix."

equal_transformer :: Derive.PassedArgs derived -> Derive.Deriver a
    -> Derive.Deriver a
equal_transformer args deriver = case Derive.passed_vals args of
    [TrackLang.VSymbol assignee, val] ->
        Derive.with_val assignee val deriver
    [control -> Just assignee, TrackLang.VControl val] -> do
        sig <- to_signal val
        Derive.with_control assignee sig deriver
    [control -> Just assignee, TrackLang.VNum val] ->
        Derive.with_control assignee (fmap Signal.constant val) deriver
    [pitch -> Just assignee, TrackLang.VPitchControl val] -> do
        sig <- to_pitch_signal val
        Derive.with_pitch assignee sig deriver
    [pitch -> Just assignee, TrackLang.VPitch val] -> do
        scale <- get_scale
        Derive.with_pitch assignee (constant_pitch scale val) deriver
    _ -> Derive.throw_arg_error
        "equal call expected (sym, val) or (sig, sig) args"
    where
    control (TrackLang.VControl (TrackLang.LiteralControl c)) = Just c
    control _ = Nothing
    pitch (TrackLang.VPitchControl (TrackLang.LiteralControl c))
        | c == Score.c_null = Just Nothing
        | otherwise = Just (Just c)
    pitch _ = Nothing

-- * postproc utils

-- Functions here force a Deriver into its LEvent.LEvents and process them
-- directly, and then repackage them as a Deriver.  This can accomplish
-- concrete post-processing type effects but has the side-effect of collapsing
-- the Deriver, which will no longer respond to the environment.

-- Generators can mostly forget about LEvents and emit plain Events since
-- 'Derive.generator' applies the fmap.  Unfortunately the story is not so
-- simple for transformers.  Hopefully functions here can mostly hide LEvents
-- from transformers.

map_around :: ([Score.Event] -> Score.Event -> [Score.Event] -> Score.Event)
    -> Derive.Events -> Derive.Events
map_around f events = go [] events
    where
    go prev (event : events) = case event of
        LEvent.Log log -> LEvent.Log log : go prev events
        LEvent.Event event ->
            let out = f prev event (LEvent.events_of events)
            in LEvent.Event out : go (out : prev) events
    go _ [] = []

-- | Apply a function on the first Event of an LEvent stream.
map_first :: (a -> Derive.Deriver a) -> LEvent.LEvents a -> Derive.LogsDeriver a
map_first f events = event_head events $ \e es -> do
    e <- f e
    return $ LEvent.Event e : es

event_head :: LEvent.LEvents d
    -> (d -> LEvent.LEvents d -> Derive.Deriver (LEvent.LEvents d))
    -> Derive.LogsDeriver d
event_head [] _ = return []
event_head (log@(LEvent.Log _) : rest) f = (log:) <$> event_head rest f
event_head (LEvent.Event event : rest) f = f event rest

map_events_asc :: state
    -> (state -> Score.Event -> Derive.Deriver (state, [Score.Event]))
    -> Derive.Events -> Derive.EventDeriver
map_events_asc state f events = do
    (_, result) <- map_controls Nil state (\Nil -> f) events
    return $ Derive.merge_asc_events result

-- | Specialization of 'map_controls' where the transformation will return
-- events in ascending order.
map_controls_asc :: (FixedList.FixedList cs) => cs TrackLang.ValControl
    -> state
    -> (cs Score.TypedVal -> state -> Score.Event
        -> Derive.Deriver (state, [Score.Event]))
    -> Derive.EventDeriver -> Derive.EventDeriver
map_controls_asc controls state f deriver = do
    (_, result) <- map_controls controls state f =<< deriver
    return $ Derive.merge_asc_events result

-- | Specialization of 'map_controls_pitches' with no pitch signals.  Also,
-- the mapped function is not in Deriver since you are expected to be
-- depending only on the control values.
map_controls :: (FixedList.FixedList cs) => cs TrackLang.ValControl
    -> state
    -> (cs Score.TypedVal -> state -> Score.Event
        -> Derive.Deriver (state, [Score.Event]))
    -> Derive.Events -> Derive.Deriver (state, [Derive.Events])
map_controls controls state f =
    map_controls_pitches controls Nil state (\cs Nil -> f cs)

-- | Map a function with state over events and lookup pitch and controls vals
-- for each event.  Exceptions are caught and logged.
--
-- This is the most general transformer map over events.
map_controls_pitches :: (FixedList.FixedList cs, FixedList.FixedList ps) =>
    cs TrackLang.ValControl -> ps TrackLang.PitchControl
    -> state
    -> (cs Score.TypedVal -> ps PitchSignal.Pitch -> state -> Score.Event
        -> Derive.Deriver (state, [Score.Event]))
    -> Derive.Events -> Derive.Deriver (state, [Derive.Events])
map_controls_pitches controls pitch_controls state f events = go state events
    where
    go state [] = return (state, [])
    go state (log@(LEvent.Log _) : rest) = do
        (final_state, rest_vals) <- go state rest
        return (final_state, [log] : rest_vals)
    go state (LEvent.Event event : rest) = do
        let pos = Score.event_start event
        control_vals <- Traversable.mapM (flip typed_control_at pos) controls
        pitch_vals <- Traversable.mapM (pitch_at pos) pitch_controls
        result <- Derive.with_event event $
            f control_vals pitch_vals state event
        (final_state, rest_vals) <- go (maybe state fst result) rest
        let vals = maybe rest_vals
                ((:rest_vals) . map LEvent.Event . snd) result
        return (final_state, vals)

-- * next in track

-- | Return only the events that follow the given event on its track.
filter_next_in_track :: Score.Event -> [Score.Event] -> [Score.Event]
filter_next_in_track event = filter (next_in_track (stack event) . stack)
    where stack = Stack.to_ui . Score.event_stack

-- | Is the second stack from an event that occurs later on the same track as
-- the first?  This is more complicated than it may seem at first because the
-- second event could come from a different deriver.  So it should look like
-- @same ; same ; bid same / tid same / range higher ; *@.
next_in_track :: [Stack.UiFrame] -> [Stack.UiFrame] -> Bool
next_in_track (s1@(bid1, tid1, r1) : stack1) (s2@(bid2, tid2, r2) : stack2)
    | s1 == s2 = next_in_track stack1 stack2
    | bid1 == bid2 && tid1 == tid2 && r1 `before` r2 = True
    | otherwise = False
    where
    before (Just (s1, _)) (Just (s2, _)) = s1 < s2
    before _ _ = False
next_in_track _ _ = True
