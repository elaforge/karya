-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Utilities for calls.

    The convention for calls is that there is a function @c_something@ which
    is type NoteCall or ControlCall or whatever.  It then extracts what is
    needed from the PassedArgs and passes those values to a function
    @something@ which is of type NoteDeriver or ControlDeriver or whatever.
    The idea is that PassedArgs is a large dependency and it should be reduced
    immediately to what is needed.
-}
module Derive.Call.Util where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Random.Mersenne.Pure64 as Pure64

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Random as Random

import qualified Ui.ScoreTime as ScoreTime
import qualified Cmd.Meter as Meter
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Args as Args
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.Flags as Flags
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
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

data TransposeType = Diatonic | Chromatic | Nn deriving (Eq, Show)
data TimeType = Real | Score deriving (Eq, Show)

instance Pretty.Pretty TransposeType where prettyt = showt
instance Pretty.Pretty TimeType where prettyt = showt

transpose_control :: TransposeType -> Score.Control
transpose_control Diatonic = Controls.diatonic
transpose_control Chromatic = Controls.chromatic
transpose_control Nn = Controls.nn

-- | To accomodate both normal calls, which are in score time, and post
-- processing calls, which are in real time, these functions take RealTimes.
control_at :: TrackLang.ValControl -> RealTime -> Derive.Deriver Signal.Y
control_at control pos = Score.typed_val <$> typed_control_at control pos

typed_control_at :: TrackLang.ValControl -> RealTime
    -> Derive.Deriver Score.TypedVal
typed_control_at control pos = case control of
    TrackLang.ControlSignal sig -> return $ Signal.at pos <$> sig
    TrackLang.DefaultedControl cont deflt ->
        fromMaybe (Signal.at pos <$> deflt) <$> Derive.control_at cont pos
    TrackLang.LiteralControl cont ->
        maybe (Derive.throw $ "not found and no default: "
                <> untxt (TrackLang.show_val cont)) return
            =<< Derive.control_at cont pos

time_control_at :: TimeType -> TrackLang.ValControl -> RealTime
    -> Derive.Deriver TrackLang.Duration
time_control_at default_type control pos = do
    Score.Typed typ val <- typed_control_at control pos
    time_type <- case typ of
        Score.Untyped -> return default_type
        Score.Score -> return Score
        Score.Real -> return Real
        _ -> Derive.throw $ "expected time type for "
            <> untxt (TrackLang.show_val control) <> " but got " <> pretty typ
    return $ case time_type of
        Real -> TrackLang.Real (RealTime.seconds val)
        Score -> TrackLang.Score (ScoreTime.double val)

real_time_at :: TrackLang.ValControl -> RealTime -> Derive.Deriver RealTime
real_time_at control pos = do
    val <- time_control_at Real control pos
    case val of
        TrackLang.Real t -> return t
        TrackLang.Score t -> Derive.throw $ "expected RealTime for "
            <> untxt (TrackLang.show_val control) <> " but got "
            <> untxt (TrackLang.show_val t)

transpose_control_at :: TransposeType -> TrackLang.ValControl -> RealTime
    -> Derive.Deriver (Signal.Y, TransposeType)
transpose_control_at default_type control pos = do
    Score.Typed typ val <- typed_control_at control pos
    transpose_type <- case typ of
        Score.Untyped -> return default_type
        Score.Chromatic -> return Chromatic
        Score.Diatonic -> return Diatonic
        _ -> Derive.throw $ "expected transpose type for "
            <> untxt (TrackLang.show_val control) <> " but got " <> pretty typ
    return (val, transpose_type)


-- * function and signal

type TypedFunction = RealTime -> Score.TypedVal
type Function = RealTime -> Signal.Y

-- | Convert a 'TrackLang.ValControl' to a function.
--
-- If a signal exists but doesn't have a type, the type will be inherited from
-- the default.  This way a call can cause a signal parameter to default to
-- a certain type.
to_typed_function :: TrackLang.ValControl -> Derive.Deriver TypedFunction
to_typed_function control =
    either (return . Derive.signal_function) from_function
        =<< to_signal_or_function control
    where
    from_function f = TrackLang.call_control_function f score_control <$>
        Derive.get_control_function_dynamic
    score_control = case control of
        TrackLang.ControlSignal {} -> Controls.null
        TrackLang.DefaultedControl cont _ -> cont
        TrackLang.LiteralControl cont -> cont

to_function :: TrackLang.ValControl -> Derive.Deriver Function
to_function = fmap (Score.typed_val .) . to_typed_function

to_typed_signal :: TrackLang.ValControl -> Derive.Deriver Score.TypedControl
to_typed_signal control =
    either return (const $ Derive.throw $ "not found: " <> pretty control)
        =<< to_signal_or_function control

to_signal :: TrackLang.ValControl -> Derive.Deriver Signal.Control
to_signal = fmap Score.typed_val . to_typed_signal

to_signal_or_function :: TrackLang.ValControl
    -> Derive.Deriver (Either Score.TypedControl TrackLang.ControlFunction)
to_signal_or_function control = case control of
    TrackLang.ControlSignal sig -> return $ Left sig
    TrackLang.DefaultedControl cont deflt ->
        get_control (Score.type_of deflt) (return (Left deflt)) cont
    TrackLang.LiteralControl cont ->
        get_control Score.Untyped (Derive.throw $ "not found: " ++ show cont)
            cont
    where
    get_control default_type deflt cont = get_function cont >>= \x -> case x of
        Just f -> return $ Right $
            TrackLang.modify_control_function (inherit_type default_type .) f
        Nothing -> Derive.get_control_signal cont >>= \x -> case x of
            Just sig -> return $ Left sig
            Nothing -> deflt
    get_function cont = Internal.get_dynamic $
        Map.lookup cont . Derive.state_control_functions
    -- If the signal was untyped, it gets the type of the default, since
    -- presumably the caller expects that type.
    inherit_type default_type val =
        val { Score.type_of = Score.type_of val <> default_type }

-- | Version of 'to_function' specialized for transpose signals.  Throws if
-- the signal had a non-transpose type.
to_transpose_function :: TransposeType -> TrackLang.ValControl
    -> Derive.Deriver (Function, Score.Control)
    -- ^ (signal, appropriate transpose control)
to_transpose_function default_type control = do
    sig <- to_typed_function control
    -- Previously, I directly returned 'Score.TypedControl's so I could look at
    -- their types.  A function is more powerful but I have to actually call
    -- it to find the type.
    let typ = Score.type_of (sig 0)
        untyped = Score.typed_val . sig
    case typ of
        Score.Untyped -> return (untyped, transpose_control default_type)
        _ -> case Controls.transpose_type typ of
            Just control -> return (untyped, control)
            _ -> Derive.throw $ "expected transpose type for "
                <> untxt (TrackLang.show_val control) <> " but got "
                <> pretty typ

-- | Version of 'to_function' that will complain if the control isn't a time
-- type.
to_time_function :: TimeType -> TrackLang.ValControl
    -> Derive.Deriver (Function, TimeType)
to_time_function default_type control = do
    sig <- to_typed_function control
    let typ = Score.type_of (sig 0)
        untyped = Score.typed_val . sig
    case typ of
        Score.Untyped -> return (untyped, default_type)
        Score.Score -> return (untyped, Score)
        Score.Real -> return (untyped, Real)
        _ -> Derive.throw $ "expected time type for "
            <> untxt (TrackLang.show_val control) <> " but got "
            <> pretty typ

-- TODO maybe pos should be be ScoreTime so I can pass it to eval_pitch?
pitch_at :: RealTime -> TrackLang.PitchControl
    -> Derive.Deriver PitchSignal.Pitch
pitch_at pos control = case control of
    TrackLang.ControlSignal sig -> require sig
    TrackLang.DefaultedControl cont deflt -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (require deflt) return maybe_pitch
    TrackLang.LiteralControl cont -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (Derive.throw $ "pitch not found and no default given: "
            ++ show cont) return maybe_pitch
    where
    require = Derive.require ("ControlSignal pitch at " <> pretty pos)
        . PitchSignal.at pos

to_pitch_signal :: TrackLang.PitchControl -> Derive.Deriver PitchSignal.Signal
to_pitch_signal control = case control of
    TrackLang.ControlSignal sig -> return sig
    TrackLang.DefaultedControl cont deflt ->
        maybe (return deflt) return =<< Derive.get_named_pitch cont
    TrackLang.LiteralControl cont ->
        maybe (Derive.throw $ "not found: " ++ show cont) return
            =<< Derive.get_named_pitch cont

nn_at :: RealTime -> TrackLang.PitchControl
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
nn_at pos control = -- TODO throw exception?
    Derive.logged_pitch_nn ("Util.nn_at " <> prettyt (pos, control))
        =<< Derive.resolve_pitch pos
        =<< pitch_at pos control

-- * dynamic

-- | Unlike 'Derive.pitch_at', the transposition has already been applied.
transposed :: RealTime -> Derive.Deriver (Maybe PitchSignal.Transposed)
transposed pos =
    justm (Derive.pitch_at pos) $ fmap Just . Derive.resolve_pitch pos

get_transposed :: RealTime -> Derive.Deriver PitchSignal.Transposed
get_transposed pos = Derive.require ("no pitch at " <> pretty pos)
    =<< transposed pos

-- | Pitch without the transposition applied.  You have to use this if you
-- create an event with a pitch based on this pitch, otherwise the
-- transposition will be applied twice.
get_pitch :: RealTime -> Derive.Deriver PitchSignal.Pitch
get_pitch pos = Derive.require ("no pitch at " <> pretty pos)
    =<< Derive.pitch_at pos

get_parsed_pitch :: (Pitch.Note -> Maybe Pitch.Pitch) -> RealTime
    -> Derive.Deriver Pitch.Pitch
get_parsed_pitch parse pos = do
    pitch <- get_transposed pos
    note <- Pitches.pitch_note pitch
    Derive.require "unparseable pitch" $ parse note

dynamic :: RealTime -> Derive.Deriver Signal.Y
dynamic pos = maybe Derive.default_dynamic Score.typed_val <$>
    Derive.control_at Controls.dynamic pos

with_pitch :: PitchSignal.Pitch -> Derive.Deriver a -> Derive.Deriver a
with_pitch = Derive.with_constant_pitch Nothing

with_symbolic_pitch :: TrackLang.PitchCall -> ScoreTime -> Derive.Deriver a
    -> Derive.Deriver a
with_symbolic_pitch call pos deriver = do
    pitch <- Eval.eval_pitch pos call
    with_pitch pitch deriver

-- | Replace the dynamic with the given one.
with_dynamic :: Signal.Y -> Derive.Deriver a -> Derive.Deriver a
with_dynamic = with_constant Controls.dynamic

multiply_dynamic :: Signal.Y -> Derive.Deriver a -> Derive.Deriver a
multiply_dynamic = multiply_constant Controls.dynamic

with_constant :: Score.Control -> Signal.Y -> Derive.Deriver a
    -> Derive.Deriver a
with_constant control = Derive.with_control control . Score.untyped
    . Signal.constant

multiply_constant :: Score.Control -> Signal.Y -> Derive.Deriver a
    -> Derive.Deriver a
multiply_constant control = Derive.with_multiplied_control control
    . Score.untyped . Signal.constant

-- * environ

get_srate :: Derive.Deriver RealTime
get_srate = RealTime.seconds <$> Derive.get_val Environ.srate

get_scale :: Derive.Deriver Scale.Scale
get_scale = Derive.get_scale =<< get_scale_id

lookup_scale :: Derive.Deriver (Maybe Scale.Scale)
lookup_scale = Derive.lookup_scale =<< get_scale_id

get_scale_id :: Derive.Deriver Pitch.ScaleId
get_scale_id = TrackLang.sym_to_scale_id <$> Derive.get_val Environ.scale

lookup_key :: Derive.Deriver (Maybe Pitch.Key)
lookup_key = fmap Pitch.Key <$> Derive.lookup_val Environ.key

get_instrument :: Derive.Deriver Score.Instrument
get_instrument = Derive.get_val Environ.instrument

lookup_instrument :: Derive.Deriver (Maybe Score.Instrument)
lookup_instrument = Derive.lookup_val Environ.instrument

get_attrs :: Derive.Deriver Score.Attributes
get_attrs = fromMaybe mempty <$> Derive.lookup_val Environ.attributes

-- | Get symbolic pitch manipulating functions for the current scale.  This
-- is for calls that want to work with symbolic pitches.
get_pitch_functions :: Derive.Deriver
    ( Pitch.Note -> Maybe Pitch.Pitch
    , Pitch.Pitch -> Maybe Pitch.Note
    , Scale.Transposition -> Pitch.Step -> Pitch.Pitch -> Maybe Pitch.Pitch
    )
get_pitch_functions = do
    scale <- get_scale
    env <- Internal.get_environ
    let transpose transposition steps =
            to_maybe . Scale.scale_transpose scale transposition env steps
    return
        ( to_maybe . Scale.scale_read scale env
        , to_maybe . Scale.scale_show scale env
        , transpose
        )
    where to_maybe = either (const Nothing) Just

-- * note

eval_note :: ScoreTime -> Pitch.Note -> Derive.Deriver PitchSignal.Pitch
eval_note pos note = Eval.eval_pitch pos $
    TrackLang.call (TrackLang.Symbol (Pitch.note_text note)) []

-- | Generate a single note, from 0 to 1.
note :: Derive.NoteDeriver
note = Eval.eval_one_call True $ TrackLang.call "" []

-- | Like 'note', but the note reuses the start and duration from the passed
-- args, rather than being normalized from 0 to 1.  This is appropriate when
-- dispatching to the default note call.
note_here :: Derive.NoteArgs -> Derive.NoteDeriver
note_here args = Eval.reapply_call (Args.info args) "" []

-- | Override the pitch signal and generate a single note.
pitched_note :: PitchSignal.Pitch -> Derive.NoteDeriver
pitched_note pitch = with_pitch pitch note

-- | Add an attribute and generate a single note.
attr_note :: Score.Attributes -> Derive.NoteDeriver
attr_note attrs = add_attrs attrs note

-- | A zero-duration 'note'.
triggered_note :: Derive.NoteDeriver
triggered_note = Eval.eval_one_at True 0 0 $ TrackLang.call "" [] :| []

place :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
place = uncurry Derive.place . Args.extent

placed_note :: Derive.PassedArgs d -> Derive.NoteDeriver
placed_note args = place args note

-- * transformer notes

-- | Derive with transformed Attributes.
with_attrs :: (Score.Attributes -> Score.Attributes) -> Derive.Deriver d
    -> Derive.Deriver d
with_attrs f deriver = do
    attrs <- get_attrs
    Derive.with_val Environ.attributes (f attrs) deriver

add_attrs :: Score.Attributes -> Derive.Deriver d -> Derive.Deriver d
add_attrs attrs
    | attrs == mempty = id
    | otherwise = with_attrs (<> attrs)

add_flags :: Flags.Flags -> Derive.NoteDeriver -> Derive.NoteDeriver
add_flags flags = fmap (fmap (fmap (Score.add_flags flags)))

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
    -- | Random numbers in the range [0, 1).
    randoms = _make_randoms Pure64.randomDouble
        -- Pure64.randomDouble doesn't document the range, but that's what it
        -- is.
    randoms_in low high = map (Num.scale low high) <$> randoms

instance Random Int where
    -- Random numbers between INT_MIN and INT_MAX.
    randoms = _make_randoms Pure64.randomInt
    randoms_in low high = map (Num.restrict low high) <$> randoms

random :: (Random a) => Derive.Deriver a
random = head <$> randoms

random_in :: (Random a, Real a) => a -> a -> Derive.Deriver a
random_in low high
    | low == high = return low
    | otherwise = head <$> randoms_in low high

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

_make_randoms :: (Pure64.PureMT -> (a, Pure64.PureMT)) -> Derive.Deriver [a]
_make_randoms f = List.unfoldr (Just . f) <$> _random_generator

_random_generator :: Derive.Deriver Pure64.PureMT
_random_generator = do
    seed <- fromMaybe 0 <$> Derive.lookup_val Environ.seed
    return $ Pure64.pureMT (floor (seed :: Double))

-- * time

-- | Get the real duration of time val at the given point in time.  RealTime is
-- linear, so 1 second is always 1 second no matter where it is, but ScoreTime
-- will map to different amounts of RealTime depending on where it is.
real_duration :: (Derive.Time t1, Derive.Time t2) => t1 -> t2
    -> Derive.Deriver RealTime
real_duration start dur = case Derive.to_duration dur of
    TrackLang.Real t -> return t
    TrackLang.Score t
        | t == 0 -> return 0
        | otherwise -> do
            -- I'm adding score to real, so I want the amount of real time in
            -- the future I am if I advance the given amount of score time from
            -- 'start'.
            score_start <- Derive.score start
            real_start <- Derive.real start
            end <- Derive.real $ score_start + t
            return $ end - real_start

-- | Like 'real_duration', but get the duration in ScoreTime.  If you are
-- manipulating deriver abstractly instead of directly emitting events then you
-- will place them via 'Derive.d_at' and family, which are in ScoreTime.
score_duration :: (Derive.Time t1, Derive.Time t2) => t1 -> t2
    -> Derive.Deriver ScoreTime
score_duration start dur = case Derive.to_duration dur of
    TrackLang.Score t -> return t
    TrackLang.Real t
        | t == 0 -> return 0
        | otherwise -> do
            -- I'm adding real to score, so I want the amount of amount of
            -- score time I'd have to advance in order for the given amount
            -- of real time to pass.
            score_start <- Derive.score start
            real_start <- Derive.real start
            end <- Derive.score $ real_start + t
            return $ end - score_start

-- | A time range from the event start until a given duration.
duration_from_start :: Derive.Time t => Derive.PassedArgs d -> t
    -> Derive.Deriver (RealTime, RealTime) -- ^ (start, start+dur)
duration_from_start args t = do
    start <- Args.real_start args
    dur <- real_duration start t
    return (start, start + dur)

-- | Like 'duration_from_start', but subtract a duration from the end.
duration_from_end :: Derive.Time t => Derive.PassedArgs d -> t
    -> Derive.Deriver (RealTime, RealTime) -- ^ (end-dur, end)
duration_from_end args t = do
    end <- Args.real_end args
    dur <- real_duration end t
    return (end - dur, end)

-- | This is 'real_duration', but takes a TypedVal.
typed_real_duration :: TimeType -> ScoreTime -> Score.TypedVal
    -> Derive.Deriver RealTime
typed_real_duration default_type from (Score.Typed typ val)
    | typ == Score.Real || typ == Score.Untyped && default_type == Real =
        return (RealTime.seconds val)
    | typ == Score.Score || typ == Score.Untyped && default_type == Score =
        real_duration from (ScoreTime.double val)
    | otherwise = Derive.throw $ "expected time type for "
        <> untxt (TrackLang.show_val (Score.Typed typ val))

-- ** timestep

-- | Take the given number of steps.  Negative means step back.
timestep :: ScoreTime -> TimeStep.TimeStep
    -> [Int] -- ^ pick the first steps that return Just
    -> Derive.Deriver ScoreTime
timestep start ts steps = do
    (block_id, tracknum) <- Internal.get_current_tracknum
    Derive.require ("no valid timestep from " <> untxt (ShowVal.show_val start))
        =<< Derive.eval_ui "c_timestep"
            (firstJusts [TimeStep.step_from step ts block_id tracknum start |
                step <- steps])

-- | Get the timestep duration from the given point.  This tries first to
-- step forward, and then back.  This is because typically you use this to
-- configure duration for a call, and it's confusing when the call stops
-- working at the end of the block.
meter_duration :: ScoreTime -> Meter.RankName -> Double
    -> Derive.Deriver ScoreTime
meter_duration start rank multiply = do
    let ts = TimeStep.time_step $
            TimeStep.RelativeMark TimeStep.match_meter
                (Meter.name_to_rank rank)
    end <- timestep start ts [1, -1]
    return $ abs (end - start) * ScoreTime.double multiply

default_timestep :: Derive.PassedArgs a -> Meter.RankName -> Maybe ScoreTime
    -> Derive.Deriver ScoreTime
default_timestep args step =
    maybe (meter_duration (Args.start args) step 1) return

instance ShowVal.ShowVal Meter.RankName where
    show_val = TrackLang.default_show_val
instance TrackLang.Typecheck Meter.RankName
instance TrackLang.TypecheckSymbol Meter.RankName
