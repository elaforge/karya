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
module Derive.Call where
import qualified Data.List as List
import qualified System.Random.Mersenne.Pure64 as Pure64

import qualified Util.Num as Num
import qualified Util.Random as Random
import qualified Ui.ScoreTime as ScoreTime
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- * signals

-- | To accomodate both normal calls, which are in score time, and post
-- processing calls, which are in real time, these functions take RealTimes.
control_at :: BaseTypes.ControlRef -> RealTime -> Derive.Deriver Signal.Y
control_at control pos = Score.typed_val <$> typed_control_at control pos

typed_control_at :: BaseTypes.ControlRef -> RealTime
    -> Derive.Deriver Score.TypedVal
typed_control_at control pos = case control of
    BaseTypes.ControlSignal sig -> return $ Signal.at pos <$> sig
    BaseTypes.DefaultedControl cont deflt ->
        fromMaybe (Signal.at pos <$> deflt) <$> Derive.control_at cont pos
    BaseTypes.LiteralControl cont ->
        Derive.require ("not found and no default: " <> ShowVal.show_val cont)
            =<< Derive.control_at cont pos

-- TODO callers should use Typecheck.DefaultRealTimeFunction
time_control_at :: Typecheck.TimeType -> BaseTypes.ControlRef -> RealTime
    -> Derive.Deriver BaseTypes.Duration
time_control_at default_type control pos = do
    Score.Typed typ val <- typed_control_at control pos
    time_type <- case typ of
        Score.Untyped -> return default_type
        Score.Score -> return Typecheck.Score
        Score.Real -> return Typecheck.Real
        _ -> Derive.throw $ "expected time type for "
            <> ShowVal.show_val control <> " but got " <> pretty typ
    return $ case time_type of
        Typecheck.Real -> BaseTypes.RealDuration (RealTime.seconds val)
        Typecheck.Score -> BaseTypes.ScoreDuration (ScoreTime.double val)

real_time_at :: BaseTypes.ControlRef -> RealTime -> Derive.Deriver RealTime
real_time_at control pos = do
    val <- time_control_at Typecheck.Real control pos
    case val of
        BaseTypes.RealDuration t -> return t
        BaseTypes.ScoreDuration t -> Derive.throw $ "expected RealTime for "
            <> ShowVal.show_val control <> " but got " <> ShowVal.show_val t

transpose_control_at :: Typecheck.TransposeType -> BaseTypes.ControlRef
    -> RealTime -> Derive.Deriver (Signal.Y, Typecheck.TransposeType)
transpose_control_at default_type control pos = do
    Score.Typed typ val <- typed_control_at control pos
    transpose_type <- case typ of
        Score.Untyped -> return default_type
        Score.Chromatic -> return Typecheck.Chromatic
        Score.Diatonic -> return Typecheck.Diatonic
        _ -> Derive.throw $ "expected transpose type for "
            <> ShowVal.show_val control <> " but got " <> pretty typ
    return (val, transpose_type)


-- * function and signal

to_function :: BaseTypes.ControlRef -> Derive.Deriver Typecheck.Function
to_function = fmap (Score.typed_val .) . Typecheck.to_typed_function

-- | Convert a ControlRef to a control signal.  If there is
-- a 'BaseTypes.ControlFunction' it will be ignored.
to_typed_signal :: BaseTypes.ControlRef -> Derive.Deriver Score.TypedControl
to_typed_signal control =
    either return (const $ Derive.throw $ "not found: " <> pretty control)
        =<< Typecheck.to_signal_or_function control

to_signal :: BaseTypes.ControlRef -> Derive.Deriver Signal.Control
to_signal = fmap Score.typed_val . to_typed_signal

-- | Version of 'to_function' specialized for transpose signals.  Throws if
-- the signal had a non-transpose type.
to_transpose_function :: Typecheck.TransposeType -> BaseTypes.ControlRef
    -> Derive.Deriver (Typecheck.Function, Score.Control)
    -- ^ (signal, appropriate transpose control)
to_transpose_function default_type control = do
    sig <- Typecheck.to_typed_function control
    -- Previously, I directly returned 'Score.TypedControl's so I could look at
    -- their types.  A function is more powerful but I have to actually call
    -- it to find the type.
    let typ = Score.type_of (sig 0)
        untyped = Score.typed_val . sig
    case typ of
        Score.Untyped ->
            return (untyped, Typecheck.transpose_control default_type)
        _ -> case Controls.transpose_type typ of
            Just control -> return (untyped, control)
            _ -> Derive.throw $ "expected transpose type for "
                <> ShowVal.show_val control <> " but got " <> pretty typ

-- | Version of 'to_function' that will complain if the control isn't a time
-- type.
to_time_function :: Typecheck.TimeType -> BaseTypes.ControlRef
    -> Derive.Deriver (Typecheck.Function, Typecheck.TimeType)
to_time_function default_type control = do
    sig <- Typecheck.to_typed_function control
    let typ = Score.type_of (sig 0)
        untyped = Score.typed_val . sig
    case typ of
        Score.Untyped -> return (untyped, default_type)
        Score.Score -> return (untyped, Typecheck.Score)
        Score.Real -> return (untyped, Typecheck.Real)
        _ -> Derive.throw $ "expected time type for "
            <> ShowVal.show_val control <> " but got " <> pretty typ

-- TODO maybe pos should be be ScoreTime so I can pass it to eval_pitch?
pitch_at :: RealTime -> BaseTypes.PControlRef -> Derive.Deriver PSignal.Pitch
pitch_at pos control = case control of
    BaseTypes.ControlSignal sig -> require sig
    BaseTypes.DefaultedControl cont deflt -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (require deflt) return maybe_pitch
    BaseTypes.LiteralControl cont -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        Derive.require ("pitch not found and no default given: " <> showt cont)
            maybe_pitch
    where
    require = Derive.require ("ControlSignal pitch at " <> pretty pos)
        . PSignal.at pos

to_psignal :: BaseTypes.PControlRef -> Derive.Deriver PSignal.PSignal
to_psignal control = case control of
    BaseTypes.ControlSignal sig -> return sig
    BaseTypes.DefaultedControl cont deflt ->
        maybe (return deflt) return =<< Derive.get_named_pitch cont
    BaseTypes.LiteralControl cont ->
        Derive.require ("not found: " <> showt cont)
            =<< Derive.get_named_pitch cont

nn_at :: RealTime -> BaseTypes.PControlRef
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
nn_at pos control = -- TODO throw exception?
    Derive.logged_pitch_nn ("Util.nn_at " <> pretty (pos, control))
        =<< Derive.resolve_pitch pos
        =<< pitch_at pos control

-- * dynamic

-- | Unlike 'Derive.pitch_at', the transposition has already been applied.
transposed :: RealTime -> Derive.Deriver (Maybe PSignal.Transposed)
transposed pos =
    justm (Derive.pitch_at pos) $ fmap Just . Derive.resolve_pitch pos

get_transposed :: RealTime -> Derive.Deriver PSignal.Transposed
get_transposed pos = Derive.require ("no pitch at " <> pretty pos)
    =<< transposed pos

-- | Pitch without the transposition applied.  You have to use this if you
-- create an event with a pitch based on this pitch, otherwise the
-- transposition will be applied twice.
get_pitch :: RealTime -> Derive.Deriver PSignal.Pitch
get_pitch pos = Derive.require ("no pitch at " <> pretty pos)
    =<< Derive.pitch_at pos

-- | Get the symbolic version of the transposed pitch.  Since it's transposed,
-- if you turn it back to a 'PSignal.Pitch', you should use
-- 'with_transposed_pitch'.
get_parsed_pitch :: (Pitch.Note -> Maybe Pitch.Pitch)
    -- ^ Parse pitch function, as returned by 'get_pitch_functions'.
    -- It's passed separately to avoid the overhead of calling
    -- get_pitch_functions multiple times.
    -> RealTime -> Derive.Deriver Pitch.Pitch
get_parsed_pitch parse = parse_pitch parse <=< get_transposed

dynamic :: RealTime -> Derive.Deriver Signal.Y
dynamic pos = maybe Derive.default_dynamic Score.typed_val <$>
    Derive.control_at Controls.dynamic pos

with_pitch :: PSignal.Pitch -> Derive.Deriver a -> Derive.Deriver a
with_pitch = Derive.with_constant_pitch

with_transposed_pitch :: PSignal.Transposed -> Derive.Deriver a
    -> Derive.Deriver a
with_transposed_pitch pitch =
    without_transpose . with_pitch (PSignal.coerce pitch)

without_transpose :: Derive.Deriver a -> Derive.Deriver a
without_transpose = Derive.remove_controls Controls.transposers

with_symbolic_pitch :: BaseTypes.PitchCall -> ScoreTime -> Derive.Deriver a
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

add_control, multiply_control :: Score.Control -> Score.TypedControl
    -> Derive.Deriver a -> Derive.Deriver a
add_control = Derive.with_merged_control Derive.merge_add
multiply_control = Derive.with_merged_control Derive.merge_mul

add_constant, multiply_constant :: Score.Control -> Signal.Y
    -> Derive.Deriver a -> Derive.Deriver a
multiply_constant control val
    | val == 1 = id
    | otherwise = Derive.with_merged_control Derive.merge_mul control
        (Score.untyped (Signal.constant val))
add_constant control val
    | val == 0 = id
    | otherwise = Derive.with_merged_control Derive.merge_add control
        (Score.untyped (Signal.constant val))

-- * environ

get_srate :: Derive.Deriver RealTime
get_srate = RealTime.seconds <$> Derive.get_val EnvKey.srate

get_scale :: Derive.Deriver Scale.Scale
get_scale = Derive.get_scale =<< get_scale_id

lookup_scale :: Derive.Deriver (Maybe Scale.Scale)
lookup_scale = Derive.lookup_scale =<< get_scale_id

get_scale_id :: Derive.Deriver Pitch.ScaleId
get_scale_id = Expr.str_to_scale_id <$> Derive.get_val EnvKey.scale

lookup_key :: Derive.Deriver (Maybe Pitch.Key)
lookup_key = fmap Pitch.Key <$> Derive.lookup_val EnvKey.key

get_instrument :: Derive.Deriver Score.Instrument
get_instrument = Derive.get_val EnvKey.instrument

lookup_instrument :: Derive.Deriver (Maybe Score.Instrument)
lookup_instrument = Derive.lookup_val EnvKey.instrument

get_attributes :: Derive.Deriver Attrs.Attributes
get_attributes = fromMaybe mempty <$> Derive.lookup_val EnvKey.attributes

-- * parsing pitches

-- | Get symbolic pitch manipulating functions for the current scale.  This
-- is for calls that want to work with symbolic pitches.
get_pitch_functions :: Derive.Deriver
    ( Pitch.Note -> Maybe Pitch.Pitch
    , Pitch.Pitch -> Maybe Pitch.Note
    , Scale.Transposition -> Pitch.Step -> Pitch.Pitch -> Maybe Pitch.Pitch
    )
get_pitch_functions = do
    scale <- get_scale
    env <- Derive.get_environ
    let transpose transposition steps =
            to_maybe . Scale.scale_transpose scale transposition env steps
    return
        ( to_maybe . Scale.scale_read scale env
        , to_maybe . Scale.scale_show scale env
        , transpose
        )
    where to_maybe = either (const Nothing) Just

parse_pitch :: (Pitch.Note -> Maybe a) -> PSignal.Transposed
    -> Derive.Deriver a
parse_pitch parse pitch = do
    note <- Pitches.pitch_note pitch
    Derive.require "unparseable pitch" $ parse note

chromatic_difference :: PSignal.Transposed -> PSignal.Transposed
    -> Derive.Deriver Pitch.Semi
chromatic_difference = pitch_difference Scale.chromatic_difference

diatonic_difference :: PSignal.Transposed -> PSignal.Transposed
    -> Derive.Deriver Pitch.PitchClass
diatonic_difference = pitch_difference Scale.diatonic_difference

pitch_difference :: (Scale.Layout -> Pitch.Pitch -> Pitch.Pitch -> a)
    -> PSignal.Transposed -> PSignal.Transposed -> Derive.Deriver a
pitch_difference difference p1 p2 = do
    scale <- get_scale
    env <- Derive.get_environ
    let parse scale env = Scale.scale_read scale env <=< PSignal.pitch_note
    let msg = pretty p1 <> " - " <> pretty p2 <> ": "
    Derive.require_right ((msg<>) . pretty) $
        difference (Scale.scale_layout scale) <$>
            parse scale env p1 <*> parse scale env p2

nn_difference :: RealTime -> PSignal.Pitch -> PSignal.Pitch
    -> Derive.Deriver Pitch.NoteNumber
nn_difference pos pitch1 pitch2 = do
    pitch1 <- Derive.resolve_pitch pos pitch1
    pitch2 <- Derive.resolve_pitch pos pitch2
    (-) <$> Pitches.pitch_nn pitch1 <*> Pitches.pitch_nn pitch2

-- * note

eval_pitch_ :: ScoreTime -> Pitch.Pitch -> Derive.Deriver PSignal.Transposed
eval_pitch_ start pitch = do
    (_, show_pitch, _) <- get_pitch_functions
    eval_pitch show_pitch start pitch

-- | Evaluate a 'Pitch.Pitch'.  It returns a transposed pitch since
-- a 'Pitch.Pitch' is assumed to have been transposed (e.g. 'get_parsed_pitch'
-- uses a transposed pitch so range calculation works).
eval_pitch :: (Pitch.Pitch -> Maybe Pitch.Note) -> ScoreTime -> Pitch.Pitch
    -> Derive.Deriver PSignal.Transposed
eval_pitch show_pitch start pitch = do
    note <- Derive.require ("scale doesn't have pitch: " <> pretty pitch)
        (show_pitch pitch)
    eval_note start note

-- | Evaluate a symbolic pitch.  Like 'eval_pitch', I assume the Note was
-- Transposed, or at least should be an absolute pitch.
eval_note :: ScoreTime -> Pitch.Note -> Derive.Deriver PSignal.Transposed
eval_note pos note = Eval.eval_pitch pos $
    Expr.call0 (Expr.Symbol (Pitch.note_text note))

-- | Generate a single note, from 0 to 1.
note :: Derive.NoteDeriver
note = Eval.eval_one_call True $ Expr.call0 ""

-- | Like 'note', but the note reuses the Context, which means it will inherit
-- the caller's start and duration as well as sub-tracks and thus may apply
-- inversion.
--
-- This is appropriate when adding a wrapper around the default note call, but
-- not if you don't want to be overridden by sub-tracks.  See 'placed_note'
-- if you want to inherit the time, but not the rest.
reapply_note :: Derive.NoteArgs -> Derive.NoteDeriver
reapply_note args = Eval.reapply_call (Args.context args) "" []

-- | Override the pitch signal and generate a single note.
pitched_note :: PSignal.Pitch -> Derive.NoteDeriver
pitched_note pitch = with_pitch pitch note

transposed_pitched_note :: PSignal.Transposed -> Derive.NoteDeriver
transposed_pitched_note pitch = with_transposed_pitch pitch note

-- | Add an attribute and generate a single note.
attribute_note :: Attrs.Attributes -> Derive.NoteDeriver
attribute_note attrs = add_attributes attrs note

-- | A zero-duration 'note'.
triggered_note :: Derive.NoteDeriver
triggered_note = Eval.eval_one_at True 0 0 $ Expr.generator0 ""

place :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
place = uncurry Derive.place . Args.extent

placed_note :: Derive.PassedArgs d -> Derive.NoteDeriver
placed_note args = place args note

-- * transformer notes

-- | Derive with transformed Attributes.
with_attributes :: (Attrs.Attributes -> Attrs.Attributes) -> Derive.Deriver d
    -> Derive.Deriver d
with_attributes f deriver = do
    attrs <- get_attributes
    Derive.with_val EnvKey.attributes (f attrs) deriver

add_attributes :: Attrs.Attributes -> Derive.Deriver d -> Derive.Deriver d
add_attributes attrs
    | attrs == mempty = id
    | otherwise = with_attributes (<> attrs)

add_flags :: Flags.Flags -> Derive.NoteDeriver -> Derive.NoteDeriver
add_flags flags
    | flags == mempty = id
    | otherwise = fmap (fmap (Score.add_flags flags))

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

random :: Random a => Derive.Deriver a
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
    seed <- fromMaybe 0 <$> Derive.lookup_val EnvKey.seed
    return $ Pure64.pureMT (floor (seed :: Double))

pick_weighted :: NonEmpty (Double, a) -> Double -> a
pick_weighted weights rnd_ = go 0 weights
    where
    rnd = rnd_ * sum (fmap fst weights)
    go collect ((weight, a) :| weights) = case weights of
        [] -> a
        w : ws
            | collect + weight > rnd -> a
            | otherwise -> go (collect + weight) (w :| ws)

-- | Like 'pick_weighted' when all the weights are equal.
pick :: NonEmpty a -> Double -> a
pick (x :| xs) rnd = (x:xs) !! i
    where i = round (rnd * fromIntegral (length xs))

-- * time

-- | Get the real duration of time val at the given point in time.  RealTime is
-- linear, so 1 second is always 1 second no matter where it is, but ScoreTime
-- will map to different amounts of RealTime depending on where it is.
real_duration :: (Derive.Time t1, Derive.Time t2) => t1 -> t2
    -> Derive.Deriver RealTime
real_duration start dur = case Derive.to_duration dur of
    BaseTypes.RealDuration t -> return t
    BaseTypes.ScoreDuration t
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
-- will place them via 'Derive.at' and family, which are in ScoreTime.
score_duration :: (Derive.Time t1, Derive.Time t2) => t1 -> t2
    -> Derive.Deriver ScoreTime
score_duration start dur = case Derive.to_duration dur of
    BaseTypes.ScoreDuration t -> return t
    BaseTypes.RealDuration t
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
typed_real_duration :: Typecheck.TimeType -> ScoreTime -> Score.TypedVal
    -> Derive.Deriver RealTime
typed_real_duration default_type from (Score.Typed typ val)
    | typ == Score.Real || typ == Score.Untyped
        && default_type == Typecheck.Real =
            return (RealTime.seconds val)
    | typ == Score.Score || typ == Score.Untyped
        && default_type == Typecheck.Score =
            real_duration from (ScoreTime.double val)
    | otherwise = Derive.throw $
        "expected time type for " <> ShowVal.show_val (Score.Typed typ val)

-- ** timestep

-- | Take the given number of steps.  Negative means step back.
timestep :: ScoreTime -> TimeStep.TimeStep
    -> [Int] -- ^ pick the first steps that return Just
    -> Derive.Deriver ScoreTime
timestep start ts steps = do
    (block_id, tracknum) <- Internal.get_current_tracknum
    Derive.require ("no valid timestep from " <> ShowVal.show_val start)
        =<< Derive.eval_ui "c_timestep"
            (firstJusts [TimeStep.step_from step ts block_id tracknum start |
                step <- steps])

-- | Get the timestep duration from the given point.  This tries first to
-- step forward, and then back.  This is because typically you use this to
-- configure duration for a call, and it's confusing when the call stops
-- working at the end of the block.
meter_duration :: ScoreTime -> Meter.RankName -> Int
    -> Derive.Deriver ScoreTime
meter_duration start rank steps = do
    let ts = TimeStep.time_step $
            TimeStep.RelativeMark TimeStep.match_meter
                (Meter.name_to_rank rank)
    end <- timestep start ts (map (*steps) [1, -1])
    return $ abs (end - start)

-- | Duration of a single timestep, starting here.
timestep_duration :: Derive.PassedArgs a -> Meter.RankName
    -> Derive.Deriver ScoreTime
timestep_duration args step = meter_duration (Args.start args) step 1


-- * general purpose types

-- | This is for arguments which can be high or low.
data UpDown = Up | Down deriving (Show, Enum, Bounded, Eq, Ord)

instance Pretty UpDown where pretty = showt
instance Typecheck.Typecheck UpDown
instance Typecheck.TypecheckSymbol UpDown
instance ShowVal.ShowVal UpDown where
    show_val Up = "u"
    show_val Down = "d"
