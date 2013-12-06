-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified System.Random.Mersenne.Pure64 as Pure64

import Util.Control
import qualified Util.Num as Num
import qualified Util.Parse as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Random as Random
import qualified Util.Seq as Seq

import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Tempo as Tempo
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

data TransposeType = Diatonic | Chromatic | Nn deriving (Eq, Show)
data TimeType = Real | Score deriving (Eq, Show)

instance Pretty.Pretty TransposeType where pretty = show
instance Pretty.Pretty TimeType where pretty = show

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
            <> untxt (TrackLang.show_val control) <> " but got "
            <> Pretty.pretty typ
    return (val, transpose_type)

-- | Convert a 'TrackLang.ValControl' to a signal.
--
-- If a signal exists but doesn't have a type, the type will be inherited from
-- the default.  This way a call can cause a signal parameter to default to
-- a certain type.
to_signal :: TrackLang.ValControl -> Derive.Deriver Score.TypedControl
to_signal control = case control of
    TrackLang.ControlSignal sig -> return sig
    TrackLang.DefaultedControl cont deflt -> do
        maybe_sig <- Derive.get_control cont
        return $ case maybe_sig of
            Nothing -> deflt
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
        Score.Untyped -> return (sig, transpose_control default_type)
        _ -> case Controls.transpose_type typ of
            Just control -> return (sig, control)
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
    TrackLang.ControlSignal sig -> require sig
    TrackLang.DefaultedControl cont deflt -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (require deflt) return maybe_pitch
    TrackLang.LiteralControl cont -> do
        maybe_pitch <- Derive.named_pitch_at cont pos
        maybe (Derive.throw $ "pitch not found and no default given: "
            ++ show cont) return maybe_pitch
    where
    require = Derive.require ("ControlSignal pitch at " <> Pretty.pretty pos)
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
    Derive.logged_pitch_nn ("Util.nn_at " ++ Pretty.pretty (pos, control))
        =<< pitch_at pos control

-- * note

-- | Get the Pitch at the particular point in time in the default pitch
-- signal.  As per 'Derive.pitch_at', the transposition controls have not been
-- applied.
pitch :: RealTime -> Derive.Deriver (Maybe PitchSignal.Pitch)
pitch = Derive.pitch_at

get_pitch :: RealTime -> Derive.Deriver PitchSignal.Pitch
get_pitch pos = Derive.require ("pitch at " ++ Pretty.pretty pos)
    =<< Derive.pitch_at pos

dynamic :: RealTime -> Derive.Deriver Signal.Y
dynamic pos = maybe Derive.default_dynamic Score.typed_val <$>
    Derive.control_at Controls.dynamic pos

with_pitch :: PitchSignal.Pitch -> Derive.Deriver a -> Derive.Deriver a
with_pitch = Derive.with_constant_pitch Nothing

with_symbolic_pitch :: TrackLang.PitchCall -> ScoreTime -> Derive.Deriver a
    -> Derive.Deriver a
with_symbolic_pitch call pos deriver = do
    pitch <- Call.eval_pitch pos call
    with_pitch pitch deriver

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

-- | Generate a single note, from 0 to 1.
note :: Derive.NoteDeriver
note = Call.eval_one_call $ TrackLang.call "" []

-- | Override the pitch signal and generate a single note.
pitched_note :: PitchSignal.Pitch -> Signal.Y -> Derive.NoteDeriver
pitched_note pitch dynamic = with_pitch pitch $ with_dynamic dynamic note

-- | Add an attribute and generate a single note.
attr_note :: Score.Attributes -> Derive.NoteDeriver
attr_note attrs = add_attrs attrs note

-- | A zero-duration 'note'.
triggered_note :: Derive.NoteDeriver
triggered_note = Call.eval_one_at 0 0 $ TrackLang.call "" [] :| []

place :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
place = uncurry Derive.d_place . Args.extent

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

pick :: NonEmpty a -> Derive.Deriver a
pick xs = shuffle (NonEmpty.toList xs) >>= \x -> case x of
    [] -> Derive.throw "Derive.Call.Util.pick expected non-null list"
    x : _ -> return x

_make_randoms :: (Pure64.PureMT -> (a, Pure64.PureMT)) -> Derive.Deriver [a]
_make_randoms f = List.unfoldr (Just . f) <$> _random_generator

_random_generator :: Derive.Deriver Pure64.PureMT
_random_generator = do
    seed <- fromMaybe 0 <$> Derive.lookup_val Environ.seed
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
    -> Derive.Deriver (RealTime, RealTime) -- ^ (start, start + dur)
duration_from_start args duration = do
    start <- Args.real_start args
    case duration of
        TrackLang.Real t -> return (start, start + t)
        TrackLang.Score t -> (,) start <$> Derive.real (Args.start args + t)

-- | Like 'duration_from_start', but subtract a duration from the end.
duration_from_end :: Derive.PassedArgs d -> TrackLang.RealOrScore
    -> Derive.Deriver (RealTime, RealTime) -- ^ (end - dur, end)
duration_from_end args duration = do
    end <- Args.real_end args
    case duration of
        TrackLang.Real t -> return (end - t, end)
        TrackLang.Score t ->
            (,) <$> Derive.real (Args.end args - t) <*> return end

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

-- | Like 'real_dur', but take a RealOrScore.
-- TODO Ugh.  Surely there's a more organized way to go about this.
real_dur' :: ScoreTime -> TrackLang.RealOrScore -> Derive.Deriver RealTime
real_dur' _ (TrackLang.Real t) = return t
real_dur' from (TrackLang.Score t)
    | t == 0 = return 0
    | otherwise = real_dur from t

-- | Add a RealTime to a ScoreTime.
delay :: ScoreTime -> RealTime -> Derive.Deriver ScoreTime
delay start time
    | time == 0 = return start
    | otherwise = Derive.score . (+time) =<< Derive.real start

-- | Get the amount of ScoreTime to add to a given ScoreTime to delay it
-- by a certain amount.  If the delay amount is already ScoreTime then
-- it's trivial, but if it's RealTime then the returned ScoreTime has to
-- be relative to the given start time.
--
-- This is the ScoreTime version of 'real_dur''.  TODO so maybe it should be
-- named like that?
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
    let ts = TimeStep.time_step $ TimeStep.RelativeMark TimeStep.match_meter rank
    end <- timestep start ts steps
    return $ end - start

parsed_meter_duration :: ScoreTime -> Text -> Int -> Derive.Deriver ScoreTime
parsed_meter_duration start rank steps = do
    rank <- Derive.require_right ("parsing timestep: "++) $
        Parse.parse TimeStep.parse_rank rank
    meter_duration start rank steps

-- * c_equal

c_equal :: (Derive.Callable d) => Derive.Transformer d
c_equal = Derive.transformer "equal" (Tags.prelude <> Tags.subs) equal_doc
    (Sig.parsed_manually equal_arg_doc equal_transformer)

equal_arg_doc :: Text
equal_arg_doc = "Many types."

equal_doc :: Text
equal_doc =
    "Evaluate the deriver with a value set. A special rule means this can be\
    \ called infix.  The arguments can take many forms to set different kinds\
    \ of values.\
    \\nSet environ values by setting a plain symbol or unset it by assigning\
    \ to `_`: `x = 42` or `x = _`.\
    \\nIf the symbol is prefixed with `>`, `*`, `.`, or `-`, it will set a\
    \ note, pitch, control, or val call, respectively. It sets the generator\
    \ by default, but will set the transformer if you add another `-`.  You\
    \ need quoting for symbols that don't match 'Derive.ParseBs.p_symbol'.\
    \ E.g.: set note generator: `>x = some-block`, note transformer: `>-x = t`,\
    \ control transfomrer: `'.-i' = t`, pitch val call: `'-4c' = 5c`.\
    \\nSet constant signals by assigning to a signal literal: `%c = .5` or\
    \ pitch: `#p = (4c)`.  `# = (4c)` sets the default pitch signal."

equal_transformer :: Derive.PassedArgs d -> Derive.Deriver a -> Derive.Deriver a
equal_transformer args deriver = case Derive.passed_vals args of
    [TrackLang.VSymbol assignee, val] ->
        case parse_equal assignee val deriver of
            Left err -> Derive.throw_arg_error err
            Right d -> d
    args -> Derive.throw_arg_error $ "unexpected arg types: "
        <> Seq.join ", " (map (Pretty.pretty . TrackLang.type_of) args)

parse_equal :: TrackLang.Symbol -> TrackLang.Val -> Derive.Deriver a
    -> Either String (Derive.Deriver a)
parse_equal (TrackLang.Symbol assignee) (TrackLang.VSymbol sym) deriver
    | Just new <- Text.stripPrefix ">" assignee = Right $
        override_call new sym deriver "note"
            (Derive.s_generator#Derive.s_note)
            (Derive.s_transformer#Derive.s_note)
    | Just new <- Text.stripPrefix "*" assignee = Right $
        override_call new sym deriver "pitch"
            (Derive.s_generator#Derive.s_pitch)
            (Derive.s_transformer#Derive.s_pitch)
    | Just new <- Text.stripPrefix "." assignee = Right $
        override_call new sym deriver "control"
            (Derive.s_generator#Derive.s_control)
            (Derive.s_transformer#Derive.s_control)
    | Just new <- Text.stripPrefix "-" assignee = Right $
        override_val_call new sym deriver
parse_equal (parse_val -> Just assignee) val deriver
    | Just control <- is_control assignee = case val of
        TrackLang.VControl val -> Right $ do
            sig <- to_signal val
            Derive.with_control control sig deriver
        TrackLang.VNum val
            | control == Controls.tempo -> Right $ set_tempo val
            | otherwise -> Right $
                Derive.with_control control (fmap Signal.constant val) deriver
        _ -> Left $ "binding a control expects a control or num, but got "
            <> Pretty.pretty (TrackLang.type_of val)
    | Just control <- is_pitch assignee = case val of
        TrackLang.VPitch val -> Right $
            Derive.with_pitch control (PitchSignal.constant val) deriver
        TrackLang.VPitchControl val -> Right $ do
            sig <- to_pitch_signal val
            Derive.with_pitch control sig deriver
        _ -> Left $ "binding a pitch signal expects a pitch or pitch"
            <> " control, but got " <> Pretty.pretty (TrackLang.type_of val)
    where
    set_tempo val =
        Internal.d_warp warp $ Internal.add_new_track_warp Nothing >> deriver
        where warp = Tempo.tempo_to_warp $ Signal.constant (Score.typed_val val)
    is_control (TrackLang.VControl (TrackLang.LiteralControl c)) = Just c
    is_control _ = Nothing
    is_pitch (TrackLang.VPitchControl (TrackLang.LiteralControl c))
        | c == Controls.null = Just Nothing
        | otherwise = Just (Just c)
    is_pitch _ = Nothing
parse_equal assignee val deriver = Right $ Derive.with_val assignee val deriver

parse_val :: TrackLang.Symbol -> Maybe TrackLang.RawVal
parse_val = either (const Nothing) Just . ParseBs.parse_val . TrackLang.unsym

-- | Look up a call with the given CallId and add it as an override to the
-- scope given by the lenses.  I wanted to pass just one lens, but apparently
-- they're not sufficiently polymorphic.
override_call :: Text -> TrackLang.CallId -> Derive.Deriver a
    -> Text
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Call d1))
    -> Lens Derive.Scopes (Derive.ScopeType (Derive.Call d2))
    -> Derive.Deriver a
override_call assignee source deriver name generator transformer
    | Just stripped <- Text.stripPrefix "-" assignee =
        override_scope stripped (name <> " transformer") transformer
    | otherwise = override_scope assignee (name <> " generator") generator
    where
    override_scope assignee name lens = do
        call <- get_call name (lens #$) source
        let modify = lens#Derive.s_override %= (single_lookup assignee call :)
        Derive.with_scopes modify deriver

override_val_call :: Text -> TrackLang.CallId -> Derive.Deriver a
    -> Derive.Deriver a
override_val_call assignee source deriver = do
    call <- get_call "val" (Derive.s_val #$) source
    let modify = Derive.s_val#Derive.s_override
            %= (single_val_lookup assignee call :)
    Derive.with_scopes modify deriver

get_call :: Text -> (Derive.Scopes -> Derive.ScopeType call)
    -> TrackLang.CallId -> Derive.Deriver call
get_call name get call_id =
    maybe (Derive.throw $ untxt $ Call.unknown_call_id name call_id)
        return =<< Derive.lookup_with get call_id

single_lookup :: Text -> Derive.Call d
    -> Derive.LookupCall (Derive.Call d)
single_lookup name = Derive.map_lookup . Map.singleton (TrackLang.Symbol name)

single_val_lookup :: Text -> Derive.ValCall
    -> Derive.LookupCall Derive.ValCall
single_val_lookup name =
    Derive.map_val_lookup . Map.singleton (TrackLang.Symbol name)
