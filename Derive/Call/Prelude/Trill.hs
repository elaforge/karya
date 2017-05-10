-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | Various kinds of trills.

    Trills want to generate an integral number of cycles.  For the purpose of
    counting integral cycles, trills count the end (either the end of the
    event, or the start of the next event).  This is different than other
    control calls, which tend to omit the end point, expecting that the next
    call will place a sample there.  This is so that a trill can end on an off
    note if it exactly fits into its allotted space, otherwise a 16th note
    trill in a quarter note would degenerate into a mordent.

    Various flavors of trills:

    - Trill cycles depend on real duration of note.  Cycle durations are given
    in real time.

    - As above, but durations are given in score time.

    - Number of trill cycles given as argument, and note stretches normally.

    - Sung style vibrato in a sine wave rather than a square wave.

    - Trill that simply adds an attribute, instrument will handle it.

    The generic 'tr' symbol can be bound to whichever variant is locally
    appropriate.

    It's easy to think of more variants of trills: hold the starting note
    briefly, hold the final note briefly, inject a little randomness, smooth
    the pitch curve by a variable amount, or variants that cover the range
    between trill and vibrato, etc.  One can also imagine dynamic effects.

    Instead of trying to provide a million functions here or a few
    with a million parameters, it should be relatively easy to reuse the
    functions in here to write a specific kind of trill for the particular
    piece.
-}
module Derive.Call.Prelude.Trill where
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.SignalTransform as SignalTransform
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- * note calls

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    ([(name, c_note_trill start end) | (name, start, end) <- trill_variations]
    ++
    [ ("trem", c_tremolo_generator)
    ])
    [ ("trem", c_tremolo_transformer) ]

c_note_trill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Note
c_note_trill hardcoded_start hardcoded_end =
    Derive.generator Module.prelude "tr" Tags.ly
    ("Generate a note with a trill.\
    \\nUnlike a trill on a pitch track, this generates events for each\
    \ note of the trill. This is more appropriate for fingered trills,\
    \ or monophonic instruments that use legato to play slurred notes."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,)
    <$> defaulted "neighbor" (Sig.typed_control "tr-neighbor" 1 Score.Diatonic)
        "Alternate with a pitch at this interval."
    <*> trill_speed_arg <*> trill_env hardcoded_start hardcoded_end
    ) $ \(neighbor, speed, (start_dir, end_dir, hold, adjust)) ->
    Sub.inverting $ \args ->
    Lily.note_code (Lily.SuffixFirst, "\\trill") args $ do
        (transpose, control) <- trill_from_controls
            (Args.range_or_next args) start_dir end_dir adjust hold
            neighbor speed
        xs <- mapM (Derive.score . fst) (Signal.unsignal transpose)
        let end = snd $ Args.range args
        let notes = do
                (x, maybe_next) <- Seq.zip_next xs
                let next = fromMaybe end maybe_next
                return $ Sub.Event x (next-x) Call.note
        Call.add_control control (Score.untyped transpose) (Sub.derive notes)

c_attr_trill :: Derive.Generator Derive.Note
c_attr_trill = Derive.generator Module.prelude "attr-tr" Tags.attr
    "Generate a trill by adding a `+trill` attribute. Presumably this is a\
    \ sampled instrument that has a trill keyswitch."
    $ Sig.call
    (defaulted "neighbor" (Sig.typed_control "tr-neighbor" 1 Score.Chromatic)
        "Alternate with a pitch at this interval.  Only 1c and 2c are allowed."
    ) $ \neighbor args -> do
        (width, typ) <- Call.transpose_control_at Typecheck.Chromatic neighbor
            =<< Args.real_start args
        width_attr <- case (width, typ) of
            (1, Typecheck.Chromatic) -> return Attrs.half
            (2, Typecheck.Chromatic) -> return Attrs.whole
            _ -> Derive.throw $
                "attribute trill only supports 1c and 2c trills: "
                <> ShowVal.show_val neighbor
        Call.add_attributes (Attrs.trill <> width_attr) (Call.placed_note args)

c_tremolo_generator :: Derive.Generator Derive.Note
c_tremolo_generator = Derive.generator Module.prelude "trem" Tags.ly
    "Repeat a single note. Or, if there are sub-notes, alternate with each of\
    \ the sub-notes in turn."
    $ Sig.call ((,) <$> Speed.arg <*> hold_env) $ \(speed, hold) args -> do
        starts <- tremolo_starts hold speed (Args.range_or_next args)
        notes <- Sub.sub_events args
        case filter (not . null) notes of
            [] -> Sub.inverting_args args $ \args -> Lily.note_code code args $
                simple_tremolo starts [Call.note]
            notes -> Lily.notes_code code args $
                Sub.derive $ chord_tremolo starts notes
    where code = (Lily.SuffixAll, ":32")

c_tremolo_transformer :: Derive.Transformer Derive.Note
c_tremolo_transformer = Derive.transformer Module.prelude "trem" Tags.subs
    "Repeat the transformed note. The generator is creating the notes so it\
    \ can set them to the appropriate duration, but this one has to stretch\
    \ them to fit." $ Sig.callt ((,) <$> Speed.arg <*> hold_env) $
    \(speed, hold) args deriver -> do
        starts <- tremolo_starts hold speed (Args.range_or_next args)
        simple_tremolo starts [Args.normalized args deriver]

tremolo_starts :: BaseTypes.Duration -> BaseTypes.ControlRef
    -> (ScoreTime, ScoreTime) -> Derive.Deriver [ScoreTime]
    -- ^ start time for each note, and one for the end of the last one
tremolo_starts hold speed (start, end) = do
    hold <- Call.score_duration start hold
    (speed_sig, time_type) <- Call.to_time_function Typecheck.Real speed
    add_hold (start, end) hold <$> case time_type of
        Typecheck.Real -> do
            start <- Derive.real (start + hold)
            end <- Derive.real end
            mapM Derive.score . full_notes end
                =<< Speed.real_starts speed_sig start end
        Typecheck.Score -> do
            starts <- Speed.score_starts speed_sig (start + hold) end
            return $ full_notes end starts

-- | This is like 'tremolo_starts', but takes a start and end speed instead
-- of a speed signal.  In exchange, it can have start and end be different
-- time types, which a signal can't express.  Of course I could make the
-- signal into duration and then do the reciprocal in the score as a val call,
-- but that seems too complicated for tracklang.
tremolo_starts_curve :: ControlUtil.Curve -> BaseTypes.Duration
    -> Speed.Speed -> Speed.Speed -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [ScoreTime]
    -- ^ start time for each note, and one for the end of the last one
tremolo_starts_curve curve hold start_speed end_speed (start, end) = do
    hold <- Call.score_duration start hold
    real_range <- (,) <$> Derive.real start <*> Derive.real end
    (add_hold (start, end) hold . full_notes end <$>) $ mapM Derive.score
        =<< Speed.starts_curve curve start_speed end_speed real_range True
        -- include_end=True because the end time is also included.

-- | Add the hold time to the first tremolo note.
add_hold :: (ScoreTime, ScoreTime) -> ScoreTime -> [ScoreTime] -> [ScoreTime]
add_hold (start, end) hold starts
    | hold >= end - start = [start, end]
    | hold > 0 = start : starts
    | otherwise = starts

-- | Alternate each note with the other notes within its range, in order from
-- the lowest track to the highest.
--
-- This doesn't restart the tremolo when a new note enters, if you want that
-- you can have multiple tremolo events.
chord_tremolo :: forall a. [ScoreTime] -> [[Sub.GenericEvent a]]
    -> [Sub.GenericEvent a]
chord_tremolo starts note_tracks =
    Maybe.catMaybes $ snd $
        List.mapAccumL emit (-1, by_track) $ zip starts (drop 1 starts)
    where
    emit (last_tracknum, notes_) (pos, next_pos) = case chosen of
        Nothing -> ((last_tracknum, notes), Nothing)
        Just (tracknum, note) -> ((tracknum, notes),
            Just $ Sub.Event pos (next_pos-pos) (Sub.event_note note))
        where
        chosen =
            Seq.minimum_on fst (filter ((>last_tracknum) . fst) overlapping)
                <|> Seq.minimum_on fst overlapping
        overlapping = filter (Sub.event_overlaps pos . snd) notes
        notes = dropWhile ((<=pos) . Sub.event_end . snd) notes_
    by_track :: [(TrackNum, Sub.GenericEvent a)]
    by_track = Seq.sort_on (Sub.event_end . snd)
        [ (tracknum, event)
        | (tracknum, track) <- zip [0..] note_tracks, event <- track
        ]

-- | Just cycle the given notes.
simple_tremolo :: [ScoreTime] -> [Derive.NoteDeriver] -> Derive.NoteDeriver
simple_tremolo starts notes = Sub.derive
    [ Sub.Event start (end - start) note
    | (start, end, note) <- zip3 starts (drop 1 starts) $
        if null notes then [] else cycle notes
    ]

-- | This is defined here instead of in "Derive.Call.Attribute" so it can be
-- next to 'c_tremolo'.
c_attr_tremolo :: Make.Calls Derive.Note
c_attr_tremolo = Make.attributed_note Module.prelude Attrs.trem

-- | Given start times, return only ones whose full duration fits before the
-- end time.  This is the tremolo analog to 'full_cycles'.  Unlike a trill, it
-- emits both the starts and ends, and therefore the last sample will be at the
-- end time, rather than before it.  It should always emit an even number of
-- elements.
full_notes :: Ord a => a -> [a] -> [a]
full_notes end [t]
    | t < end = [t, end]
    | otherwise = []
full_notes end ts = go ts
    where
    go [] = []
    go (t1:ts) = case ts of
        t2 : _
            | t2 > end -> [end]
            | otherwise -> t1 : go ts
        [] -> [end]
    -- This is surprisingly tricky.


-- * pitch calls

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map $
    [(name, c_pitch_trill start end) | (name, start, end) <- trill_variations]
    ++
    [ ("xcut", c_xcut_pitch False)
    , ("xcut-h", c_xcut_pitch True)
    ]

c_pitch_trill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Pitch
c_pitch_trill hardcoded_start hardcoded_end =
    Derive.generator1 Module.prelude "tr" mempty
    ("Generate a pitch signal of alternating pitches."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,,,)
    <$> required "note" "Base pitch."
    <*> defaulted "neighbor" (Sig.typed_control "tr-neighbor" 1 Score.Diatonic)
        "Alternate with a pitch at this interval."
    <*> trill_speed_arg <*> transition_env
    <*> trill_env hardcoded_start hardcoded_end
    ) $ \(note, neighbor, speed, transition, (start_dir, end_dir, hold, adjust))
            args -> do
        (transpose, control) <- trill_from_controls (Args.range_or_next args)
            start_dir end_dir adjust hold neighbor speed
        transpose <- smooth_trill transition transpose
        start <- Args.real_start args
        return $ PSignal.apply_control control (Score.untyped transpose) $
            PSignal.signal [(start, note)]

c_xcut_pitch :: Bool -> Derive.Generator Derive.Pitch
c_xcut_pitch hold = Derive.generator1 Module.prelude "xcut" mempty
    "Cross-cut between two pitches.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> defaulted "val1" (Sig.pitch "xcut1") "First pitch."
    <*> defaulted "val2" (Sig.pitch "xcut2") "Second pitch."
    <*> defaulted "speed" (Sig.typed_control "xcut-speed" 14 Score.Real)
        "Speed."
    ) $ \(val1, val2, speed) args -> do
        transitions <- Speed.starts speed (Args.range_or_next args) False
        val1 <- Call.to_psignal val1
        val2 <- Call.to_psignal val2
        return $ xcut_pitch hold val1 val2 transitions

xcut_pitch :: Bool -> PSignal.PSignal -> PSignal.PSignal -> [RealTime]
    -> PSignal.PSignal
xcut_pitch hold val1 val2 =
    mconcat . snd . List.mapAccumL slice (val1, val2) . Seq.zip_next
    where
    slice (val1, val2) (t, next_t)
        | hold = (next, initial)
        | otherwise = (next, initial <> chunk)
        where
        chopped = PSignal.drop_before_strict t val1
        chunk = maybe chopped (\n -> PSignal.drop_at_after n chopped) next_t
        next = (val2, PSignal.drop_before t val1)
        initial = case PSignal.at t val1 of
            Nothing -> mempty
            Just p -> PSignal.signal [(t, p)]


-- * control calls

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.generator_call_map $
    [(name, c_control_trill start end) | (name, start, end) <- trill_variations]
    ++
    [ ("saw", c_saw)
    , ("sine", c_sine Bipolar)
    , ("sine+", c_sine Positive)
    , ("sine-", c_sine Negative)
    , ("xcut", c_xcut_control False)
    , ("xcut-h", c_xcut_control True)
    ]

c_control_trill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Control
c_control_trill hardcoded_start hardcoded_end =
    Derive.generator1 Module.prelude "tr" mempty
    ("The control version of the pitch trill. It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,,)
    <$> defaulted "neighbor" (Sig.control "tr-neighbor" 1)
        "Alternate with this value."
    <*> trill_speed_arg <*> transition_env
    <*> trill_env hardcoded_start hardcoded_end
    ) $ \(neighbor, speed, transition, (start_dir, end_dir, hold, adjust))
            args -> do
        (sig, _) <- trill_from_controls (Args.range_or_next args) start_dir
            end_dir adjust hold neighbor speed
        smooth_trill transition sig

c_saw :: Derive.Generator Derive.Control
c_saw = Derive.generator1 Module.prelude "saw" mempty
    "Emit a sawtooth.  By default it has a downward slope, but you can make\
    \ an upward slope by setting `from` and `to`."
    $ Sig.call ((,,)
    <$> Speed.arg
    <*> defaulted "from" 1 "Start from this value."
    <*> defaulted "to" 0 "End at this value."
    ) $ \(speed, from, to) args -> do
        starts <- Speed.starts speed (Args.range_or_next args) True
        srate <- Call.get_srate
        return $ saw srate starts from to

saw :: RealTime -> [RealTime] -> Double -> Double -> Signal.Control
saw srate starts from to = mconcat $ zipWith saw starts (drop 1 starts)
    where
    saw t1 t2 = ControlUtil.segment srate True True id t1 from (t2-srate) to

-- ** sine

data SineMode = Bipolar | Negative | Positive deriving (Show)

-- | This is probably not terribly convenient to use on its own, I should
-- have some more specialized calls based on this.
c_sine :: SineMode -> Derive.Generator Derive.Control
c_sine mode = Derive.generator1 Module.prelude "sine" mempty
    "Emit a sine wave. The default version is centered on the `offset`,\
    \ and the `+` and `-` variants are above and below it, respectively."
    $ Sig.call ((,,)
    <$> defaulted "speed" (Sig.typed_control "sine-speed" 1 Score.Real)
        "Frequency."
    <*> defaulted "amp" 1 "Amplitude, measured center to peak."
    <*> defaulted "offset" 0 "Center point."
    ) $ \(speed, amp, offset) args -> do
        (speed_sig, time_type) <- Call.to_time_function Typecheck.Real speed
        case time_type of
            Typecheck.Score -> Derive.throw "RealTime signal required"
            _ -> return ()
        srate <- Call.get_srate
        let sign = case mode of
                Bipolar -> 0
                Negative -> -amp
                Positive -> amp
        (start, end) <- Args.real_range_or_next args
        -- let samples = Seq.range' start end srate
        return $ Signal.map_y ((+(offset+sign)) . (*amp)) $
            sine srate start end speed_sig

sine :: RealTime -> RealTime -> RealTime -> Typecheck.Function -> Signal.Control
sine srate start end freq_sig = Signal.unfoldr go (start, 0)
    where
    go (pos, phase)
        | pos >= end = Nothing
        | otherwise = Just ((pos, sin phase), (pos + srate, next_phase))
        where
        next_phase = phase + RealTime.to_seconds srate * 2*pi * freq_sig pos


-- ** xcut

c_xcut_control :: Bool -> Derive.Generator Derive.Control
c_xcut_control hold = Derive.generator1 Module.prelude "xcut" mempty
    "Cross-cut between two signals.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> defaulted "val1" (Sig.control "xcut1" 1) "First value."
    <*> defaulted "val2" (Sig.control "xcut2" 0) "Second value."
    <*> defaulted "speed" (Sig.typed_control "xcut-speed" 14 Score.Real)
        "Speed."
    ) $ \(val1, val2, speed) args -> do
        transitions <- Speed.starts speed (Args.range_or_next args) False
        val1 <- Call.to_signal val1
        val2 <- Call.to_signal val2
        return $ xcut_control hold val1 val2 transitions

xcut_control :: Bool -> Signal.Control -> Signal.Control -> [RealTime]
    -> Signal.Control
xcut_control hold val1 val2 =
    mconcat . snd . List.mapAccumL slice (val1, val2) . Seq.zip_next
    where
    slice (val1, val2) (t, next_t)
        | hold = (next, initial)
        | otherwise = (next, initial <> chunk)
        where
        chopped = Signal.drop_before_strict t val1
        chunk = maybe chopped (\n -> Signal.drop_at_after n chopped) next_t
        next = (val2, Signal.drop_before t val1)
        initial = Signal.signal [(t, Signal.at t val1)]

-- * util

trill_speed_arg :: Sig.Parser BaseTypes.ControlRef
trill_speed_arg = defaulted "speed" (Sig.typed_control "tr-speed" 14 Score.Real)
    "Trill at this speed. If it's a RealTime, the value is the number of\
    \ cycles per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of cycles per ScoreTime\
    \ unit, and will stretch along with tempo changes. In either case,\
    \ this will emit only whole notes, i.e. it will end sooner to avoid\
    \ emitting a cut-off note at the end."

-- | Whether the trill starts or ends on the high or low note.  This is another
-- way to express 'AbsoluteMode'.
--
-- I had a lot of debate about whether I should use High and Low, or Unison and
-- Neighbor.  Unison-Neighbor is more convenient for the implementation but
-- High-Low I think is more musically intuitive.
data Direction = High | Low deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal Direction where show_val = Typecheck.enum_show_val
instance Typecheck.Typecheck Direction
instance Typecheck.TypecheckSymbol Direction

-- | This is the like 'Direction', but in terms of the unison and neighbor
-- pitches, instead of high and low.
data AbsoluteMode = Unison | Neighbor deriving (Bounded, Eq, Enum, Show)

transition_env :: Sig.Parser BaseTypes.ControlRef
transition_env =
    Sig.environ "tr-transition" Sig.Unprefixed (Sig.control "tr-transition" 0)
    "Alternate with a pitch at this interval."

-- | A bundle of standard configuration for trills.
trill_env :: Maybe Direction -> Maybe Direction
    -> Sig.Parser (Maybe Direction, Maybe Direction, BaseTypes.Duration, Adjust)
trill_env start_dir end_dir =
    (,,,) <$> start <*> end <*> hold_env <*> adjust_env
    where
    start = case start_dir of
        Nothing -> Sig.environ "tr-start" Sig.Unprefixed Nothing
            "Which note the trill starts with. If not given, it will start\
            \ the unison note, which means it may move up or down."
        Just dir -> pure $ Just dir
    end = case end_dir of
        Nothing -> Sig.environ "tr-end" Sig.Unprefixed Nothing
            "Which note the trill ends with. If not given, it can end with\
            \ either."
        Just dir -> pure $ Just dir

-- Its default is both prefixed and unprefixed so you can put in a tr-hold
-- globally, and so you can have a short @hold=n |@ for a single call.
hold_env :: Sig.Parser BaseTypes.Duration
hold_env = Typecheck._real <$>
    Sig.environ (Derive.str_to_arg_name EnvKey.hold) Sig.Both
        (Typecheck.real 0) "Time to hold the first note."

trill_variations :: [(Expr.CallId, Maybe Direction, Maybe Direction)]
trill_variations =
    [ (Expr.CallId $ "tr"
            <> (if start == Nothing && end /= Nothing
                then "-" else direction_affix start)
            <> direction_affix end,
        start, end)
    | start <- dirs, end <- dirs
    ]
    where dirs = [Nothing, Just High, Just Low]

direction_affix :: Maybe Direction -> Text
direction_affix Nothing = ""
direction_affix (Just High) = "^"
direction_affix (Just Low) = "_"

direction_doc :: Maybe a -> Maybe a -> Doc.Doc
direction_doc Nothing Nothing = ""
direction_doc _ _ = "\nA `^` suffix makes the trill starts on the higher value,\
    \ while `_` makes it start on the lower value. A second suffix causes it\
    \ to end on the higher or lower value, e.g. `^_` starts high and ends low.\
    \ `-_` has start unspecified, and ends low.\
    \ No suffix causes it to obey the settings in scope."

-- | Resolve start and end Directions to the first and second trill notes.
convert_direction :: RealTime -> Typecheck.Function
    -> Maybe Direction -> Maybe Direction
    -> ((Typecheck.Function, Typecheck.Function), Maybe Bool)
    -- ^ Signals for the first and second trill notes.  The boolean indicates
    -- whether the transitions should be even to end on the expected end
    -- Direction, and Nothing if it doesn't matter.
convert_direction start_t neighbor start end = (vals, even_transitions)
    where
    first = case start of
        Nothing -> Unison
        Just Low -> if neighbor_low then Neighbor else Unison
        Just High -> if neighbor_low then Unison else Neighbor
    vals = case first of
        Unison -> (const 0, neighbor)
        Neighbor -> (neighbor, const 0)
    -- If I end Low, and neighbor is low, and I started with Unison, then val2
    -- is low, so I want even transitions.  Why is it so complicated just to
    -- get a trill to end high or low?
    first_low = case first of
        Unison -> not neighbor_low
        Neighbor -> neighbor_low
    even_transitions = case end of
        Nothing -> Nothing
        Just Low -> Just (not first_low)
        Just High -> Just first_low
    neighbor_low = neighbor start_t < 0

-- | How to adjust an ornament to fulfill its 'Direction' restrictions.
data Adjust =
    -- | Adjust by shortening the ornament.
    Shorten
    -- | Adjust by increasing the speed.
    | Stretch
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal Adjust where show_val = Typecheck.enum_show_val
instance Typecheck.Typecheck Adjust
instance Typecheck.TypecheckSymbol Adjust

adjust_env :: Sig.Parser Adjust
adjust_env = Sig.environ "adjust" Sig.Both Shorten
    "How to adjust a trill to fulfill its start and end pitch restrictions."

-- ** transitions

trill_from_controls :: (ScoreTime, ScoreTime) -> Maybe Direction
    -> Maybe Direction -> Adjust -> BaseTypes.Duration
    -> BaseTypes.ControlRef -> BaseTypes.ControlRef
    -> Derive.Deriver (Signal.Control, Score.Control)
trill_from_controls (start, end) start_dir end_dir adjust hold neighbor speed
        = do
    (neighbor_sig, control) <-
        Call.to_transpose_function Typecheck.Diatonic neighbor
    real_start <- Derive.real start
    let ((val1, val2), even_transitions) = convert_direction real_start
            neighbor_sig start_dir end_dir
    hold <- Call.score_duration start hold
    transitions <- adjusted_transitions False even_transitions adjust 0 hold
        speed (start, end)
    return (trill_from_transitions val1 val2 transitions, control)

smooth_trill :: BaseTypes.ControlRef -> Signal.Control
    -> Derive.Deriver Signal.Control
smooth_trill time transitions = do
    srate <- Call.get_srate
    sig_function <- Typecheck.to_signal_or_function time
    case sig_function of
        Left sig | Signal.constant_val (Score.typed_val sig) == Just 0 ->
            return transitions
        _ -> do
            f <- Typecheck.convert_to_function time sig_function
            return $ SignalTransform.smooth_relative id srate
                (Score.typed_val . f) transitions

-- | Get trill transition times, adjusted for all the various fancy parameters
-- that trills have.
adjusted_transitions :: Bool -- ^ include a transition at the end time
    -> Maybe Bool -- ^ emit an even number of transitions, or Nothing for
    -- however many will fit
    -> Adjust -- ^ how to fit the transitions into the time range
    -> Double -- ^ offset every other transition by this amount, from -1--1
    -> ScoreTime -- ^ extend the first transition by this amount
    -> BaseTypes.ControlRef -- ^ transition speed
    -> (ScoreTime, ScoreTime) -> Derive.Deriver [RealTime]
adjusted_transitions include_end even adjust bias hold speed (start, end) = do
    real_end <- Derive.real end
    add_hold . add_bias bias . adjust_transitions real_end adjust . trim
        =<< trill_transitions (start + hold, end) include_end speed
    where
    add_hold transitions
        | hold > 0 = (: drop 1 transitions) <$> Derive.real start
        | otherwise = return transitions
    trim = case even of
        Nothing -> id
        Just even -> if even then take_even else take_odd
    take_even (x:y:zs) = x : y : take_even zs
    take_even _ = []
    take_odd [x, _] = [x]
    take_odd (x:y:zs) = x : y : take_odd zs
    take_odd xs = xs

adjust_transitions :: RealTime -> Adjust -> [RealTime] -> [RealTime]
adjust_transitions _ Shorten ts = ts
adjust_transitions end Stretch ts@(_:_:_) = zipWith (+) offsets ts
    where
    -- (_:_:_) above means both the last and division are safe.
    stretch = max 0 (end - last ts) / fromIntegral (length ts - 1)
    offsets = Seq.range_ 0 stretch
adjust_transitions _ Stretch ts = ts

add_bias :: Double -> [RealTime] -> [RealTime]
add_bias _ [] = []
add_bias bias (t:ts)
    | bias == 0 = t : ts
    | bias > 0 = t : positive (min 1 (RealTime.seconds bias)) ts
    | otherwise = negative (min 1 (RealTime.seconds (abs bias))) (t:ts)
    where
    positive bias (x:y:zs) = Num.scale x y bias : y : positive bias zs
    positive _ xs = xs
    negative bias (x:y:zs) = x : Num.scale x y bias : negative bias zs
    negative _ xs = xs

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: Typecheck.Function -> Typecheck.Function
    -> [RealTime] -> Signal.Control
trill_from_transitions val1 val2 transitions = Signal.signal
    [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]

-- | Create trill transition points from a speed.
trill_transitions :: (ScoreTime, ScoreTime) -> Bool -> BaseTypes.ControlRef
    -> Derive.Deriver [RealTime]
trill_transitions range include_end speed = do
    (speed_sig, time_type) <- Call.to_time_function Typecheck.Real speed
    case time_type of
        Typecheck.Real -> real_transitions range include_end speed_sig
        Typecheck.Score -> score_transitions range include_end speed_sig

real_transitions :: (ScoreTime, ScoreTime) -> Bool -> Typecheck.Function
    -> Derive.Deriver [RealTime]
real_transitions (start, end) include_end speed = do
    start <- Derive.real start
    end <- Derive.real end
    full_cycles RealTime.eta end include_end <$>
        Speed.real_starts speed start end

score_transitions :: (ScoreTime, ScoreTime) -> Bool -> Typecheck.Function
    -> Derive.Deriver [RealTime]
score_transitions (start, end) include_end speed = do
    all_transitions <- Speed.score_starts speed start end
    mapM Derive.real $ full_cycles ScoreTime.eta end include_end all_transitions

-- | Given a list of trill transition times, take only ones with a complete
-- duration.  Otherwise a trill can wind up with a short note at the end, which
-- sounds funny.  However it's ok if the note is slightly too short, as tends
-- to happen with floating point.
full_cycles :: (Ord a, Num a) => a -> a -> Bool -> [a] -> [a]
full_cycles eta end include_end vals
    | null cycles = take 1 vals
    | otherwise = cycles
    where
    cycles = go vals
    go (x1 : xs) = case xs of
        x2 : _ | x2 <= end + eta -> x1 : go xs
        _ | include_end && x1 - eta <= end -> [x1]
        _ -> []
    go [] = []
