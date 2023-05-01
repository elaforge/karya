-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
module Derive.C.Prelude.Trill (
    library
    , c_note_trill, c_tremolo_generator
    , hold_env, tremolo_starts_curve
    -- * transitions
    , trill_transitions, adjusted_transitions
    -- * types
    , Config(..)
    , Direction(..), direction_affix
    , AbsoluteMode(..)
    , Adjust(..), adjust_env
    , get_trill_control_smooth

    -- testing
    , full_notes, chord_tremolo, get_trill_control, xcut_control
) where
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Lilypond.Convert
import qualified Perform.Lilypond.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


library :: Library.Library
library = mconcat
    -- Note
    [ make_trills "tr" (c_note_trill False)
    , Library.generators [("trem", c_tremolo_generator Nothing)]
    , Library.transformers [("trem", c_tremolo_transformer)]
    -- Pitch
    , make_trills "tr" c_pitch_trill
    , make_trills "trs" c_pitch_trill_smooth
    , Library.generators
        [ ("xcut", c_xcut_pitch False)
        , ("xcut-h", c_xcut_pitch True)
        ]
    -- Control
    , make_trills "tr" c_control_trill
    , make_trills "trs" c_control_trill_smooth
    , Library.generators $
        [ ("saw", c_saw)
        , ("sine", c_sine Bipolar)
        , ("sine+", c_sine Positive)
        , ("sine-", c_sine Negative)
        , ("xcut", c_xcut_control False)
        , ("xcut-h", c_xcut_control True)
        ]
    ]
    where
    make_trills prefix make = Library.generators
        [(name, make start end) | (name, start, end) <- trill_variations prefix]

-- * note calls

c_note_trill :: Bool -> Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Note
c_note_trill use_attributes hardcoded_start hardcoded_end =
    Derive.generator Module.prelude "tr" Tags.ly
    ("Generate a note with a trill.\
    \\nUnlike a trill on a pitch track, this generates events for each\
    \ note of the trill. This is more appropriate for fingered trills,\
    \ or monophonic instruments that use legato to play slurred notes.\
    \\nInstruments that support +trill attributes should enable the attributes\
    \ version, which emits a single note with `+trill+half`, `+trill+whole`, or\
    \ all the notes with `+trill`, depending on the interval."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,)
    <$> neighbor_arg
    <*> config_arg hardcoded_start hardcoded_end
    <*> Sig.environ_key "tr-style" Tr
        "Notation variant: tr symbol, tr~, or tremolo."
    ) $ \(neighbor, config, style) -> Sub.inverting $ \args ->
    Ly.when_lilypond (note_trill_ly style args neighbor)
        (note_trill use_attributes neighbor config args)

type Neighbor = Either Typecheck.DiatonicTransposeFunctionT PSignal.Pitch

neighbor_arg :: Sig.Parser Neighbor
neighbor_arg = Sig.defaulted "neighbor" (Left 1 :: Either Int Sig.Dummy)
    "Alternate with an interval or pitch."

note_trill :: Bool -> Neighbor -> Config
    -> Derive.PassedArgs a -> Derive.NoteDeriver
note_trill use_attributes neighbor config args
    | use_attributes = trill_attributes neighbor (Args.start args) >>= \case
        Just attr ->
            Call.add_attributes (Attrs.trill <> attr) (Call.placed_note args)
        Nothing -> Call.add_attributes Attrs.trill trill_notes
    | otherwise = trill_notes
    where
    trill_notes = do
        (neighbor, control) <- neighbor_to_function (Args.start args) neighbor
        transpose <- get_trill_control config (Args.range_or_next args) neighbor
        Sub.derive =<< mapM (note control) (Lists.zipNext transpose)
    note control ((x, transpose), next) = do
        start <- Derive.score x
        let end = snd $ Args.range args
        next <- maybe (return end) (Derive.score . fst) next
        return $ SubT.EventT start (next-start) $
            Call.add_constant control transpose Call.note
    -- trill_notes = do
    --     neighbor <- neighbor_to_signal2 (Args.start args) neighbor
    --     (transpose, control) <- get_trill_control2
    --         (Args.range_or_next args) start_dir end_dir adjust hold
    --         neighbor speed
    --     xs <- mapM (Derive.score . fst) transpose
    --     let end = snd $ Args.range args
    --     let notes = do
    --             (x, maybe_next) <- Lists.zipNext xs
    --             let next = fromMaybe end maybe_next
    --             return $ SubT.EventT x (next-x) Call.note
    --     Call.add_control control (ScoreT.untyped transpose)
    --         (Sub.derive notes)

        -- TODO this is an implementation that directly uses the neighbor pitch
        -- instead of the roundabout signal thing.  But I still need the signal
        -- if it changes.  Implement when I'm not in such a hurry.

        -- neighbor <- case neighbor of
        --     Right p -> return p
        --     Left control -> undefined
        -- let neighbor_low = False -- TODO
        -- (who_first, transitions) <- get_trill_transitions
        --     (Args.range_or_next args) start_dir end_dir
        --     adjust hold speed neighbor_low
        -- base <- Call.get_pitch_here args
        -- let pitches = cycle $ case who_first of
        --         Unison -> [base, neighbor]
        --         Neighbor -> [neighbor, base]
        -- transitions <- mapM Derive.score transitions
        -- Sub.derive $ do
        --     (pitch, (x, maybe_next)) <-
        --         zip pitches (Lists.zipNext transitions)
        --     let next = fromMaybe (snd (Args.range args)) maybe_next
        --     return $ SubT.EventT x (next-x) (Call.pitched_note pitch)

neighbor_to_function :: ScoreTime -> Neighbor
    -> Derive.Deriver (ScoreT.Function, ScoreT.Control)
neighbor_to_function _ (Left (Typecheck.DiatonicTransposeFunctionT typ f)) =
    return (f, Typecheck.transpose_control typ)
neighbor_to_function start (Right neighbor) = do
    start <- Derive.real start
    base <- Call.get_pitch start
    diff <- Call.nn_difference start neighbor base
    return (const (realToFrac diff), Typecheck.transpose_control ScoreT.TNn)

trill_attributes :: Neighbor -> ScoreTime
    -> Derive.Deriver (Maybe Attrs.Attributes)
trill_attributes neighbor start = do
    start <- Derive.real start
    (pitch, neighbor) <- pitch_and_neighbor neighbor start
    diff <- Call.nn_difference start neighbor pitch
    return $ if
        | Pitch.nns_equal diff 1 -> Just Attrs.half
        | Pitch.nns_equal diff 2 -> Just Attrs.whole
        | otherwise -> Nothing

data TrillStyle = Tr | Span | Tremolo
    deriving (Show, Eq, Enum, Bounded)

instance Typecheck.Typecheck TrillStyle
instance Typecheck.ToVal TrillStyle
instance ShowVal.ShowVal TrillStyle

note_trill_ly :: TrillStyle -> Derive.PassedArgs a -> Neighbor
    -> Derive.NoteDeriver
note_trill_ly style args neighbor = do
    start <- Args.real_start args
    (pitch, neighbor) <- pitch_and_neighbor neighbor start
    diff <- Call.nn_difference start neighbor pitch
    let tremolo = tremolo_trill_ly pitch neighbor (Args.start args)
            (Args.duration args)
    env <- Derive.get_environ
    let ly_pitch = Derive.require_right id . Lilypond.Convert.pitch_to_lily env
            =<< Derive.resolve_pitch start neighbor
    case style of
        _ | not (Pitch.nns_equal diff 1) && not (Pitch.nns_equal diff 2) ->
            tremolo
        Tremolo -> tremolo
        Tr -> do
            npitch@(Types.Pitch _ _ acc) <- ly_pitch
            in_key <- pitch_in_key npitch
            let code = case acc of
                    _ | in_key -> "\\trill"
                    Types.FlatFlat -> "^\\trFlatFlat"
                    Types.Flat -> "^\\trFlat"
                    Types.Natural -> "^\\trNatural"
                    Types.Sharp -> "^\\trSharp"
                    Types.SharpSharp -> "^\\trSharpSharp"
            -- TODO this should by Ly.note_append, but I can't put the \trFlat
            -- macros on a single pitch.
            Ly.add_first (Ly.append Constants.All, code)
                (Call.placed_note args)
        Span -> do
            npitch <- ly_pitch
            Ly.add_first (Ly.prepend, "\\pitchedTrill") $
                Ly.add_first (Ly.append Constants.First, "\\startTrillSpan "
                    <> Types.to_lily npitch) $
                Ly.add_first (Ly.append Constants.Last, "\\stopTrillSpan") $
                Call.placed_note args

pitch_in_key :: Types.Pitch -> Derive.Deriver Bool
pitch_in_key ly_pitch = do
    key <- get_key
    return $ in_key key (Pitch.pitch_degree (Types.to_pitch ly_pitch))

get_key :: Derive.Deriver Theory.Key
get_key = do
    maybe_key <- Call.lookup_key
    Derive.require ("unrecognized key: " <> pretty maybe_key) $
        Twelve.lookup_key maybe_key

in_key :: Theory.Key -> Pitch.Degree -> Bool
in_key key (Pitch.Degree pc acc) = Theory.accidentals_at_pc key pc == acc

pitch_and_neighbor :: Neighbor -> RealTime
    -> Derive.Deriver (PSignal.Pitch, PSignal.Pitch)
pitch_and_neighbor (Right neighbor) start =
    (, neighbor) <$> Call.get_pitch start
pitch_and_neighbor (Left (Typecheck.DiatonicTransposeFunctionT typ f))
        start = do
    base <- Call.get_pitch start
    let width = f start
    case (typ, width) of
        (ScoreT.TChromatic, 1) ->
            (base,) <$> (chromatic_neighbor =<< Derive.resolve_pitch start base)
        _ -> return
            (base, Pitches.transpose (Typecheck.to_transpose typ width) base)

-- | Given a pitch, find the enharmonic one chromatic step above it which is
-- at pitch class + 1.  This is because trills should alternate with the next
-- pitch class, so c to d flat, not c to c#.
chromatic_neighbor :: PSignal.Transposed -> Derive.Deriver PSignal.Pitch
chromatic_neighbor pitch = do
    -- TODO this is way too complicated
    (parse, unparse, transpose) <- Call.get_pitch_functions
    pitch <- Call.parse_pitch parse pitch
    key <- get_key
    neighbor <- Derive.require "transpose" $ transpose Scale.Chromatic 1 pitch
    neighbor <- Derive.require "enharmonic" $
        List.find ((== Pitch.pitch_pc pitch + 1) . Pitch.pitch_pc) $
            Theory.enharmonics_of (Theory.key_layout key) neighbor
    note <- Derive.require "unparse" $ unparse neighbor
    scale <- Call.get_scale
    Eval.eval_note scale note

-- | Emit the magic events to trigger lilypond's tremolo processing.
tremolo_trill_ly :: PSignal.Pitch -> PSignal.Pitch -> ScoreTime -> ScoreTime
    -> Derive.NoteDeriver
tremolo_trill_ly pitch1 pitch2 start dur =
    Derive.place start dur $ mconcat
        [ Ly.add_first (Ly.SetEnviron Constants.v_tremolo, "") Call.note
        , Call.pitched_note pitch1
        , Call.pitched_note pitch2
        ]

c_tremolo_generator :: Maybe ([Attrs.Attributes], Attrs.Attributes)
    -> Derive.Generator Derive.Note
c_tremolo_generator attrs_unless =
    Derive.generator Module.prelude "trem" Tags.ly
    ("Repeat a single note. Or, if there are sub-notes, alternate with each of\
    \ the sub-notes in turn." <> case attrs_unless of
        Nothing -> ""
        Just (unless, _) -> if attrs_unless == Nothing then "" else
            "\nThis version just derives plain notes with the "
            <> ShowVal.doc Attrs.trem <> " attribute, unless any of these\
            \ attributes are present: " <> ShowVal.doc unless <> ".")
    $ Sig.call ((,) <$> Speed.arg <*> hold_env) $ \(speed, hold) args -> do
        starts <- tremolo_starts hold speed (Args.range_or_next args)
        notes <- Sub.sub_events args
        attrs <- Call.get_attributes
        let use_attrs = maybe False (not . any (Attrs.contain attrs). fst)
                attrs_unless
        let trem_attrs = maybe mempty snd attrs_unless
        case filter (not . null) notes of
            [] -> Sub.inverting_args args $ \args -> Ly.note_code code args $
                if use_attrs
                    then Call.add_attributes Attrs.trem Call.note
                    else Call.add_attributes trem_attrs $
                        simple_tremolo starts [Call.note]
            notes -> Ly.notes_code code args $ if use_attrs
                then Call.add_attributes Attrs.trem $ Sub.derive_tracks notes
                else Call.add_attributes trem_attrs $
                    Sub.derive $ chord_tremolo starts notes
    where
    code = (Ly.append Constants.All, ":32")

c_tremolo_transformer :: Derive.Transformer Derive.Note
c_tremolo_transformer = Derive.transformer Module.prelude "trem" Tags.subs
    "Repeat the transformed note. The generator is creating the notes so it\
    \ can set them to the appropriate duration, but this one has to stretch\
    \ them to fit." $ Sig.callt ((,) <$> Speed.arg <*> hold_env) $
    \(speed, hold) args deriver -> do
        starts <- tremolo_starts hold speed (Args.range_or_next args)
        simple_tremolo starts [Args.normalized args deriver]

tremolo_starts :: DeriveT.Duration -> Typecheck.RealTimeFunctionT
    -> (ScoreTime, ScoreTime) -> Derive.Deriver [ScoreTime]
    -- ^ start time for each note, and one for the end of the last one
tremolo_starts hold (Typecheck.RealTimeFunctionT ttype speed) (start, end) = do
    hold <- Call.score_duration start hold
    add_hold (start, end) hold <$> case ttype of
        ScoreT.TReal -> do
            start <- Derive.real (start + hold)
            end <- Derive.real end
            mapM Derive.score . full_notes end
                =<< Speed.real_starts speed start end
        ScoreT.TScore -> do
            starts <- Speed.score_starts speed (start + hold) end
            return $ full_notes end starts

-- | This is like 'tremolo_starts', but takes a start and end speed instead
-- of a speed signal.  In exchange, it can have start and end be different
-- time types, which a signal can't express.  Of course I could make the
-- signal into duration and then do the reciprocal in the score as a val call,
-- but that seems too complicated for tracklang.
tremolo_starts_curve :: ControlUtil.CurveF -> DeriveT.Duration
    -> Speed.Speed -> Speed.Speed -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [ScoreTime]
    -- ^ start time for each note, and one for the end of the last one
tremolo_starts_curve curvef hold start_speed end_speed (start, end) = do
    hold <- Call.score_duration start hold
    real_range <- (,) <$> Derive.real start <*> Derive.real end
    (add_hold (start, end) hold . full_notes end <$>) $
        mapM Derive.score =<< Speed.starts_curve curvef start_speed end_speed
            real_range include_end
    where include_end = True -- because the end time is also included.

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
chord_tremolo :: forall a. [ScoreTime] -> [[SubT.EventT a]] -> [SubT.EventT a]
chord_tremolo starts note_tracks =
    Maybe.catMaybes $ snd $
        List.mapAccumL emit (-1, by_track) $ zip starts (drop 1 starts)
    where
    emit (last_tracknum, notes_) (pos, next_pos) = case chosen of
        Nothing -> ((last_tracknum, notes), Nothing)
        Just (tracknum, note) -> ((tracknum, notes),
            Just $ SubT.EventT pos (next_pos-pos) (SubT._note note))
        where
        chosen =
            Lists.minimumOn fst (filter ((>last_tracknum) . fst) overlapping)
                <|> Lists.minimumOn fst overlapping
        overlapping = filter (SubT.overlaps pos . snd) notes
        notes = dropWhile ((<=pos) . SubT.end . snd) notes_
    by_track :: [(TrackNum, SubT.EventT a)]
    by_track = Lists.sortOn (SubT.end . snd)
        [ (tracknum, event)
        | (tracknum, track) <- zip [0..] note_tracks, event <- track
        ]

-- | Just cycle the given notes.
simple_tremolo :: [ScoreTime] -> [Derive.NoteDeriver] -> Derive.NoteDeriver
simple_tremolo starts notes = Sub.derive
    [ SubT.EventT start (end - start) note
    | (start, end, note) <- zip3 starts (drop 1 starts) $
        if null notes then [] else cycle notes
    ]

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

c_pitch_trill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Pitch
c_pitch_trill hardcoded_start hardcoded_end =
    Derive.generator1 Module.prelude "tr" mempty
    ("Generate a pitch signal of alternating pitches."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,,,)
    <$> Sig.required "note" "Base pitch."
    <*> neighbor_arg
    <*> config_arg hardcoded_start hardcoded_end
    <*> ControlUtil.curve_env
    <*> transition_env
    ) $ \(note, neighbor, config, curve, transition) args -> do
        (neighbor, control) <- neighbor_to_function (Args.start args) neighbor
        transpose <- get_trill_control config (Args.range_or_next args) neighbor
        transpose <- smooth_trill transition curve transpose
        start <- Args.real_start args
        return $ PSignal.apply_control control (ScoreT.untyped transpose) $
            PSignal.from_sample start note

c_pitch_trill_smooth :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Pitch
c_pitch_trill_smooth hardcoded_start hardcoded_end =
    Derive.generator1 Module.prelude "trs" mempty
    ("Generate a pitch signal of alternating pitches. Like `tr`, but with\
    \ defaults for smooth transitions."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,)
    <$> Sig.required "note" "Base pitch."
    <*> neighbor_arg
    <*> config_arg hardcoded_start hardcoded_end
    ) $ \(note, neighbor, config) args -> do
        (neighbor, control) <- neighbor_to_function (Args.start args) neighbor
        transpose <- get_trill_control_smooth config curve
            (Args.range_or_next args) neighbor
        start <- Args.real_start args
        return $ PSignal.apply_control control (ScoreT.untyped transpose) $
            PSignal.from_sample start note
    where
    curve = ControlUtil.Function $ ControlUtil.sigmoid 0.5 0.5

c_xcut_pitch :: Bool -> Derive.Generator Derive.Pitch
c_xcut_pitch hold = Derive.generator1 Module.prelude "xcut" mempty
    "Cross-cut between two pitches.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> Sig.required "fst" "First pitch."
    <*> Sig.required "snd" "Second pitch."
    <*> Sig.defaulted "speed" (14 :: Int) "Speed."
    ) $ \(xcut1, xcut2, speed) args -> do
        transitions <- Speed.starts speed (Args.range_or_next args) False
        return $ xcut_pitch hold xcut1 xcut2 transitions

xcut_pitch :: Bool -> PSignal.PSignal -> PSignal.PSignal -> [RealTime]
    -> PSignal.PSignal
xcut_pitch hold val1 val2 = mconcat . map slice . zip (cycle [val1, val2])
    where
    slice (val, t)
        | hold = maybe mempty (PSignal.from_sample t) (PSignal.at t val)
        | otherwise = PSignal.clip_before t val


-- * control calls

c_control_trill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Control
c_control_trill hardcoded_start hardcoded_end =
    Derive.generator1 Module.prelude "tr" mempty
    ("The control version of the pitch trill. It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,,)
    <$> Sig.defaulted "neighbor" (1 :: Int) "Alternate with this value."
    <*> config_arg hardcoded_start hardcoded_end
    <*> transition_env
    ) $ \(neighbor, config, transition) args -> do
        transpose <- get_trill_control config (Args.range_or_next args) neighbor
        smooth_trill transition ControlUtil.Linear transpose

c_control_trill_smooth :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Control
c_control_trill_smooth hardcoded_start hardcoded_end =
    Derive.generator1 Module.prelude "tr" mempty
    ("The control version of the pitch trill. It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    <> direction_doc hardcoded_start hardcoded_end
    ) $ Sig.call ((,)
    <$> Sig.defaulted "neighbor" (1 :: Int) "Alternate with this value."
    <*> config_arg hardcoded_start hardcoded_end
    ) $ \(neighbor, config) args -> do
        transpose <- get_trill_control_smooth config curve
            (Args.range_or_next args) neighbor
        return transpose
    where
    curve = ControlUtil.Function $ ControlUtil.sigmoid 0.5 0.5

c_saw :: Derive.Generator Derive.Control
c_saw = Derive.generator1 Module.prelude "saw" mempty
    "Emit a sawtooth.  By default it has a downward slope, but you can make\
    \ an upward slope by setting `from` and `to`."
    $ Sig.call ((,,)
    <$> Speed.arg
    <*> Sig.defaulted "from" (1 :: Double) "Start from this value."
    <*> Sig.defaulted "to" (0 :: Double) "End at this value."
    ) $ \(speed, from, to) args -> do
        starts <- Speed.starts speed (Args.range_or_next args) True
        srate <- Call.get_srate
        return $ saw srate starts from to

saw :: RealTime -> [RealTime] -> Double -> Double -> Signal.Control
saw srate starts from to = mconcat $ zipWith saw starts (drop 1 starts)
    where saw t1 t2 = ControlUtil.segment srate ControlUtil.Linear t1 from t2 to

-- ** sine

data SineMode = Bipolar | Negative | Positive deriving (Show)

-- | This is probably not terribly convenient to use on its own, I should
-- have some more specialized calls based on this.
c_sine :: SineMode -> Derive.Generator Derive.Control
c_sine mode = Derive.generator1 Module.prelude "sine" mempty
    "Emit a sine wave. The default version is centered on the `offset`,\
    \ and the `+` and `-` variants are above and below it, respectively."
    $ Sig.call ((,,)
    <$> Sig.defaulted "speed" (1 :: RealTime) "Frequency."
    <*> Sig.defaulted "amp" (1 :: Double) "Amplitude, measured center to peak."
    <*> Sig.defaulted "offset" (0 :: Double) "Center point."
    ) $ \(Typecheck.RealTimeFunctionT time_type speed, amp, offset) args -> do
        case time_type of
            ScoreT.TScore -> Derive.throw "RealTime signal required"
            _ -> return ()
        srate <- Call.get_srate
        let sign = case mode of
                Bipolar -> 0
                Negative -> -amp
                Positive -> amp
        (start, end) <- Args.real_range_or_next args
        return $ Signal.map_y_linear ((+(offset+sign)) . (*amp)) $
            sine srate start end speed

sine :: RealTime -> RealTime -> RealTime -> ScoreT.Function -> Signal.Control
sine srate start end freq_sig = Signal.unfoldr go (start, 0)
    where
    go (pos, phase)
        | pos >= end = Nothing
        | otherwise = Just ((pos, sin phase), (pos + 1/srate, next_phase))
        where
        next_phase = phase + 1 / RealTime.to_seconds srate * 2*pi * freq_sig pos


-- ** xcut

c_xcut_control :: Bool -> Derive.Generator Derive.Control
c_xcut_control hold = Derive.generator1 Module.prelude "xcut" mempty
    "Cross-cut between two signals.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> Sig.defaulted "fst" (1 :: Int) "First value."
    <*> Sig.defaulted "snd" (0 :: Int) "Second value."
    <*> Sig.defaulted "speed" (14 :: Int) "Speed."
    ) $ \(xcut1, xcut2, speed) args -> do
        transitions <- Speed.starts speed (Args.range_or_next args) False
        return $ xcut_control hold xcut1 xcut2 transitions

-- TODO(polymorphic-signals) This is the same as 'xcut_pitch'
xcut_control :: Bool -> Signal.Control -> Signal.Control -> [RealTime]
    -> Signal.Control
xcut_control hold val1 val2 = mconcat . map slice . zip (cycle [val1, val2])
    where
    slice (val, t)
        | hold = Signal.from_sample t (Signal.at t val)
        | otherwise = Signal.clip_before t val

-- * util

trill_speed_arg :: Sig.Parser Typecheck.RealTimeFunctionT
trill_speed_arg =
    Sig.defaulted "speed" (14 :: Int)
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
instance ShowVal.ShowVal Direction
instance Typecheck.Typecheck Direction
instance Typecheck.ToVal Direction

-- | This is the like 'Direction', but in terms of the unison and neighbor
-- pitches, instead of high and low.
data AbsoluteMode = Unison | Neighbor deriving (Bounded, Eq, Enum, Show)

transition_env :: Sig.Parser ScoreT.Function
transition_env =
    Sig.environ "tr-transition" Sig.Unprefixed (0 :: Int)
    "Take this long to reach the neighbor, as a proportion of time available."

-- | A bundle of standard configuration for trills.
config_arg :: Maybe Direction -> Maybe Direction -> Sig.Parser Config
config_arg start_dir end_dir =
    Config <$> trill_speed_arg <*> start <*> end <*> hold_env <*> adjust_env
        <*> bias <*> pure False
    where
    start = case start_dir of
        Nothing -> Sig.environ "tr-start" Sig.Unprefixed
            (Nothing :: Maybe Direction)
            "Which note the trill starts with. If not given, it will start\
            \ the unison note, which means it may move up or down."
        Just dir -> pure $ Just dir
    end = case end_dir of
        Nothing -> Sig.environ "tr-end" Sig.Unprefixed
            (Nothing :: Maybe Direction)
            "Which note the trill ends with. If not given, it can end with\
            \ either."
        Just dir -> pure $ Just dir
    bias = Typecheck.normalized_bipolar <$>
        Sig.environ "tr-bias" Sig.Unprefixed (Typecheck.NormalizedBipolar 0)
            "Offset every other transition by this amount."

data Config = Config {
    -- | transition speed
    _speed :: !Typecheck.RealTimeFunctionT
    , _start_dir :: !(Maybe Direction)
    , _end_dir :: !(Maybe Direction)
    -- | extend the first transition by this amount
    , _hold :: !DeriveT.Duration
    -- | how to fit the transitions into the time range
    , _adjust :: !Adjust
    -- | offset every other transition by this amount, from -1--1
    , _bias :: !Double
    -- | include a transition at the end time
    , _include_end :: !Bool
    }

-- Its default is both prefixed and unprefixed so you can put in a tr-hold
-- globally, and so you can have a short @hold=n |@ for a single call.
hold_env :: Sig.Parser DeriveT.Duration
hold_env = Typecheck._real <$>
    Sig.environ (Derive.ArgName EnvKey.hold) Sig.Both
        (Typecheck.real 0) "Time to hold the first note."

trill_variations :: Text -> [(Expr.Symbol, Maybe Direction, Maybe Direction)]
trill_variations prefix =
    [ (Expr.Symbol $ prefix
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

-- | How to adjust an ornament to fulfill its 'Direction' restrictions.
data Adjust =
    -- | Adjust by shortening the ornament.
    Shorten
    -- | Adjust by increasing the speed.
    | Stretch
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal Adjust
instance Typecheck.Typecheck Adjust
instance Typecheck.ToVal Adjust

adjust_env :: Sig.Parser Adjust
adjust_env = Sig.environ "adjust" Sig.Both Shorten
    "How to adjust a trill to fulfill its start and end pitch restrictions."

-- ** transitions

-- | A signal that alternates between the base and neighbor values.
get_trill_control :: Config -> (ScoreTime, ScoreTime)
    -> ScoreT.Function -> Derive.Deriver [(RealTime, Signal.Y)]
get_trill_control config (start, end) neighbor = do
    real_start <- Derive.real start
    let neighbor_low = neighbor real_start < 0
    (who_first, transitions) <- get_trill_transitions config (start, end)
        neighbor_low
    let (val1, val2) = case who_first of
            Unison -> (const 0, neighbor)
            Neighbor -> (neighbor, const 0)
    return $ trill_from_transitions val1 val2 real_start transitions

-- | Like 'get_trill_control', but for a curved trill.
get_trill_control_smooth :: Config
    -> ControlUtil.Curve -> (ScoreTime, ScoreTime) -> ScoreT.Function
    -> Derive.Deriver Signal.Control
get_trill_control_smooth config curve range neighbor = do
    transpose <- get_trill_control
        -- Trills usually omit the transition that coincides with the end
        -- because that would create a zero duration note.  But these
        -- trills are smoothed and thus will still have a segment leading
        -- to the cut-off transition.
        (config { _include_end = True })
        range neighbor
    signal <- smooth_trill (const 1) curve transpose
    return signal


-- | The points in time where the trill should transition between pitches.
get_trill_transitions :: Config -> (ScoreTime, ScoreTime) -> Bool
    -> Derive.Deriver (AbsoluteMode, [RealTime])
get_trill_transitions config (start, end) neighbor_low = do
    let (who_first, even_transitions) =
            convert_direction neighbor_low (_start_dir config) (_end_dir config)
    hold <- Call.score_duration start (_hold config)
    (who_first,) <$>
        adjusted_transitions config 0 even_transitions (start + hold, end)

-- | Resolve start and end Directions to the first and second trill notes.
convert_direction :: Bool -> Maybe Direction -> Maybe Direction
    -> (AbsoluteMode, Maybe Bool)
    -- ^ Who starts the trill.  The boolean indicates whether the transitions
    -- should be even to end on the expected end Direction, and Nothing if it
    -- doesn't matter.
convert_direction neighbor_low start end = (first, even_transitions)
    where
    first = case start of
        Nothing -> Unison
        Just Low -> if neighbor_low then Neighbor else Unison
        Just High -> if neighbor_low then Unison else Neighbor
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

-- | Turn transition times into a trill control.
smooth_trill :: ScoreT.Function -- ^ time to take make the transition,
    -- where 0 is instant and 1 is all available time
    -> ControlUtil.Curve
    -> [(RealTime, Signal.Y)]
    -> Derive.Deriver Signal.Control
smooth_trill time curve transitions = do
    srate <- Call.get_srate
    -- I used to optimize sig_function == const 0, but it probably doesn't make
    -- much difference.
    return $ ControlUtil.smooth_relative curve srate time transitions

-- | Get trill transition times, adjusted for all the various fancy parameters
-- that trills have.
adjusted_transitions :: Config
    -> ScoreTime
    -> Maybe Bool -- ^ emit an even number of transitions, or Nothing for
    -- however many will fit
    -> (ScoreTime, ScoreTime) -> Derive.Deriver [RealTime]
adjusted_transitions config hold even (start, end) = do
    real_end <- Derive.real end
    add_hold . add_bias (_bias config)
        . adjust_transitions real_end (_adjust config)
        . trim
        =<< trill_transitions (start + hold, end)
            (_include_end config) (_speed config)
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
    offsets = Lists.range_ 0 stretch
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

-- | Make a trill signal from a list of transition times.  It will alternate
-- between values from the given Functions.
trill_from_transitions :: ScoreT.Function -> ScoreT.Function
    -> RealTime -> [RealTime] -> [(RealTime, Signal.Y)]
trill_from_transitions val1 val2 start transitions =
    initial ++ [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]
    where
    -- Hold might have push the first transition forward, so make a flat
    -- segment for it.
    initial = case transitions of
        x : _ | start < x -> [(start, val1 start)]
        _ -> []

-- | Create trill transition points from a speed.
trill_transitions :: (ScoreTime, ScoreTime) -> Bool
    -> Typecheck.RealTimeFunctionT -> Derive.Deriver [RealTime]
trill_transitions range include_end (Typecheck.RealTimeFunctionT ttype speed) =
    case ttype of
        ScoreT.TReal -> real_transitions range include_end speed
        ScoreT.TScore -> score_transitions range include_end speed

real_transitions :: (ScoreTime, ScoreTime) -> Bool -> ScoreT.Function
    -> Derive.Deriver [RealTime]
real_transitions (start, end) include_end speed = do
    start <- Derive.real start
    end <- Derive.real end
    full_cycles RealTime.eta end include_end <$>
        Speed.real_starts speed start end

score_transitions :: (ScoreTime, ScoreTime) -> Bool -> ScoreT.Function
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
