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
module Derive.Call.Trill where
import qualified Control.Applicative as Applicative
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required, typed_control, control, pitch)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * note calls

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("tr", c_note_trill)
    , ("`tr`", c_note_trill)
    , ("trem", c_tremolo_generator)
    ]
    [ ("trem", c_tremolo_transformer) ]

c_note_trill :: Derive.Generator Derive.Note
c_note_trill = Derive.make_call "trill" (Tags.ornament <> Tags.ly)
    ("Generate a note with a trill.\
    \\nUnlike a trill on a pitch track, this generates events for each\
    \ note of the trill. This is more appropriate for fingered trills,\
    \ or monophonic instruments that use legato to play slurred notes."
    ) $ Sig.call ((,,)
    <$> defaulted "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic)
        "Alternate with a pitch at this interval."
    <*> trill_speed_arg
    <*> mode_arg Nothing
    ) $ \(neighbor, speed, mode) -> Sub.inverting $ \args ->
    Lily.note_code (Lily.SuffixFirst, "\\trill") args $ do
        (transpose, control) <- trill_from_controls
            (Args.start args, Args.end args) mode neighbor speed
        xs <- mapM (Derive.score . fst) (Signal.unsignal transpose)
        let end = snd $ Args.range args
        let notes = do
                (x, maybe_next) <- Seq.zip_next xs
                let next = fromMaybe end maybe_next
                return $ Sub.Event x (next-x) Util.note
        Derive.with_added_control control (Score.untyped transpose) $
            Sub.place notes

c_attr_trill :: Derive.Generator Derive.Note
c_attr_trill = Derive.make_call "attr-trill" (Tags.ornament <> Tags.attr)
    "Generate a trill by adding a `+trill` attribute. Presumably this is a\
    \ sampled instrument that has a trill keyswitch."
    $ Sig.call
    (defaulted "neighbor" (typed_control "trill-neighbor" 1 Score.Chromatic)
        "Alternate with a pitch at this interval.  Only 1c and 2c are allowed."
    ) $ \neighbor args -> do
        (width, typ) <- Util.transpose_control_at Util.Chromatic neighbor
            =<< Args.real_start args
        width_attr <- case (width, typ) of
            (1, Util.Chromatic) -> return Attrs.half
            (2, Util.Chromatic) -> return Attrs.whole
            _ -> Derive.throw $
                "attribute trill only supports 1c and 2c trills: "
                <> untxt (ShowVal.show_val neighbor)
        Util.add_attrs (Attrs.trill <> width_attr) (Util.placed_note args)

c_tremolo_generator :: Derive.Generator Derive.Note
c_tremolo_generator = Derive.make_call "trem" (Tags.ornament <> Tags.ly)
    "Repeat a single note." $ Sig.call Speed.arg $ \speed args -> do
        starts <- tremolo_starts speed (Args.range_or_next args)
        notes <- Sub.sub_events args
        case filter (not . null) notes of
            [] -> Sub.inverting_args args $ Lily.note_code code args $
                simple_tremolo starts [Util.note]
            notes -> Lily.notes_code code args $ chord_tremolo starts notes
    where code = (Lily.SuffixAll, ":32")

c_tremolo_transformer :: Derive.Transformer Derive.Note
c_tremolo_transformer = Derive.transformer "trem"
    (Tags.ornament <> Tags.subs)
    "Repeat the transformed note. The generator is creating the notes so it\
    \ can set them to the appropriate duration, but this one has to stretch\
    \ them to fit." $ Sig.callt Speed.arg $ \speed args deriver -> do
        starts <- tremolo_starts speed (Args.range_or_next args)
        simple_tremolo starts [Args.normalized args deriver]

tremolo_starts :: TrackLang.ValControl -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [ScoreTime]
tremolo_starts speed range = do
    (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
    case time_type of
        Util.Real -> do
            start <- Derive.real (fst range)
            end <- Derive.real (snd range)
            mapM Derive.score . full_notes end
                =<< Speed.real_starts speed_sig start end
        Util.Score -> do
            let (start, end) = range
            starts <- Speed.score_starts speed_sig start end
            return $ full_notes end starts

-- | Alternate each note with the other notes within its range, in order from
-- the lowest track to the highest.
--
-- This doesn't restart the tremolo when a new note enters, if you want that
-- you can have multiple tremolo events.
--
-- TODO Optionally, extend each note to the next time that note occurs.
chord_tremolo :: [ScoreTime] -> [[Sub.Event]] -> Derive.NoteDeriver
chord_tremolo starts note_tracks =
    Sub.place $ concat $ snd $
        List.mapAccumL emit (-1, by_track) $ zip starts (drop 1 starts)
    where
    emit (tracknum, notes_) (pos, next_pos) = case chosen of
            Nothing -> ((tracknum, notes), [])
            Just (tracknum, note) -> ((tracknum, notes),
                [Sub.Event pos (next_pos-pos) (Sub.event_deriver note)])
        where
        chosen = Seq.minimum_on fst (filter ((>tracknum) . fst) overlapping)
            `mplus` Seq.minimum_on fst overlapping
        overlapping = filter (Sub.event_overlaps pos . snd) notes
        notes = dropWhile ((<=pos) . Sub.event_end . snd) notes_
    by_track = Seq.sort_on (Sub.event_end . snd)
        [(tracknum, note) | (tracknum, track) <- zip [0..] note_tracks,
            note <- track]

-- | Just cycle the given notes.
simple_tremolo :: [ScoreTime] -> [Derive.NoteDeriver] -> Derive.NoteDeriver
simple_tremolo starts notes = Sub.place
    [ Sub.Event start (end - start) note
    | (start, end, note) <- zip3 starts (drop 1 starts) $
        if null notes then [] else cycle notes
    ]

-- | This is defined here instead of in "Derive.Call.Attribute" so it can be
-- next to 'c_tremolo'.
c_attr_tremolo :: Make.Calls Derive.Note
c_attr_tremolo = Make.attributed_note Attrs.trem

-- | This is the tremolo analog to 'full_cycles'.  Unlike a trill, it emits
-- both the starts and ends, and therefore the last sample will be at the end
-- time, rather than before it.  It should always emit an even number of
-- elements.
full_notes :: (Ord a) => a -> [a] -> [a]
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
pitch_calls = Derive.call_maps
    [ ("tr", c_pitch_trill Nothing)
    , ("tr1", c_pitch_trill (Just Unison))
    , ("tr2", c_pitch_trill (Just Neighbor))
    , ("`tr`", c_pitch_trill Nothing)
    , ("xcut", c_xcut_pitch False)
    , ("xcut-h", c_xcut_pitch True)
    ]
    []

c_pitch_trill :: Maybe Mode -> Derive.Generator Derive.Pitch
c_pitch_trill maybe_mode = Derive.generator1 "trill" Tags.ornament
    ("Generate a pitch signal of alternating pitches. `tr1` will start with\
    \ the unison, while `tr2` will start with the neighbor. `tr` is\
    \ configurabled with the environment."
    ) $ Sig.call ((,,,)
    <$> required "note" "Base pitch."
    <*> defaulted "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic)
        "Alternate with a pitch at this interval."
    <*> trill_speed_arg
    <*> mode_arg maybe_mode
    ) $ \(note, neighbor, speed, mode) args -> do
        (transpose, control) <- trill_from_controls (Args.range_or_next args)
            mode neighbor speed
        start <- Args.real_start args
        return $ PitchSignal.apply_control control (Score.untyped transpose) $
            PitchSignal.signal [(start, note)]

c_xcut_pitch :: Bool -> Derive.Generator Derive.Pitch
c_xcut_pitch hold = Derive.generator1 "xcut" mempty
    "Cross-cut between two pitches.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> defaulted "val1" (pitch "xcut1") "First pitch."
    <*> defaulted "val2" (pitch "xcut2") "Second pitch."
    <*> defaulted "speed" (typed_control "xcut-speed" 14 Score.Real) "Speed."
    ) $ \(val1, val2, speed) args -> do
        transitions <- Speed.starts speed (Args.range_or_next args) False
        val1 <- Util.to_pitch_signal val1
        val2 <- Util.to_pitch_signal val2
        return $ xcut_pitch hold val1 val2 transitions

xcut_pitch :: Bool -> PitchSignal.Signal -> PitchSignal.Signal -> [RealTime]
    -> PitchSignal.Signal
xcut_pitch hold val1 val2 =
    mconcat . snd . List.mapAccumL slice (val1, val2) . Seq.zip_next
    where
    slice (val1, val2) (t, next_t)
        | hold = (next, initial)
        | otherwise = (next, initial <> chunk)
        where
        chopped = PitchSignal.drop_before_strict t val1
        chunk = maybe chopped (\n -> PitchSignal.drop_after n chopped) next_t
        next = (val2, PitchSignal.drop_before t val1)
        initial = case PitchSignal.at t val1 of
            Nothing -> mempty
            Just p -> PitchSignal.signal [(t, p)]


-- * control calls

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.call_maps
    [ ("tr", c_control_trill Nothing)
    , ("tr1", c_control_trill (Just Unison))
    , ("tr2", c_control_trill (Just Neighbor))
    , ("saw", c_saw)
    , ("sine", c_sine Bipolar)
    , ("sine+", c_sine Positive)
    , ("sine-", c_sine Negative)
    , ("xcut", c_xcut_control False)
    , ("xcut-h", c_xcut_control True)
    ]
    []

-- | The control version of 'c_pitch_trill'.  It generates a signal of values
-- alternating with 0, and can be used in a transposition signal.
--
-- Args are the same as 'c_pitch_trill'.
c_control_trill :: Maybe Mode -> Derive.Generator Derive.Control
c_control_trill maybe_mode = Derive.generator1 "trill" Tags.ornament
    ("The control version of the pitch trill.  It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    ) $ Sig.call ((,,)
    <$> defaulted "neighbor" (control "trill-neighbor" 1)
        "Alternate with this value."
    <*> trill_speed_arg
    <*> mode_arg maybe_mode
    ) $ \(neighbor, speed, mode) args ->
        fst <$> trill_from_controls (Args.start args, Args.next args)
            mode neighbor speed

trill_speed_arg :: Sig.Parser TrackLang.ValControl
trill_speed_arg = defaulted "speed" (typed_control "trill-speed" 14 Score.Real)
    "Trill at this speed. If it's a RealTime, the value is the number of\
    \ cycles per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of cycles per ScoreTime\
    \ unit, and will stretch along with tempo changes. In either case,\
    \ this will emit only whole notes, i.e. a note will not be cut short sooner\
    \ than it should according to the speed."

c_saw :: Derive.Generator Derive.Control
c_saw = Derive.generator1 "saw" Tags.ornament
    "Emit a sawtooth.  By default it has a downward slope, but you can make\
    \ an upward slope by setting `from` and `to`."
    $ Sig.call ((,,)
    <$> Speed.arg
    <*> defaulted "from" 1 "Start from this value."
    <*> defaulted "to" 0 "End at this value."
    ) $ \(speed, from, to) args -> do
        starts <- Speed.starts speed (Args.range_or_next args) True
        srate <- Util.get_srate
        return $ saw srate starts from to

saw :: RealTime -> [RealTime] -> Double -> Double -> Signal.Control
saw srate starts from to =
    mconcat $ zipWith saw starts (drop 1 starts)
    where
    saw t1 t2 = Control.interpolate_segment
        True srate id t1 from (t2-srate) to

-- ** sine

data SineMode = Bipolar | Negative | Positive deriving (Show)

-- | This is probably not terribly convenient to use on its own, I should
-- have some more specialized calls based on this.
c_sine :: SineMode -> Derive.Generator Derive.Control
c_sine mode = Derive.generator1 "sine" Tags.ornament
    "Emit a sine wave. The default version is centered on the `offset`,\
    \ and the `+` and `-` variants are above and below it, respectively."
    $ Sig.call ((,,)
    <$> defaulted "speed" (typed_control "sine-speed" 1 Score.Real) "Frequency."
    <*> defaulted "amp" 1 "Amplitude, measured center to peak."
    <*> defaulted "offset" 0 "Center point."
    ) $ \(speed, amp, offset) args -> do
        (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
        case time_type of
            Util.Score -> Derive.throw "RealTime signal required"
            _ -> return ()
        srate <- Util.get_srate
        let sign = case mode of
                Bipolar -> 0
                Negative -> -amp
                Positive -> amp
        (start, end) <- Args.real_range_or_next args
        -- let samples = Seq.range' start end srate
        return $ Signal.map_y ((+(offset+sign)) . (*amp)) $
            sine srate start end speed_sig

sine :: RealTime -> RealTime -> RealTime -> Signal.Control -> Signal.Control
sine srate start end freq_sig = Signal.unfoldr go (start, 0)
    where
    go (pos, phase)
        | pos >= end = Nothing
        | otherwise = Just ((pos, sin phase), (pos + srate, next_phase))
        where
        freq = Signal.at pos freq_sig
        next_phase = phase + RealTime.to_seconds srate * 2*pi * freq


-- ** xcut

c_xcut_control :: Bool -> Derive.Generator Derive.Control
c_xcut_control hold = Derive.generator1 "xcut" mempty
    "Cross-cut between two signals.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> defaulted "val1" (control "xcut1" 1) "First value."
    <*> defaulted "val2" (control "xcut2" 0) "Second value."
    <*> defaulted "speed" (typed_control "xcut-speed" 14 Score.Real) "Speed."
    ) $ \(val1, val2, speed) args -> do
        transitions <- Speed.starts speed (Args.range_or_next args) False
        val1 <- Util.to_untyped_signal val1
        val2 <- Util.to_untyped_signal val2
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
        chunk = maybe chopped (\n -> Signal.drop_after n chopped) next_t
        next = (val2, Signal.drop_before t val1)
        initial = Signal.signal [(t, Signal.at t val1)]

-- * util

data Mode = Unison | Neighbor deriving (Eq, Show)

instance ShowVal.ShowVal Mode where
    show_val Unison = "unison"
    show_val Neighbor = "neighbor"

instance TrackLang.TypecheckSymbol Mode where
    parse_symbol t
        | t == ShowVal.show_val Unison = Just Unison
        | t == ShowVal.show_val Neighbor = Just Neighbor
        | otherwise = Nothing

mode_arg :: Maybe Mode -> Sig.Parser Mode
mode_arg (Just mode) = Applicative.pure mode
mode_arg Nothing = TrackLang.get_s <$>
    Sig.environ "trill-mode" Sig.Unprefixed (TrackLang.S Unison)
    "This affects which note the trill starts with, and can be `unison` or\
    \ `neighbor`, defaulting to `unison`."

-- | Create a transposition signal from neighbor and speed controls.
trill_from_controls :: (ScoreTime, ScoreTime) -> Mode -> TrackLang.ValControl
    -> TrackLang.ValControl -> Derive.Deriver (Signal.Control, Score.Control)
trill_from_controls range mode neighbor speed = do
    transitions <- trill_transitions range False speed
    (neighbor_sig, control) <- Util.to_transpose_signal Util.Diatonic neighbor
    let transpose = trill_from_transitions mode transitions neighbor_sig
    return (transpose, control)

-- | Create trill transition points from a speed.
trill_transitions :: (ScoreTime, ScoreTime) -> Bool -> TrackLang.ValControl
    -> Derive.Deriver [RealTime]
trill_transitions range include_end speed = do
    (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
    case time_type of
        Util.Real -> real_transitions range include_end speed_sig
        Util.Score -> score_transitions range include_end speed_sig

real_transitions :: (ScoreTime, ScoreTime) -> Bool -> Signal.Control
    -> Derive.Deriver [RealTime]
real_transitions (start, end) include_end speed = do
    start <- Derive.real start
    end <- Derive.real end
    full_cycles RealTime.eta end include_end <$>
        Speed.real_starts speed start end

score_transitions :: (ScoreTime, ScoreTime) -> Bool -> Signal.Control
    -> Derive.Deriver [RealTime]
score_transitions (start, end) include_end speed = do
    all_transitions <- Speed.score_starts speed start end
    mapM Derive.real $ full_cycles ScoreTime.eta end include_end all_transitions

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: Mode -> [RealTime] -> Signal.Control -> Signal.Control
trill_from_transitions mode transitions neighbor =
    Signal.signal [(x, if t then Signal.at x neighbor else 0)
        | (x, t) <- zip transitions (cycle ts)]
    where
    ts = case mode of
        Unison -> [False, True]
        Neighbor -> [True, False]

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
