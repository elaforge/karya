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
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted, required, typed_control, control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * note calls

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("tr", c_note_trill)
    , ("`tr`", c_note_trill)
    , ("trem", c_tremolo)
    ]

c_note_trill :: Derive.NoteCall
c_note_trill = Derive.stream_generator "trill" (Tags.ornament <> Tags.ly)
    ("Generate a note with a trill.\
    \\nUnlike a trill on a pitch track, this generates events for each\
    \ note of the trill. This is more appropriate for fingered trills,\
    \ or monophonic instruments that use legato to play slurred notes."
    ) $ Sig.call ((,)
    <$> defaulted "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic)
        "Alternate with a pitch at this interval."
    <*> trill_speed_arg
    ) $ \(neighbor, speed) -> Note.inverting $ \args ->
    Lily.note_code (Lily.SuffixFirst, "\\trill") args $ do
        mode <- get_mode
        (transpose, control) <- trill_from_controls
            (Args.start args, Args.end args) mode neighbor speed
        xs <- mapM (Derive.score . fst) (Signal.unsignal transpose)
        let end = snd $ Args.range args
        let notes = do
                (x, maybe_next) <- Seq.zip_next xs
                let next = fromMaybe end maybe_next
                return $ Note.Event x (next-x) Util.note
        Derive.with_added_control control (Score.untyped transpose) $
            Note.place notes

c_attr_trill :: Derive.NoteCall
c_attr_trill = Derive.stream_generator "attr-trill" (Tags.ornament <> Tags.attr)
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

c_tremolo :: Derive.NoteCall
c_tremolo = Derive.Call
    { Derive.call_name = "tremolo"
    , Derive.call_generator = Just $ Derive.generator_call
        (Tags.ornament <> Tags.ly) "Repeat a single note." generator
    , Derive.call_transformer = Just $ Derive.transformer_call
        (Tags.ornament <> Tags.subs)
        "Repeat the transformed note. The generator is creating the notes so it\
        \ can set them to the appropriate duration, but this one has to stretch\
        \ them to fit." transformer
    }
    where
    speed_arg = defaulted "speed" (typed_control "tremolo-speed" 10 Score.Real)
        "Tremolo at this speed. Its meaning is the same as the trill speed."
    transformer = Sig.callt speed_arg $ \speed args deriver -> do
        starts <- tremolo_starts speed (Args.range_or_next args)
        simple_tremolo starts [Args.normalized args deriver]
    generator = Sig.call speed_arg $ \speed args -> do
        starts <- tremolo_starts speed (Args.range_or_next args)
        notes <- Note.sub_events args
        case filter (not . null) notes of
            [] -> Note.inverting_args args $ Lily.note_code code args $
                simple_tremolo starts [Util.note]
            notes -> Lily.notes_code code args $ chord_tremolo starts notes
    code = (Lily.SuffixAll, ":32")

tremolo_starts :: TrackLang.ValControl -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [ScoreTime]
tremolo_starts speed range = do
    (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
    case time_type of
        Util.Real -> do
            start <- Derive.real (fst range)
            end <- Derive.real (snd range)
            mapM Derive.score $ take_full_notes end $
                real_pos_at_speed speed_sig start
        Util.Score -> do
            let (start, end) = range
            starts <- score_pos_at_speed speed_sig start end
            return $ take_full_notes end starts

-- | Alternate each note with the other notes within its range, in order from
-- the lowest track to the highest.
--
-- This doesn't restart the tremolo when a new note enters, if you want that
-- you can have multiple tremolo events.
--
-- TODO Optionally, extend each note to the next time that note occurs.
chord_tremolo :: [ScoreTime] -> [[Note.Event]] -> Derive.EventDeriver
chord_tremolo starts note_tracks =
    Note.place $ concat $ snd $
        List.mapAccumL emit (-1, by_track) $ zip starts (drop 1 starts)
    where
    emit (tracknum, notes_) (pos, next_pos) = case chosen of
            Nothing -> ((tracknum, notes), [])
            Just (tracknum, note) -> ((tracknum, notes),
                [Note.Event pos (next_pos-pos) (Note.event_deriver note)])
        where
        chosen = Seq.minimum_on fst (filter ((>tracknum) . fst) overlapping)
            `mplus` Seq.minimum_on fst overlapping
        overlapping = filter (Note.event_overlaps pos . snd) notes
        notes = dropWhile ((<=pos) . Note.event_end . snd) notes_
    by_track = Seq.sort_on (Note.event_end . snd)
        [(tracknum, note) | (tracknum, track) <- zip [0..] note_tracks,
            note <- track]

-- | Just cycle the given notes.
simple_tremolo :: [ScoreTime] -> [Derive.EventDeriver] -> Derive.EventDeriver
simple_tremolo starts notes =
    Note.place [Note.Event start (end - start) note
        | (start, end, note) <- zip3 starts (drop 1 starts)
            (if null notes then [] else cycle notes)]

-- | Only over here instead of in "Derive.Call.Attribute" so it can be next to
-- 'c_tremolo'.
c_attr_tremolo :: Derive.NoteCall
c_attr_tremolo = Make.attributed_note Attrs.trem

take_full_notes :: (Ord a) => a -> [a] -> [a]
take_full_notes _ [] = []
take_full_notes end (t:ts) = t : go ts
    where
    go (t1 : ts@(t2:_))
        | t2 > end = [end]
        | otherwise = t1 : go ts
    go ts = ts

-- * pitch calls

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("tr", c_pitch_trill Nothing)
    , ("tr1", c_pitch_trill (Just UnisonFirst))
    , ("tr2", c_pitch_trill (Just NeighborFirst))
    , ("`tr`", c_pitch_trill Nothing)
    , ("`tr`1", c_pitch_trill (Just UnisonFirst))
    , ("`tr`2", c_pitch_trill (Just NeighborFirst))
    ]

c_pitch_trill :: Maybe Mode -> Derive.PitchCall
c_pitch_trill maybe_mode = Derive.generator1 "pitch-trill" Tags.ornament
    ("Generate a pitch signal of alternating pitches. `tr1` will start with\
    \ the unison, while `tr2` will start with the neighbor. `tr` will\
    \ use the `trill-mode` env var, which should be either `'unison'`\
    \ or `'neighbor'`, defaulting to unison."
    ) $ Sig.call ((,,)
    <$> required "note" "Base pitch."
    <*> defaulted "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic)
        "Alternate with a pitch at this interval."
    <*> trill_speed_arg
    ) $ \(note, neighbor, speed) args -> do
        mode <- maybe get_mode return maybe_mode
        (transpose, control) <- trill_from_controls
            (Args.start args, Args.next args) mode neighbor speed
        start <- Args.real_start args
        PitchSignal.apply_control control (Score.untyped transpose) <$>
            Util.pitch_signal [(start, note)]


-- * control calls

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("tr", c_control_trill Nothing)
    , ("tr1", c_control_trill (Just UnisonFirst))
    , ("tr2", c_control_trill (Just NeighborFirst))
    , ("tr1", c_control_trill (Just UnisonFirst))
    , ("tr2", c_control_trill (Just NeighborFirst))
    , ("saw", c_sawtooth)
    , ("sine", c_sine Bipolar)
    , ("sine+", c_sine Positive)
    , ("sine-", c_sine Negative)
    ]

-- | The control version of 'c_pitch_trill'.  It generates a signal of values
-- alternating with 0, and can be used in a transposition signal.
--
-- Args are the same as 'c_pitch_trill'.
c_control_trill :: Maybe Mode -> Derive.ControlCall
c_control_trill maybe_mode = Derive.generator1 "control-trill" Tags.ornament
    ("The control version of the pitch trill.  It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    ) $ Sig.call ((,)
    <$> defaulted "neighbor" (control "trill-neighbor" 1)
        "Alternate with this value."
    <*> trill_speed_arg
    ) $ \(neighbor, speed) args -> do
        mode <- maybe get_mode return maybe_mode
        fst <$> trill_from_controls (Args.start args, Args.next args)
            mode neighbor speed

trill_speed_arg :: Sig.Parser TrackLang.ValControl
trill_speed_arg = defaulted "speed" (typed_control "trill-speed" 14 Score.Real)
    "Trill at this speed. If it's a RealTime, the value is the number of\
    \ cycles per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of cycles per ScoreTime\
    \ unit, and will stretch along with tempo changes. In either case,\
    \ this will emit an integral number of cycles."

c_sawtooth :: Derive.ControlCall
c_sawtooth = Derive.generator1 "sawtooth" Tags.ornament
    "Emit a sawtooth.  By default it has a downward slope, but you can make\
    \ an upward slope by setting `from` and `to`."
    $ Sig.call ((,,)
    <$> defaulted "speed" (typed_control "saw-speed" 10 Score.Real)
        "Repeat at this speed. Its meaning is the same as the trill speed."
    <*> defaulted "from" 1 "Start from this value."
    <*> defaulted "to" 0 "End at this value."
    ) $ \(speed, from, to) args -> do
        starts <- speed_starts speed (Args.range_or_next args)
        srate <- Util.get_srate
        return $ sawtooth srate starts from to

sawtooth :: RealTime -> [RealTime] -> Double -> Double -> Signal.Control
sawtooth srate starts from to =
    Signal.signal $ concatMap saw (zip starts (drop 1 starts))
    where saw (t1, t2) = Control.interpolate_list srate id t1 from (t2-srate) to

data SineMode = Bipolar | Negative | Positive deriving (Show)

-- | This is probably not terribly convenient to use on its own, I should
-- have some more specialized calls based on this.
c_sine :: SineMode -> Derive.ControlCall
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

-- | Get start times before the end of the range, at the given speed.
speed_starts :: TrackLang.ValControl -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [RealTime]
speed_starts speed range = do
    (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
    case time_type of
        Util.Real -> do
            start <- Derive.real (fst range)
            end <- Derive.real (snd range)
            return $ takeWhile (<=end) $
                real_pos_at_speed speed_sig start
        Util.Score -> do
            let (start, end) = range
            starts <- score_pos_at_speed speed_sig start end
            mapM Derive.real $ takeWhile (<=end) starts


-- * util

data Mode = UnisonFirst | NeighborFirst deriving (Show)

get_mode :: Derive.Deriver Mode
get_mode = do
    mode_name :: Maybe Text <- Derive.lookup_val (TrackLang.Symbol "trill-mode")
    case mode_name of
        Nothing -> return UnisonFirst
        Just name
            | name == "unison" -> return UnisonFirst
            | name == "neighbor" -> return NeighborFirst
            | otherwise -> Derive.throw $ "unknown trill mode: " ++ show name

-- | Create a transposition signal from neighbor and speed controls.
trill_from_controls :: (ScoreTime, ScoreTime) -> Mode -> TrackLang.ValControl
    -> TrackLang.ValControl -> Derive.Deriver (Signal.Control, Score.Control)
trill_from_controls range mode neighbor speed = do
    (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
    (neighbor_sig, control) <- Util.to_transpose_signal Util.Diatonic neighbor
    transpose <- time_trill time_type mode range neighbor_sig speed_sig
    return (transpose, control)
    where
    time_trill Util.Real = real_trill
    time_trill Util.Score = score_trill

real_trill :: Mode -> (ScoreTime, ScoreTime) -> Signal.Control
    -> Signal.Control -> Derive.Deriver Signal.Control
real_trill mode (start, end) neighbor speed = do
    start <- Derive.real start
    end <- Derive.real end
    return $ make_trill mode start end neighbor speed

score_trill :: Mode -> (ScoreTime, ScoreTime) -> Signal.Control
    -> Signal.Control -> Derive.Deriver Signal.Control
score_trill mode (start, end) neighbor speed = do
    all_transitions <- score_pos_at_speed speed start end
    let transitions = integral_cycles ScoreTime.eta end all_transitions
    real_transitions <- mapM Derive.real transitions
    return $ trill_from_transitions mode real_transitions neighbor

-- | Emit ScoreTimes at the given speed, which may change over time.  The
-- ScoreTimes are emitted as the reciprocal of the signal at the given point
-- in time.
--
-- The result is that the speed of the emitted samples should depend on the
-- tempo in effect.
score_pos_at_speed :: Signal.Control -> ScoreTime -> ScoreTime
    -> Derive.Deriver [ScoreTime]
score_pos_at_speed sig start end
    | start > end = return []
    | otherwise = do
        real <- Derive.real start
        let speed = Signal.y_to_score (Signal.at real sig)
        rest <- score_pos_at_speed sig (start + recip speed) end
        return (start : rest)

-- | Make a trill transposition signal.
make_trill :: Mode -> RealTime -> RealTime -> Signal.Control -> Signal.Control
    -> Signal.Control
make_trill mode start end neighbor speed =
    trill_from_transitions mode transitions neighbor
    where transitions = integral_cycles RealTime.eta end (real_pos_at_speed speed start)

-- | Emit an infinite list of RealTimes at the given speed, which may change
-- over time.  The speed is taken as hertz in real time.
real_pos_at_speed :: Signal.Control -> RealTime -> [RealTime]
real_pos_at_speed sig start =
    start : real_pos_at_speed sig (start + Signal.y_to_real (recip speed))
    where speed = Signal.at start sig

-- | Make a trill signal from a list of transition times.
trill_from_transitions :: Mode -> [RealTime] -> Signal.Control -> Signal.Control
trill_from_transitions mode transitions neighbor =
    Signal.signal [(x, if t then Signal.at x neighbor else 0)
        | (x, t) <- zip transitions (cycle ts)]
    where
    ts = case mode of
        UnisonFirst -> [False, True]
        NeighborFirst -> [True, False]

make_square :: [RealTime] -> Signal.Control
make_square xs = Signal.signal (zip xs (cycle [0, 1]))

-- | Given a list of trill transition times, take only complete cycles (pairs)
-- that fall before the end time.  A bit of eta is to ensure that a transition
-- that almost lines up with the end doesn't result in a super short note.
integral_cycles :: (Ord a, Num a) => a -> a -> [a] -> [a]
integral_cycles eta end (x0:x1:x2:xs)
    -- This is what makes trills include the end, as documented in the module
    -- haddock.
    | x2 + eta >= end = [x0]
    | otherwise = x0 : x1 : integral_cycles eta end (x2:xs)
integral_cycles _ _ xs = take 1 xs
