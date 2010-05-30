{- | Various kinds of trills.

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
import Ui

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (optional, required, control)

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("abs-trill", c_absolute_trill)
    , ("score-trill", c_score_trill)
    ]


-- * note calls

-- | Absolute time trill.
--
-- [neighbor /Control/] Alternate with this relative pitch.
--      TODO should be a pitch signal
--
-- [speed /Control/ @%trill-speed,14@] Trill at this many cycles per second.
c_absolute_trill :: Derive.NoteCall
c_absolute_trill = Derive.transformer "absolute_trill" $
    \args deriver -> TrackLang.call2 args
    (required "neighbor", optional "speed" (control "trill-speed" 14)) $
    \neighbor speed -> do
        neighbor_sig <- Call.to_signal neighbor
        speed_sig <- Call.to_signal speed
        absolute_trill neighbor_sig speed_sig deriver

absolute_trill :: Signal.Control -> Signal.Control -> Derive.Transformer
absolute_trill neighbor speed deriver = do
    real_start <- Derive.now
    real_end <- Derive.score_to_real 1
    let all_transitions = pos_at_speed speed real_start
    let transitions = integral_cycles real_end all_transitions
    trill_from_transitions transitions neighbor deriver

pos_at_speed :: Signal.Control -> RealTime -> [RealTime]
pos_at_speed sig pos = pos : pos_at_speed sig (pos + Signal.y_to_real (1/speed))
    where speed = Signal.at pos sig

-- | Trill in score time.  Unlike 'c_absolute_trill', the trill rate will be
-- affected by the current tempo.
--
-- [neighbor /Control/] Alternate with this relative pitch.
--
-- [speed /Control/ @%trill-speed,14@] Trill at this many cycles per score unit.
c_score_trill :: Derive.NoteCall
c_score_trill = Derive.transformer "score_trill" $
    \args deriver -> TrackLang.call2 args
    (required "neighbor", optional "speed" (control "trill-speed" 14)) $
    \neighbor speed -> do
        neighbor_sig <- Call.to_signal neighbor
        speed_sig <- Call.to_signal speed
        score_trill (TrackLang.passed_stretch args) neighbor_sig speed_sig
            deriver

score_trill :: ScoreTime -> Signal.Control -> Signal.Control
    -> Derive.EventDeriver -> Derive.EventDeriver
score_trill stretch neighbor speed deriver = do
    all_transitions <- score_pos_at_speed stretch speed 0 1
    let transitions = integral_cycles 1 all_transitions
    real_transitions <- mapM Derive.score_to_real transitions
    trill_from_transitions real_transitions neighbor deriver

score_pos_at_speed :: ScoreTime -> Signal.Control -> ScoreTime -> ScoreTime
    -> Derive.Deriver [ScoreTime]
score_pos_at_speed stretch sig pos end
    | pos > end = return []
    | otherwise = do
        real <- Derive.score_to_real pos
        -- If the event stretch is 2, I can double the speed to put it in
        -- score time wrt the track.
        let speed = Signal.y_to_score (Signal.at real sig) * stretch
        rest <- score_pos_at_speed stretch sig (pos + recip speed) end
        return (pos : rest)


-- * pitch calls

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("tr", c_pitch_absolute_trill)
    , ("abs-trill", c_pitch_absolute_trill)
    ]

c_pitch_absolute_trill :: Derive.PitchCall
c_pitch_absolute_trill = Derive.generate_one "pitch_absolute_trill" $
    \args _ _ next -> TrackLang.call3 (Call.default_relative_note args)
    (required "note", optional "neighbor" (control "trill-neighbor" 1),
        optional "speed" (control "trill-speed" 14)) $
    \note neighbor speed -> do
        speed_sig <- Call.to_signal speed
        neighbor_sig <- Call.to_signal neighbor
        next_event <- maybe (return 1) Derive.score_to_real $
            Call.next_event_begin next
        pitch_absolute_trill note speed_sig neighbor_sig next_event

pitch_absolute_trill :: Pitch.Note -> Signal.Control -> Signal.Control
    -> RealTime -> Derive.PitchDeriver
pitch_absolute_trill note speed neighbor dur = do
    degree <- Call.note_to_degree note
    start <- Derive.now
    scale <- Derive.require_val TrackLang.v_scale
    let all_transitions = pos_at_speed speed start
    let transitions = integral_cycles (start + dur) all_transitions
    return $ PitchSignal.shorten start $ PitchSignal.sig_add
            (PitchSignal.constant (Pitch.scale_id scale) degree)
            (make_trill transitions neighbor)



-- * util

trill_from_transitions :: [RealTime] -> Signal.Control
    -> Derive.EventDeriver -> Derive.EventDeriver
trill_from_transitions transitions neighbor = Derive.with_relative_pitch
    PitchSignal.sig_add (make_trill transitions neighbor)

-- | I feel like this should at least return
-- @[(x0, (0, y, 0)), (x1, (0, y, 1)), ...]@, but does it make a difference?
make_trill :: [RealTime] -> Signal.Control -> PitchSignal.Relative
make_trill transitions neighbor = PitchSignal.relative_from_control $
    Signal.sig_multiply (make_square transitions) neighbor

make_square :: [RealTime] -> Signal.Control
make_square xs = Signal.signal (zip xs (cycle [0, 1]))

integral_cycles :: (Ord a) => a -> [a] -> [a]
integral_cycles end (x0:x1:x2:xs)
    | x2 > end = [x0]
    | otherwise = x0 : x1 : integral_cycles end (x2:xs)
integral_cycles _ xs = take 1 xs
