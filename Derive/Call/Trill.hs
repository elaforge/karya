{-# LANGUAGE ScopedTypeVariables #-}
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
import Util.Control
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, required, typed_control, control)
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import Types


-- * note calls

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("abs-trill", c_absolute_trill)
    , ("score-trill", c_score_trill)
    ]

-- | Absolute time trill.
--
-- [neighbor /Control/] Alternate with this relative pitch.
--
-- [speed /Control/ @%trill-speed,14@] Trill at this many cycles per second.
c_absolute_trill :: Derive.NoteCall
c_absolute_trill = Derive.transformer "absolute_trill" $
    \args deriver -> CallSig.call2 args
    (required "neighbor", optional "speed" (control "trill-speed" 14)) $
    \neighbor speed -> do
        (neighbor_sig, control) <-
            Util.to_transpose_signal Util.Diatonic neighbor
        speed_sig <- Score.typed_val <$> Util.to_signal speed
        transpose <- absolute_trill (Derive.passed_range args) neighbor_sig
            speed_sig
        Derive.with_added_control control (Score.untyped transpose) deriver

absolute_trill :: (ScoreTime, ScoreTime) -> Signal.Control -> Signal.Control
    -> Derive.Deriver Signal.Control
absolute_trill (s_start, s_end) neighbor speed = do
    start <- Derive.real s_start
    end <- Derive.real s_end
    return $ make_trill start end speed neighbor

-- | Trill in score time.  Unlike 'c_absolute_trill', the trill rate will be
-- affected by the current tempo.
--
-- [neighbor /Control/] Alternate with this relative pitch.
--
-- [speed /Control/ @%trill-speed,14@] Trill at this many cycles per score
-- unit.
c_score_trill :: Derive.NoteCall
c_score_trill = Derive.transformer "score_trill" $
    \args deriver -> CallSig.call2 args
    (required "neighbor", optional "speed" (control "trill-speed" 14)) $
    \neighbor speed -> do
        (neighbor_sig, control) <-
            Util.to_transpose_signal Util.Diatonic neighbor
        speed_sig <- Score.typed_val <$> Util.to_signal speed
        transpose <- score_trill
            (Derive.passed_range args) neighbor_sig speed_sig
        Derive.with_added_control control (Score.untyped transpose) deriver

score_trill :: (ScoreTime, ScoreTime) -> Signal.Control -> Signal.Control
    -> Derive.Deriver Signal.Control
score_trill (start, end) neighbor speed = do
    all_transitions <- score_pos_at_speed speed start end
    let transitions = integral_cycles end all_transitions
    real_transitions <- mapM Derive.real transitions
    return $ trill_from_transitions real_transitions neighbor

-- | Emit ScoreTimes at the given speed, which may change over time.  The
-- ScoreTimes are emitted as the reciprocal of the signal at the given point
-- in time.
--
-- The result is that the speed of the emitted samples should depend on the
-- tempo in effect.
score_pos_at_speed :: Signal.Control -> ScoreTime -> ScoreTime
    -> Derive.Deriver [ScoreTime]
score_pos_at_speed sig pos end
    | pos > end = return []
    | otherwise = do
        real <- Derive.real pos
        let speed = Signal.y_to_score (Signal.at real sig)
        rest <- score_pos_at_speed sig (pos + recip speed) end
        return (pos : rest)


-- * pitch calls

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("tr", c_pitch_absolute_trill)
    , ("`tr`", c_pitch_absolute_trill)
    , ("abs-trill", c_pitch_absolute_trill)
    ]

c_pitch_absolute_trill :: Derive.PitchCall
c_pitch_absolute_trill = Derive.generator1 "pitch_absolute_trill" $ \args ->
    CallSig.call3 args (required "note",
        optional "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic),
        optional "speed" (control "trill-speed" 14)) $
    \note neighbor speed -> do
        speed_sig <- Score.typed_val <$> Util.to_signal speed
        (neighbor_sig, control) <-
            Util.to_transpose_signal Util.Diatonic neighbor
        start <- Derive.passed_real args
        end <- Derive.real (Derive.passed_event_end args)
        let transpose = make_trill start end speed_sig neighbor_sig
        PitchSignal.apply_control control (Score.untyped transpose) <$>
            Util.pitch_signal [(0, note)]


-- * util

-- | Make a trill transposition signal.
make_trill :: RealTime -> RealTime -> Signal.Control -> Signal.Control
    -> Signal.Control
make_trill start end speed neighbor =
    trill_from_transitions transitions neighbor
    where transitions = integral_cycles end (real_pos_at_speed speed start)

-- | Emit an infinite list of RealTimes at the given speed, which may change
-- over time.  The speed is taken as hertz in real time.
real_pos_at_speed :: Signal.Control -> RealTime -> [RealTime]
real_pos_at_speed sig pos =
    pos : real_pos_at_speed sig (pos + Signal.y_to_real (recip speed))
    where speed = Signal.at pos sig

trill_from_transitions :: [RealTime] -> Signal.Control -> Signal.Control
trill_from_transitions transitions neighbor =
    Signal.signal [(x, if t then Signal.at x neighbor else 0)
        | (x, t) <- zip transitions (cycle [False, True])]

make_square :: [RealTime] -> Signal.Control
make_square xs = Signal.signal (zip xs (cycle [0, 1]))

integral_cycles :: (Ord a) => a -> [a] -> [a]
integral_cycles end (x0:x1:x2:xs)
    | x2 > end = [x0]
    | otherwise = x0 : x1 : integral_cycles end (x2:xs)
integral_cycles _ xs = take 1 xs
