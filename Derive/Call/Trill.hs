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
import qualified Data.List as List

import qualified Util.Num as Num
import Ui
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, required, control)
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale

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
    \args deriver -> CallSig.call2 args
    (required "neighbor", optional "speed" (control "trill-speed" 14)) $
    \neighbor speed -> do
        neighbor_sig <- Util.to_signal neighbor
        speed_sig <- Util.to_signal speed
        absolute_trill (Derive.passed_range args) neighbor_sig speed_sig
            deriver

absolute_trill :: (ScoreTime, ScoreTime) -> Signal.Control -> Signal.Control
    -> Derive.EventDeriver -> Derive.EventDeriver
absolute_trill (s_start, s_end) neighbor speed deriver = do
    start <- Derive.real s_start
    end <- Derive.real s_end
    let all_transitions = pos_at_speed speed start
    let transitions = integral_cycles end all_transitions
    trill_from_transitions transitions neighbor deriver

pos_at_speed :: Signal.Control -> RealTime -> [RealTime]
pos_at_speed sig pos =
    pos : pos_at_speed sig (pos + Signal.y_to_real (1/speed))
    where speed = Signal.at pos sig

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
        neighbor_sig <- Util.to_signal neighbor
        speed_sig <- Util.to_signal speed
        score_trill (Derive.passed_range args) neighbor_sig speed_sig deriver

score_trill :: (ScoreTime, ScoreTime) -> Signal.Control -> Signal.Control
    -> Derive.EventDeriver -> Derive.EventDeriver
score_trill (start, end) neighbor speed deriver = do
    all_transitions <- score_pos_at_speed speed start end
    let transitions = integral_cycles end all_transitions
    real_transitions <- mapM Derive.real transitions
    trill_from_transitions real_transitions neighbor deriver

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
c_pitch_absolute_trill = Derive.generator1 "pitch_absolute_trill" $ \args -> do
    args <- Util.default_relative_note args
    CallSig.call3 args
        (required "degree",
            optional "neighbor" (control "trill-neighbor" 1),
            optional "speed" (control "trill-speed" 14)) $
        \degree neighbor speed -> do
            speed_sig <- Util.to_signal speed
            neighbor_sig <- Util.to_signal neighbor
            start <- Derive.passed_real args
            end <- Derive.real (Derive.passed_event_end args)
            pitch_absolute_trill start degree speed_sig neighbor_sig end

pitch_absolute_trill :: RealTime -> Pitch.Degree -> Signal.Control
    -> Signal.Control -> RealTime -> Derive.Deriver PitchSignal.PitchSignal
pitch_absolute_trill start degree speed neighbor end = do
    scale <- Util.get_scale
    let all_transitions = pos_at_speed speed start
        transitions = integral_cycles end all_transitions
    return $ PitchSignal.drop_before start $ PitchSignal.sig_add
            (PitchSignal.constant (Scale.scale_id scale) degree)
            (make_trill transitions neighbor)


-- * util

trill_from_transitions :: [RealTime] -> Signal.Control
    -> Derive.EventDeriver -> Derive.EventDeriver
trill_from_transitions transitions neighbor = Derive.with_relative_pitch
    Nothing PitchSignal.sig_add (make_trill transitions neighbor)

-- | I feel like this should at least return
-- @[(x0, (0, y, 0)), (x1, (0, y, 1)), ...]@, but does it make a difference?
make_trill :: [RealTime] -> Signal.Control -> PitchSignal.Relative
make_trill transitions neighbor =
    PitchSignal.relative [(x, (0, Num.d2f neighbor, at))
        | (x, neighbor, at) <- List.zip3 transitions neighbors (cycle [0, 1])]
    where neighbors = map (flip Signal.at neighbor) transitions

make_square :: [RealTime] -> Signal.Control
make_square xs = Signal.signal (zip xs (cycle [0, 1]))

integral_cycles :: (Ord a) => a -> [a] -> [a]
integral_cycles end (x0:x1:x2:xs)
    | x2 > end = [x0]
    | otherwise = x0 : x1 : integral_cycles end (x2:xs)
integral_cycles _ xs = take 1 xs
