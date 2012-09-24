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
import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, required, typed_control, control)
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


-- * note calls

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("tr", c_note_trill)
    , ("`tr`", c_note_trill)
    , ("trem", c_tremolo)
    ]

-- | Generate a note with a trill.
--
-- Unlike a trill on a pitch track, this generates events for each note of
-- the trill.  This is more appropriate for fingered trills, or monophonic
-- instruments that use legato to play slurred notes.
--
-- The args are the same as 'c_pitch_trill'.
c_note_trill :: Derive.NoteCall
c_note_trill = Derive.stream_generator "trill" $ Note.inverting $ \args ->
    CallSig.call2 args (
        optional "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic),
        optional "speed" (typed_control "trill-speed" 14 Score.Real)) $
    \neighbor speed -> Lily.note args Attrs.trill $ do
        mode <- get_mode
        (transpose, control) <- trill_from_controls
            (Args.start args, Args.end args) mode neighbor speed
        xs <- mapM Derive.score $ map fst $ Signal.unsignal transpose
        let end = snd $ Args.range args
        let notes = do
                (x, maybe_next) <- Seq.zip_next xs
                let next = fromMaybe end maybe_next
                return $ Note.Event x (next-x) Util.note
        Derive.with_added_control control (Score.untyped transpose) $
            Note.place notes

-- | TODO randomize dyn, randomize starts
c_tremolo :: Derive.NoteCall
c_tremolo = Derive.stream_generator "tremolo" $ Note.inverting $ \args ->
    CallSig.call1 args (
        optional "speed" (typed_control "tremolo-speed" 10 Score.Real)) $
    \speed -> Lily.note args Attrs.trem $ do
        (speed_sig, time_type) <- Util.to_time_signal Util.Real speed
        notes <- case time_type of
            Util.Real -> do
                (start, end) <- Args.real_range args
                mapM Derive.score $ take_full_notes end $
                    real_pos_at_speed speed_sig start
            Util.Score -> do
                let (start, end) = Args.range args
                notes <- score_pos_at_speed speed_sig start end
                return $ take_full_notes end notes
        let events = [Note.Event start (end - start) Util.note
                | (start, end) <- zip notes (drop 1 notes)]
        Note.place events

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

-- | Generate a pitch signal of alternating pitches.  If mode is UnisonFirst
-- it will start with the unison, and NeighborFirst will start with the
-- neighbor, Baroque style.  If the mode is not given explicitly via @tr1@ or
-- @tr2@, it's taken from the @trill-mode@ env var, which should be @'unison'@
-- or @'neighbor'@.
--
-- [neighbor /Control/ @%trill-neighbor,1d@] Alternate with this relative
-- pitch.
--
-- [speed /Control/ @%trill-speed,14r@] Trill at this speed.  If it's
-- a RealTime, the value is the number of cycles per second, which will be
-- unaffected by the tempo.  If it's a ScoreTime, the value is the number
-- of cycles per ScoreTime unit, and will stretch along with tempo changes.
c_pitch_trill :: Maybe Mode -> Derive.PitchCall
c_pitch_trill maybe_mode = Derive.generator1 "pitch_trill" $ \args ->
    CallSig.call3 args (required "note",
        optional "neighbor" (typed_control "trill-neighbor" 1 Score.Diatonic),
        optional "speed" (typed_control "trill-speed" 14 Score.Real)) $
    \note neighbor speed -> do
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
    ]

-- | The control version of 'c_pitch_trill'.  It generates a signal of values
-- alternating with 0, and can be used in a transposition signal.
--
-- Args are the same as 'c_pitch_trill'.
c_control_trill :: Maybe Mode -> Derive.ControlCall
c_control_trill maybe_mode = Derive.generator1 "control_trill" $ \args ->
    CallSig.call2 args (
        optional "neighbor" (control "trill-neighbor" 1),
        optional "speed" (typed_control "trill-speed" 14 Score.Real)) $
    \neighbor speed -> do
        mode <- maybe get_mode return maybe_mode
        fst <$> trill_from_controls (Args.start args, Args.next args)
            mode neighbor speed


-- * util

data Mode = UnisonFirst | NeighborFirst deriving (Show)

get_mode :: Derive.Deriver Mode
get_mode = do
    mode_name <- Derive.lookup_val (TrackLang.Symbol "trill-mode")
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
    let transitions = integral_cycles end all_transitions
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
    where transitions = integral_cycles end (real_pos_at_speed speed start)

-- | Emit an infinite list of RealTimes at the given speed, which may change
-- over time.  The speed is taken as hertz in real time.
real_pos_at_speed :: Signal.Control -> RealTime -> [RealTime]
real_pos_at_speed sig start =
    start : real_pos_at_speed sig (start + Signal.y_to_real (recip speed))
    where speed = Signal.at start sig

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

integral_cycles :: (Ord a) => a -> [a] -> [a]
integral_cycles end (x0:x1:x2:xs)
    -- This is what makes trills include the end, as documented in the module
    -- haddock.
    | x2 > end = [x0]
    | otherwise = x0 : x1 : integral_cycles end (x2:xs)
integral_cycles _ xs = take 1 xs
