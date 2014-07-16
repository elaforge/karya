-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.Call.Europe.Grace where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("g", c_grace)
    , ("roll", c_roll)
    , ("`mordent`", c_mordent (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent (Pitch.Diatonic (-1)))
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map
    [ ("g", c_grace_p)
    , ("`mordent`", c_mordent_p (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent_p (Pitch.Diatonic (-1)))
    ]


-- | It's pretty much arbitrary, but this seems ok.
default_grace_dur :: TrackLang.DefaultReal
default_grace_dur = TrackLang.real (1/12)

-- * standard args

grace_envs :: Sig.Parser (TrackLang.Duration, Double, TrackLang.ValControl)
grace_envs = (,,) <$> grace_dur_env <*> grace_dyn_env <*> grace_placement_env

grace_dur_env :: Sig.Parser TrackLang.Duration
grace_dur_env = TrackLang.default_real <$>
    Sig.environ "grace-dur" Sig.Unprefixed default_grace_dur
        "Duration of grace notes."

grace_dyn_env :: Sig.Parser Double
grace_dyn_env = TrackLang.positive <$>
    Sig.environ "grace-dyn" Sig.Unprefixed (TrackLang.Positive 0.5)
        "Scale the dyn of the grace notes."

grace_placement_env :: Sig.Parser TrackLang.ValControl
grace_placement_env = Sig.environ "grace-place" Sig.Unprefixed
    (Sig.control "grace-place" 0)
    "At 0, grace notes fall before their base note.  At 1, grace notes fall on\
    \ the base note, and the base note is delayed."


-- * note calls

c_mordent :: Pitch.Transpose -> Derive.Generator Derive.Note
c_mordent default_neighbor = Derive.make_call Module.europe "mordent"
    Tags.ornament
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,)
    <$> Sig.defaulted "neighbor" (TrackLang.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_envs
    ) $ \(TrackLang.DefaultDiatonic neighbor, (grace_dur, dyn, place)) ->
    Sub.inverting $ \args ->
        Lily.when_lilypond (lily_mordent args neighbor) $ do
            pitch <- Util.get_pitch =<< Args.real_start args
            grace_call args dyn [pitch, Pitches.transpose neighbor pitch]
                grace_dur place

lily_mordent :: Derive.PassedArgs d -> Pitch.Transpose -> Derive.NoteDeriver
lily_mordent args neighbor = do
    start <- Args.real_start args
    pitch <- Util.get_pitch start
    lily_grace args start [pitch, Pitches.transpose neighbor pitch]

-- | Grace is in the prelude since it's so commonly used.  Mordent and the
-- other variations are still in 'Module.europe'.
c_grace :: Derive.Generator Derive.Note
c_grace = Derive.make_call Module.prelude "grace" (Tags.ornament <> Tags.ly)
    "Emit grace notes. The grace notes go through the `(` call, so they will\
    \ overlap or apply a keyswitch, or do whatever `(` does."
    $ Sig.call ((,)
    <$> grace_pitches_arg <*> grace_envs
    ) $ \(pitches, (grace_dur, dyn, place)) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Util.get_pitch start
        pitches <- resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args start pitches) $
            grace_call args dyn pitches grace_dur place

lily_grace :: Derive.PassedArgs d -> RealTime -> [PitchSignal.Pitch]
    -> Derive.NoteDeriver
lily_grace args start pitches = do
    pitches <- mapM Lily.pitch_to_lily =<< mapM (Derive.resolve_pitch start)
        pitches
    let ly_notes = map (<> Lilypond.to_lily Lilypond.D8) pitches
        beamed = Seq.first_last (<>"[") (<>"]") ly_notes
        -- I use \acciaccatura instead of \grace because it adds a slur
        -- automatically.
        code = "\\acciaccatura { " <> Text.unwords beamed <> " } "
    -- Prepending to the note instead of emitting a separate Lily.code ensures
    -- it stays with the note's voice.
    Lily.prepend_code code $ Util.place args Util.note

grace_call :: Derive.NoteArgs -> Signal.Y -> [PitchSignal.Pitch]
    -> TrackLang.Duration -> TrackLang.ValControl -> Derive.NoteDeriver
grace_call args dyn_scale pitches grace_dur place = do
    dyn <- (*dyn_scale) <$> (Util.dynamic =<< Args.real_start args)
    let notes = map (Util.with_dynamic dyn . Util.pitched_note) pitches
            ++ [Util.note]
    events <- make_grace_notes (Args.prev_start args) (Args.extent args)
        notes grace_dur place
    -- Normally legato notes emphasize the first note, but that's not
    -- appropriate for grace notes.
    Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call args "(" [] [events]

c_roll :: Derive.Generator Derive.Note
c_roll = Derive.make_call Module.europe "roll" Tags.ornament
    "These are like grace notes, but they all have the same pitch."
    $ Sig.call ((,,)
    <$> Sig.defaulted "times" 1 "Number of grace notes."
    <*> Sig.defaulted "time" default_grace_dur "Time between the strokes."
    <*> Sig.defaulted "dyn" 0.5 "Dyn scale for the grace notes."
    ) $ \(times, TrackLang.DefaultReal time, dyn_scale) ->
    Sub.inverting $ roll times time dyn_scale

roll :: Int -> TrackLang.Duration -> Signal.Y -> Derive.PassedArgs a
    -> Derive.NoteDeriver
roll times time dyn_scale args = do
    start <- Args.real_start args
    pitch <- Util.get_pitch start
    repeat_notes (Util.with_pitch pitch Util.note) times time dyn_scale args

repeat_notes :: Derive.NoteDeriver -> Int -> TrackLang.Duration -> Signal.Y
    -> Derive.PassedArgs a -> Derive.NoteDeriver
repeat_notes note times time dyn_scale args = do
    start <- Args.real_start args
    dyn <- (*dyn_scale) <$> Util.dynamic start
    let notes = replicate times (Util.with_dynamic dyn note) ++ [note]
    Sub.place =<< make_grace_notes (Args.prev_start args)
        (Args.extent args) notes time (TrackLang.constant_control 0)

make_grace_notes :: Maybe ScoreTime -> (ScoreTime, ScoreTime)
    -> [Derive.NoteDeriver] -> TrackLang.Duration -> TrackLang.ValControl
    -> Derive.Deriver [Sub.Event]
make_grace_notes prev (start, dur) notes grace_dur place = do
    real_start <- Derive.real start
    place <- Num.clamp 0 1 <$> Util.control_at place real_start
    case grace_dur of
        TrackLang.Score grace_dur -> do
            let extents = fit_grace_durs (ScoreTime.double place)
                    prev start (start + dur) (length notes) grace_dur
            return [Sub.Event start dur note
                | ((start, dur), note) <- zip extents notes]
        TrackLang.Real grace_dur -> do
            real_end <- Derive.real (start + dur)
            real_prev <- maybe (return Nothing) ((Just <$>) . Derive.real) prev
            let extents = fit_grace_durs (RealTime.seconds place)
                    real_prev real_start real_end (length notes) grace_dur
            zipWithM note_real extents notes
    where
    note_real (start, dur) note = do
        score_start <- Derive.score start
        score_end <- Derive.score (start + dur)
        return $ Sub.Event score_start (score_end - score_start) note

c_grace_attr :: Map.Map Int Score.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.Generator Derive.Note
c_grace_attr supported =
    Derive.make_call Module.europe "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> Text.intercalate ", " (map ShowVal.show_val (Map.elems supported))
    ) $ Sig.call ((,)
    <$> grace_pitches_arg <*> grace_envs
    ) $ \(pitches, (grace_dur, dyn, place)) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Util.get_pitch start
        pitches <- resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args start pitches) $ do
            maybe_attrs <- grace_attrs supported pitches base
            case maybe_attrs of
                Just attrs -> attr_grace start args grace_dur (length pitches)
                    attrs
                -- Fall back on normal grace.
                Nothing -> grace_call args dyn pitches grace_dur place
    where
    attr_grace real_start args grace_dur notes attrs = do
        let (start, dur) = Args.extent args
        grace_dur <- Util.score_duration start grace_dur
        dyn <- Util.dynamic real_start
        let before = fromIntegral notes * grace_dur
        pitch <- Util.get_pitch real_start
        Derive.place (start - before) (dur + before) $
            Util.add_attrs attrs $ Util.with_dynamic dyn $
            Util.pitched_note pitch

grace_attrs :: Map.Map Int Score.Attributes -> [PitchSignal.Pitch]
    -> PitchSignal.Pitch -> Derive.Deriver (Maybe Score.Attributes)
grace_attrs supported [grace] base = do
    diff <- (-) <$> Pitches.pitch_nn base <*> Pitches.pitch_nn grace
    return $ Map.lookup (round diff) supported
grace_attrs _ _ _ = return Nothing


-- * pitch calls

c_mordent_p :: Pitch.Transpose -> Derive.Generator Derive.Pitch
c_mordent_p default_neighbor = Derive.generator1 Module.europe "mordent"
    Tags.ornament "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.defaulted "neighbor" (TrackLang.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_dur_env
    ) $ \(pitch, TrackLang.DefaultDiatonic neighbor, grace_dur) args ->
        grace_p grace_dur [pitch, Pitches.transpose neighbor pitch, pitch]
            (Args.range_or_next args)

c_grace_p :: Derive.Generator Derive.Pitch
c_grace_p = Derive.generator1 Module.europe "grace" Tags.ornament
    "Generate grace note pitches.  They start on the event and have the given\
    \ duration, but are shortened if the available duration is too short.\
    \ The destination pitch is first, even though it plays last, so\
    \ `g (c) (a) (b)` produces `a b c`."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> grace_pitches_arg <*> grace_dur_env
    ) $ \(pitch, pitches, grace_dur) args -> do
        ps <- (++[pitch]) <$> resolve_pitches pitch pitches
        grace_p grace_dur ps (Args.range_or_next args)

grace_p :: TrackLang.Duration -> [PitchSignal.Pitch]
    -> (ScoreTime, ScoreTime) -> Derive.Deriver PitchSignal.Signal
grace_p grace_dur pitches (start, end) = do
    real_dur <- Util.real_duration start grace_dur
    real_start <- Derive.real start
    real_end <- Derive.real end
    let starts = fit_after real_start real_end (length pitches) real_dur
    return $ PitchSignal.signal $ zip starts pitches

-- * util

grace_pitches_arg :: Sig.Parser [Either PitchSignal.Pitch Score.TypedVal]
grace_pitches_arg = Sig.many "pitch" "Grace note pitches. If they are numbers,\
    \ they are taken as transpositions and must all be the same type,\
    \ defaulting to diatonic."

resolve_pitches :: PitchSignal.Pitch
    -> [Either PitchSignal.Pitch Score.TypedVal]
    -> Derive.Deriver [PitchSignal.Pitch]
resolve_pitches base = either Derive.throw return . check_pitches base

check_pitches :: PitchSignal.Pitch
    -> [Either PitchSignal.Pitch Score.TypedVal]
    -> Either String [PitchSignal.Pitch]
check_pitches base pitches = do
    make <- case types of
        t : ts
            | all (==t) ts -> case t of
                Score.Diatonic -> Right Pitch.Diatonic
                Score.Chromatic -> Right Pitch.Chromatic
                Score.Nn -> Right Pitch.Nn
                _ -> Left $ "expected transpose type, but got " ++ pretty t
            | otherwise ->
                Left $ "arguments should all have the same type, got "
                    <> pretty types
        [] -> Right Pitch.Diatonic
    return $ map (either id (resolve make . Score.typed_val)) pitches
    where
    resolve make n = Pitches.transpose (make n) base
    types = snd . List.mapAccumL type_of Score.Diatonic . Either.rights $
        pitches
    type_of deflt n = case Score.type_of n of
        Score.Untyped -> (deflt, deflt)
        t -> (t, t)

-- | Determine grace note starting times and durations if they are to fit in
-- the given time range, shortening them if they don't fit.
fit_grace_durs :: (Show a, Fractional a, Ord a) => a -> Maybe a -> a -> a
    -> Int -> a -> [(a, a)]
fit_grace_durs place prev start end notes dur =
    map add_dur $ Seq.zip_next $ fit_grace place prev start end notes dur
    where
    add_dur (x, Nothing) = (x, end - x)
    add_dur (x, Just next) = (x, next - x)

fit_grace :: (Show a, Fractional a, Ord a) => a -> Maybe a -> a -> a -> Int
    -> a -> [a]
fit_grace place maybe_prev start end notes dur
    | place <= 0 = before
    | place >= 1 = after
    | otherwise = zipWith (\x y -> Num.scale x y place) before after
    where
    after = fit_after start end notes dur
    before = fit_before maybe_prev start notes dur

fit_before :: (Fractional a, Ord a) => Maybe a -> a -> Int -> a -> [a]
fit_before maybe_prev start notes dur =
    take notes $ drop 1 $ Seq.range_ (start - notes_t * step) step
    where
    notes_t = fromIntegral notes
    step
        | Just prev <- maybe_prev, start - dur * notes_t < prev =
            (start - prev) / notes_t
        | otherwise = dur

fit_after :: (Fractional a, Ord a) => a -> a -> Int -> a -> [a]
fit_after start end notes dur = take notes $ Seq.range_ start step
    where
    notes_t = fromIntegral notes
    step
        | dur * notes_t >= end - start = (end - start) / notes_t
        | otherwise = dur
