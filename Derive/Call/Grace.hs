-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.Call.Grace where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Call.Lily as Lily
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
    , ("`mordent`", c_mordent (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent (Pitch.Diatonic (-1)))
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map
    [ ("g", c_grace_p)
    , ("`mordent`", c_mordent_p (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent_p (Pitch.Diatonic (-1)))
    ]


-- * standard args

grace_envs :: Sig.Parser (TrackLang.RealOrScore, Double, TrackLang.ValControl)
grace_envs = (,,) <$> grace_dur_env <*> grace_dyn_env <*> grace_placement_env

grace_dur_env :: Sig.Parser TrackLang.RealOrScore
grace_dur_env = TrackLang.default_real <$>
    Sig.environ "grace-dur" Sig.Unprefixed (TrackLang.real (1/12))
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
c_mordent default_neighbor = Derive.make_call "mordent"
    (Tags.europe <> Tags.ornament)
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
    pitch <- Util.get_pitch =<< Args.real_start args
    lily_grace args [pitch, Pitches.transpose neighbor pitch]

c_grace :: Derive.Generator Derive.Note
c_grace = Derive.make_call "grace" (Tags.europe <> Tags.ornament <> Tags.ly)
    "Emit grace notes. The grace notes go through the `(` call, so they will\
    \ overlap or apply a keyswitch, or do whatever `(` does."
    $ Sig.call ((,)
    <$> Sig.many "pitch" "Grace note pitches."
    <*> grace_envs
    ) $ \(pitches, (grace_dur, dyn, place)) -> Sub.inverting $ \args -> do
        base <- Util.get_pitch =<< Args.real_start args
        let ps = resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args ps) $
            grace_call args dyn ps grace_dur place

lily_grace :: Derive.PassedArgs d -> [PitchSignal.Pitch] -> Derive.NoteDeriver
lily_grace args pitches = do
    pitches <- mapM Lily.pitch_to_lily pitches
    let ly_notes = map (<> Lilypond.to_lily Lilypond.D8) pitches
        beamed = Seq.first_last (<>"[") (<>"]") ly_notes
        -- I use \acciaccatura instead of \grace because it adds a slur
        -- automatically.
        code = "\\acciaccatura { " <> Text.unwords beamed <> " } "
    -- Prepending to the note instead of emitting a separate Lily.code ensures
    -- it stays with the note's voice.
    Lily.prepend_code code $ Util.place args Util.note

grace_call :: Derive.NoteArgs -> Signal.Y -> [PitchSignal.Pitch]
    -> TrackLang.RealOrScore -> TrackLang.ValControl -> Derive.NoteDeriver
grace_call args dyn pitches grace_dur place = do
    notes <- make_grace_notes (Args.prev_start args) (Args.extent args) dyn
        pitches grace_dur place
    -- Normally legato notes emphasize the first note, but that's not
    -- appropriate for grace notes.
    Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call args "(" [] [notes]

make_grace_notes :: Maybe ScoreTime -> (ScoreTime, ScoreTime) -> Signal.Y
    -> [PitchSignal.Pitch] -> TrackLang.RealOrScore -> TrackLang.ValControl
    -> Derive.Deriver [Sub.Event]
make_grace_notes prev (start, dur) dyn_scale pitches grace_dur place = do
    real_start <- Derive.real start
    dyn <- (*dyn_scale) <$> Util.dynamic real_start
    place <- Num.clamp 0 1 <$> Util.control_at place real_start
    let notes = map (flip Util.pitched_note dyn) pitches ++ [Util.note]
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

grace_notes :: RealTime -- ^ note start time, grace notes fall before this
    -> RealTime -- ^ duration of each grace note
    -> [Derive.NoteDeriver] -> Derive.Deriver [Sub.Event]
grace_notes start dur notes = mapM note $ zip starts notes
    where
    starts = Seq.range_ (start - dur * fromIntegral (length notes)) dur
    note (start, d) = do
        s_start <- Derive.score start
        s_end <- Derive.score (start + dur)
        return $ Sub.Event s_start (s_end - s_start) d

resolve_pitches :: PitchSignal.Pitch
    -> [Either PitchSignal.Pitch TrackLang.DefaultDiatonic]
    -> [PitchSignal.Pitch]
resolve_pitches base = map $ either id (resolve . TrackLang.default_diatonic)
    where resolve t = Pitches.transpose t base

c_grace_attr :: Map.Map Int Score.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.Generator Derive.Note
c_grace_attr supported =
    Derive.make_call "grace" (Tags.europe <> Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> Text.intercalate ", " (map ShowVal.show_val (Map.elems supported))
    ) $ Sig.call ((,)
    <$> Sig.many "pitch" "Grace note pitches."
    <*> grace_envs
    ) $ \(pitches, (grace_dur, dyn, place)) -> Sub.inverting $ \args -> do
        base <- Util.get_pitch =<< Args.real_start args
        let ps = resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args ps) $ do
            maybe_attrs <- grace_attrs supported ps base
            case maybe_attrs of
                Just attrs -> attr_grace args grace_dur (length pitches) attrs
                -- Fall back on normal grace.
                Nothing -> grace_call args dyn ps grace_dur place
    where
    attr_grace args grace_dur notes attrs = do
        let (start, dur) = Args.extent args
        grace_dur <- Util.duration_from start grace_dur
        let before = fromIntegral notes * grace_dur
        Util.add_attrs attrs $
            Derive.d_place (start - before) (dur + before) Util.note

grace_attrs :: Map.Map Int Score.Attributes -> [PitchSignal.Pitch]
    -> PitchSignal.Pitch -> Derive.Deriver (Maybe Score.Attributes)
grace_attrs supported [grace] base = do
    diff <- (-) <$> Pitches.pitch_nn base <*> Pitches.pitch_nn grace
    return $ Map.lookup (round diff) supported
grace_attrs _ _ _ = return Nothing


-- * pitch calls

c_mordent_p :: Pitch.Transpose -> Derive.Generator Derive.Pitch
c_mordent_p default_neighbor = Derive.generator1 "mordent"
    (Tags.europe <> Tags.ornament)
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.defaulted "neighbor" (TrackLang.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_dur_env
    ) $ \(pitch, TrackLang.DefaultDiatonic neighbor, grace_dur) args ->
        grace_p grace_dur [pitch, Pitches.transpose neighbor pitch, pitch]
            (Args.range_or_next args)

c_grace_p :: Derive.Generator Derive.Pitch
c_grace_p = Derive.generator1 "grace" (Tags.europe <> Tags.ornament)
    "Generate grace note pitches.  They start on the event and have the given\
    \ duration, but are shortened if the available duration is too short.\
    \ The destination pitch is first, even though it plays last, so\
    \ `g (c) (a) (b)` produces `a b c`."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.many "pitch" "Grace note pitches."
    <*> grace_dur_env
    ) $ \(pitch, pitches, grace_dur) args -> do
        let ps = resolve_pitches pitch pitches ++ [pitch]
        grace_p grace_dur ps (Args.range_or_next args)

grace_p :: TrackLang.RealOrScore -> [PitchSignal.Pitch]
    -> (ScoreTime, ScoreTime) -> Derive.Deriver PitchSignal.Signal
grace_p grace_dur pitches (start, end) = do
    real_dur <- Util.real_dur' start grace_dur
    real_start <- Derive.real start
    real_end <- Derive.real end
    let starts = fit_after real_start real_end (length pitches) real_dur
    return $ PitchSignal.signal $ zip starts pitches

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
