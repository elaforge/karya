-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.Call.Grace where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
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


-- * note calls

c_mordent :: Pitch.Transpose -> Derive.Generator Derive.Note
c_mordent default_neighbor = Derive.make_call "mordent" Tags.ornament
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.defaulted "neighbor" (TrackLang.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_dur_env <*> grace_dyn_env
    ) $ \(TrackLang.DefaultDiatonic neighbor, grace_dur, dyn) ->
    Sub.inverting $ \args ->
        Lily.when_lilypond (lily_mordent args neighbor) $ do
            pitch <- Util.get_pitch =<< Args.real_start args
            grace_call args dyn [pitch, Pitches.transpose neighbor pitch]
                grace_dur

lily_mordent :: Derive.PassedArgs d -> Pitch.Transpose -> Derive.NoteDeriver
lily_mordent args neighbor = do
    pitch <- Util.get_pitch =<< Args.real_start args
    lily_grace args [pitch, Pitches.transpose neighbor pitch]

c_grace :: Derive.Generator Derive.Note
c_grace = Derive.make_call "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes.\n" <> grace_doc) $ Sig.call ((,,)
    <$> Sig.many "pitch" "Grace note pitches."
    <*> grace_dur_env <*> grace_dyn_env
    ) $ \(pitches, grace_dur, dyn) -> Sub.inverting $ \args -> do
        base <- Util.get_pitch =<< Args.real_start args
        let ps = resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args ps) $
            grace_call args dyn ps grace_dur

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
    -> RealTime -> Derive.NoteDeriver
grace_call args dyn pitches grace_dur = do
    notes <- make_grace_notes (Args.extent args) dyn pitches grace_dur
    -- Normally legato notes emphasize the first note, but that's not
    -- appropriate for grace notes.
    Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call args "(" [] [notes]

make_grace_notes :: (ScoreTime, ScoreTime) -> Signal.Y -> [PitchSignal.Pitch]
    -> RealTime -> Derive.Deriver [Sub.Event]
make_grace_notes (start, dur) dyn_scale pitches grace_dur = do
    pos <- Derive.real start
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    notes <- grace_notes pos grace_dur $
        map (flip Util.pitched_note dyn) pitches
    return $ notes ++ [Sub.Event start dur Util.note]

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
resolve_pitches base = map $
    either id (flip Pitches.transpose base . TrackLang.defaulted_diatonic)

grace_dur_env :: Sig.Parser RealTime
grace_dur_env = Sig.environ "grace-dur" Sig.Unprefixed (1/12)
    "Duration of grace notes."

grace_dyn_env :: Sig.Parser Double
grace_dyn_env = TrackLang.positive <$>
    Sig.environ "grace-dyn" Sig.Unprefixed (TrackLang.Positive 0.5)
        "Scale the dyn of the grace notes."

grace_doc :: Text
grace_doc = "This kind of grace note falls before the start of the \
    \ destination note, and is of a uniform speed, regardless of the tempo.\
    \ The grace notes go through the `(` call, so they will overlap or apply a\
    \ keyswitch, or whatever `(` does."

c_grace_attr :: Map.Map Int Score.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.Generator Derive.Note
c_grace_attr supported =
    Derive.make_call "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> Text.intercalate ", " (map ShowVal.show_val (Map.elems supported))
    ) $ Sig.call ((,,)
    <$> Sig.many "pitch" "Grace note pitches."
    <*> grace_dur_env <*> grace_dyn_env
    ) $ \(pitches, grace_dur, dyn) -> Sub.inverting $ \args -> do
        base <- Util.get_pitch =<< Args.real_start args
        let ps = resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args ps) $ do
            maybe_attrs <- grace_attrs supported ps base
            case maybe_attrs of
                Just attrs -> Util.add_attrs attrs (Util.placed_note args)
                -- Fall back on normal grace.
                Nothing -> grace_call args dyn ps grace_dur

grace_attrs :: Map.Map Int Score.Attributes -> [PitchSignal.Pitch]
    -> PitchSignal.Pitch -> Derive.Deriver (Maybe Score.Attributes)
grace_attrs supported [grace] base = do
    diff <- (-) <$> Pitches.pitch_nn base <*> Pitches.pitch_nn grace
    return $ Map.lookup (round diff) supported
grace_attrs _ _ _ = return Nothing


-- * pitch calls

c_mordent_p :: Pitch.Transpose -> Derive.Generator Derive.Pitch
c_mordent_p default_neighbor = Derive.generator1 "mordent" Tags.ornament
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.defaulted "neighbor" (TrackLang.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_dur_env
    ) $ \(pitch, TrackLang.DefaultDiatonic neighbor, grace_dur) args ->
        grace_p grace_dur [pitch, Pitches.transpose neighbor pitch, pitch]
            <$> Args.real_range_or_next args

c_grace_p :: Derive.Generator Derive.Pitch
c_grace_p = Derive.generator1 "grace" Tags.ornament
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
        grace_p grace_dur ps <$> Args.real_range_or_next args

grace_p :: RealTime -> [PitchSignal.Pitch] -> (RealTime, RealTime)
    -> PitchSignal.Signal
grace_p grace_dur pitches (start, end) = PitchSignal.signal $ zip starts pitches
    where starts = fit_grace start end (length pitches) grace_dur

-- | Determine grace note starting times if they are to fit in the given time
-- range.
fit_grace :: RealTime -> RealTime -> Int -> RealTime -> [RealTime]
fit_grace start end notes dur = take notes $ Seq.range_ start step
    where
    ndur = RealTime.seconds (fromIntegral notes)
    step
        | dur * ndur >= end - start = (end - start) / ndur
        | otherwise = dur
