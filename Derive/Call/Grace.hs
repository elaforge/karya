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
import Derive.Sig (defaulted, many)

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("`mordent`", c_mordent (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent (Pitch.Diatonic (-1)))
    , ("g", c_grace)
    ]
    []

c_mordent :: Pitch.Transpose -> Derive.Generator Derive.Note
c_mordent default_neighbor = Derive.make_call "mordent" Tags.ornament
    ("Generate some grace notes for a mordent, similar to a brief trill.\
    \ The grace notes fall before the onset of the main note, and\
    \ are in absolute RealTime, unaffected by tempo changes.\n" <> grace_doc) $
    Sig.call ((,,)
    <$> defaulted "neighbor" default_neighbor "Neighbor pitch."
    <*> defaulted "dyn" 0.5 "Scale the dyn of the generated grace notes."
    <*> grace_dur_arg
    ) $ \(neighbor, dyn, grace_dur) -> Sub.inverting $ \args ->
        Lily.when_lilypond (lily_mordent args neighbor) $
            mordent (Args.extent args) dyn neighbor grace_dur

mordent :: (ScoreTime, ScoreTime) -> Signal.Y -> Pitch.Transpose
    -> RealTime -> Derive.NoteDeriver
mordent (start, dur) dyn_scale neighbor grace_dur = do
    pos <- Derive.real start
    pitch <- Util.get_pitch pos
    dyn <- (*dyn_scale) <$> Util.dynamic pos
    notes <- grace_notes pos grace_dur
        [ Util.pitched_note pitch dyn
        , Util.pitched_note (Pitches.transpose neighbor pitch) dyn
        ]
    Sub.place notes <> Derive.d_place start dur Util.note

lily_mordent :: Derive.PassedArgs d -> Pitch.Transpose -> Derive.NoteDeriver
lily_mordent args neighbor = do
    pitch <- Util.get_pitch =<< Args.real_start args
    lily_grace args [pitch, Pitches.transpose neighbor pitch]

type Pitch = Either PitchSignal.Pitch Pitch.Transpose

c_grace :: Derive.Generator Derive.Note
c_grace = Derive.make_call "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes.\n" <> grace_doc) $ Sig.call ((,,)
    <$> grace_dyn_arg
    <*> many "pitch" "Grace note pitches."
    <*> grace_dur_arg
    ) $ \(dyn, pitches, grace_dur) -> Sub.inverting $ \args -> do
        (_, pitches) <- resolve_pitches args pitches
        Lily.when_lilypond (lily_grace args pitches) $
            grace_call args dyn pitches grace_dur

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

resolve_pitches :: Derive.PassedArgs d
    -> [Either PitchSignal.Pitch Pitch.Transpose]
    -> Derive.Deriver (PitchSignal.Pitch, [PitchSignal.Pitch])
resolve_pitches args pitches = do
    base <- Derive.require "no pitch" =<< Derive.pitch_at
        =<< Args.real_start args
    return (base, map (either id (flip Pitches.transpose base)) pitches)

grace_dur_arg :: Sig.Parser RealTime
grace_dur_arg = Sig.environ "grace-dur" Sig.Unprefixed (1/12)
    "Duration of grace notes."

grace_dyn_arg :: Sig.Parser Double
grace_dyn_arg = Sig.optional "dyn" 0.5 "Scale the dyn of the grace notes."

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
    <$> grace_dyn_arg
    <*> many "pitch" "Grace note pitches."
    <*> grace_dur_arg
    ) $ \(dyn, pitches, grace_dur) -> Sub.inverting $ \args -> do
        (base, pitches) <- resolve_pitches args pitches
        Lily.when_lilypond (lily_grace args pitches) $ do
            maybe_attrs <- grace_attrs supported pitches base
            case maybe_attrs of
                Just attrs -> Util.add_attrs attrs (Util.placed_note args)
                -- Fall back on normal grace.
                Nothing -> grace_call args dyn pitches grace_dur

grace_attrs :: Map.Map Int Score.Attributes -> [PitchSignal.Pitch]
    -> PitchSignal.Pitch -> Derive.Deriver (Maybe Score.Attributes)
grace_attrs supported [grace] base = do
    diff <- (-) <$> Pitches.pitch_nn base <*> Pitches.pitch_nn grace
    return $ Map.lookup (round diff) supported
grace_attrs _ _ _ = return Nothing
