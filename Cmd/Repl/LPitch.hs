-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Operations on pitch tracks.
--
-- TODO It seems a little arbitrary to divide these cmds out like this.
-- However, they are distinct from Cmd.PitchTrack, so a separate module is
-- good.  I suppose if I need these functions elsewhere I can more them to more
-- generic places.
module Cmd.Repl.LPitch where
import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import Global


-- | Turn an nn back to a human-readable note name.
nn_to_note :: Int -> Maybe Pitch.Note
nn_to_note = Twelve.show_nn . fromIntegral

-- * scales

patch_scale :: Cmd.M m => Pitch.ScaleId -> m Patch.Scale
patch_scale scale_id = (Cmd.require_right id <=< Perf.derive) $ do
    scale <- Derive.get_scale scale_id
    nns <- Scale.note_numbers scale mempty
    return $ Scale.patch_scale scale_id nns


-- * transpose

-- | Chromatic transposition.
transpose_c :: Cmd.M m => Pitch.Step -> ModifyEvents.Track m
transpose_c = transpose Scale.Chromatic 0

-- | Diatonic transposition.
transpose_d :: Cmd.M m => Pitch.Step -> ModifyEvents.Track m
transpose_d = transpose Scale.Diatonic 0

-- | Octave transposition.
transpose_o :: Cmd.M m => Pitch.Octave -> ModifyEvents.Track m
transpose_o octaves = transpose Scale.Chromatic octaves 0

transpose :: Cmd.M m => Scale.Transposition -> Pitch.Octave -> Pitch.Step
    -> ModifyEvents.Track m
transpose transposition octs steps =
    PitchTrack.pitch_tracks $ PitchTrack.transpose transposition octs steps

-- * modify pitches

modify_pitch :: Cmd.M m => (Pitch.Pitch -> Pitch.Pitch)
    -> ModifyEvents.Track m
modify_pitch modify = PitchTrack.pitch_tracks $ \scale env note -> do
    pitch <- first pretty $ Scale.scale_read scale env note
    first pretty $ Scale.scale_show scale env $ modify pitch

-- | Change notes from one scale to another.  This only makes sense if the
-- scales have the same number of notes per octave.
--
-- TODO it would be nice to change the track title too, but ModifyEvents.Track
-- doesn't support that.  Of course, maybe I want the scale in the block or
-- global transform.
change_scale :: Cmd.M m => Pitch.ScaleId -> m (ModifyEvents.Track m)
change_scale to_scale_id = do
    track <- Selection.track
    to_scale <- Cmd.require "scale not found"
        =<< Perf.lookup_scale track to_scale_id
    return $ PitchTrack.pitch_tracks $ \from_scale env note -> do
        pitch <- first pretty $ Scale.scale_read from_scale env note
        first pretty $ Scale.scale_show to_scale env pitch

-- | Convert the selected absolute pitch track into a relative one by
-- subtracting all the notes from the given base note.
--
-- TODO as above, would be nice to set thet track title.
to_relative :: Cmd.M m => Bool -> Pitch.Note -> ModifyEvents.Track m
to_relative diatonic base = PitchTrack.pitch_tracks $ \scale env note -> do
    base <- first pretty $ Scale.scale_read scale env base
    pitch <- first pretty $ Scale.scale_read scale env note
    let layout = Scale.scale_layout scale
    let d = if diatonic then Scale.diatonic_difference layout pitch base
            else Scale.chromatic_difference layout pitch base
    return $ Pitch.Note $ ShowVal.show_val d


-- * enharmonics

-- | Respell enharmonics according to the key.
respell :: Cmd.M m => ModifyEvents.Track m
respell = PitchTrack.pitch_tracks $ \scale env note -> first pretty $ do
    pitch <- Scale.scale_read scale env note
    Scale.scale_input_to_note scale env (to_piano_input pitch)

to_piano_input :: Pitch.Pitch -> Pitch.Input
to_piano_input pitch = Pitch.Input Pitch.PianoKbd (to_sharps pitch) 0
    where
    to_sharps = Theory.semis_to_pitch_sharps Theory.piano_layout
        . Theory.pitch_to_semis Theory.piano_layout

-- | Convert all sharps to their enharmonic flats.
sharps_to_flats :: Cmd.M m => ModifyEvents.Track m
sharps_to_flats = PitchTrack.pitch_tracks $ \scale env note -> first pretty $ do
    pitch <- Scale.scale_read scale env note
    if Pitch.pitch_accidentals pitch <= 0 then return note else do
        notes <- Scale.scale_enharmonics scale env note
        pitches <- mapM (Scale.scale_read scale env) notes
        justErr (BaseTypes.PitchError $ "no flats for " <> pretty note) $
            e_sharps_to_flats (zip pitches notes)

e_sharps_to_flats :: [(Pitch.Pitch, a)] -> Maybe a
e_sharps_to_flats = fmap snd . Seq.maximum_on accs . filter ((<0) . accs)
    where accs = Pitch.pitch_accidentals . fst
