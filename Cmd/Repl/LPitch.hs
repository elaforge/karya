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
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import Global
import Types


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
modify_pitch modify = PitchTrack.pitch_tracks $ \scale key note -> do
    pitch <- first pretty $ Scale.scale_read scale key note
    first pretty $ Scale.scale_show scale key $ modify pitch

-- | Change notes from one scale to another.  This only makes sense if the
-- scales have the same number of notes per octave.
--
-- TODO it would be nice to change the track title too, but ModifyEvents.Track
-- doesn't support that.
change_scale :: Cmd.M m => Pitch.ScaleId -> m (ModifyEvents.Track m)
change_scale to_scale_id = do
    track <- Selection.track
    to_scale <- Cmd.require "scale not found"
        =<< Perf.lookup_scale track to_scale_id
    return $ PitchTrack.pitch_tracks $ \from_scale key note -> do
        pitch <- first pretty $ Scale.scale_read from_scale key note
        first pretty $ Scale.scale_show to_scale key pitch

-- | Convert the selected absolute pitch track into a relative one by
-- subtracting all the notes from the given base note.
--
-- TODO as above, would be nice to set thet track title.
to_relative :: Cmd.M m => Bool -> Pitch.Note -> ModifyEvents.Track m
to_relative diatonic base = PitchTrack.pitch_tracks $ \scale key note -> do
    base <- first pretty $ Scale.scale_read scale key base
    pitch <- first pretty $ Scale.scale_read scale key note
    let layout = Scale.scale_layout scale
    let d = if diatonic then Scale.diatonic_difference layout pitch base
            else Scale.chromatic_difference layout pitch base
    return $ Pitch.Note $ ShowVal.show_val d


-- * enharmonics

simplify_block_enharmonics :: BlockId -> Cmd.CmdL ()
simplify_block_enharmonics block_id =
    ModifyEvents.block block_id simplify_enharmonics

-- | This only works for *twelve at the moment.  For it to work for any scale
-- I need a way to parse to Pitch.Pitch.  Can't use scale_enharmonics because
-- I don't want to mess with ones that are already simple.
--
-- TODO I have scale_read now, a generic implementation should be possible
simplify_enharmonics :: Cmd.M m => ModifyEvents.Track m
simplify_enharmonics = PitchTrack.pitch_tracks $ \scale key note ->
    case Twelve.read_absolute_pitch note of
        Nothing -> Right note
        Just pitch
            | abs (Pitch.pitch_accidentals pitch) < 2 -> Right note
            | otherwise -> case Scale.scale_enharmonics scale key note of
                Right (simpler : _) -> Right simpler
                _ -> Right note
