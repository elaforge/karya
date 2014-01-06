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
import Util.Control
import qualified Ui.Event as Event
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import Types


-- | Turn an nn back to a human-readable note name.
nn_to_note :: Int -> Maybe Pitch.Note
nn_to_note = Twelve.show_nn . fromIntegral

-- * invert

-- TODO these should invert position of control events too
to_negative, to_positive :: Cmd.CmdL ()
to_negative = ModifyEvents.selection $ ModifyEvents.event $ \evt ->
    if Event.negative evt then evt else negate_event evt
to_positive = ModifyEvents.selection $ ModifyEvents.event $ \evt ->
    if Event.positive evt then evt else negate_event evt

negate_event :: Event.Event -> Event.Event
negate_event event =
    Event.move (+ Event.duration event) $ Event.modify_duration negate event

-- * transpose

-- | Chromatic transposition.
transpose_c :: Pitch.Octave -> Double -> Cmd.CmdL ()
transpose_c oct steps =
    PitchTrack.transpose_selection oct (Pitch.Chromatic steps)

-- | Diatonic transposition.
transpose_d :: Pitch.Octave -> Double -> Cmd.CmdL ()
transpose_d oct steps =
    PitchTrack.transpose_selection oct (Pitch.Diatonic steps)

transpose_all :: Pitch.Octave -> Pitch.Transpose -> Cmd.CmdL ()
transpose_all octs steps = ModifyEvents.all_blocks $ PitchTrack.pitch_tracks $
    PitchTrack.transpose octs steps

-- * modify pitches

modify_pitch :: Cmd.M m => (Theory.Pitch -> Theory.Pitch)
    -> ModifyEvents.Track m
modify_pitch modify = PitchTrack.pitch_tracks $ \scale key note -> do
    pitch <- PitchTrack.pretty_err $ Scale.scale_read scale key note
    PitchTrack.pretty_err $ Scale.scale_show scale key $ modify pitch

-- | Change notes from one scale to another.  This only makes sense if the
-- scales have the same number of notes per octave.
--
-- TODO it would be nice to change the track title too, but ModifyEvents.Track
-- doesn't support that.
change_scale :: Cmd.M m => Pitch.ScaleId -> m (ModifyEvents.Track m)
change_scale to_scale = do
    to_scale <- Cmd.get_scale "LPitch.change_scale" to_scale
    return $ PitchTrack.pitch_tracks $ \from_scale key note -> do
        pitch <- PitchTrack.pretty_err $ Scale.scale_read from_scale key note
        PitchTrack.pretty_err $ Scale.scale_show to_scale key pitch

-- | Convert the selected absolute pitch track into a relative one by
-- subtracting all the notes from the given base note.
--
-- TODO as above, would be nice to set thet track title.
to_relative :: Cmd.M m => Bool -> Pitch.Note -> ModifyEvents.Track m
to_relative diatonic base = PitchTrack.pitch_tracks $ \scale key note -> do
    base <- PitchTrack.pretty_err $ Scale.scale_read scale key base
    pitch <- PitchTrack.pretty_err $ Scale.scale_read scale key note
    let layout = Scale.scale_layout scale
    let d = if diatonic then Scale.diatonic_difference layout pitch base
            else Scale.chromatic_difference layout pitch base
    return $ Pitch.Note $ ShowVal.show_val d


-- * enharmonics

simplify_block_enharmonics :: BlockId -> Cmd.CmdL ()
simplify_block_enharmonics block_id =
    ModifyEvents.block block_id simplify_enharmonics

-- | This only works for Twelve at the moment.  For it to work for any scale
-- I need a way to parse to Theory.Pitch.  Can't use scale_enharmonics because
-- I don't want to mess with ones that are already simple.
simplify_enharmonics :: (Cmd.M m) => ModifyEvents.Track m
simplify_enharmonics = PitchTrack.pitch_tracks $ \scale key note ->
    case Twelve.read_pitch note of
        Nothing -> Right note
        Just pitch
            | abs (Theory.pitch_accidentals pitch) < 2 -> Right note
            | otherwise -> case Scale.scale_enharmonics scale key note of
                Right (simpler : _) -> Right simpler
                _ -> Right note
