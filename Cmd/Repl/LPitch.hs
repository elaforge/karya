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
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Types


-- | Turn an nn back to a human-readable note name.
nn_to_note :: Int -> Maybe Pitch.Note
nn_to_note key = Scale.scale_input_to_note Twelve.scale Nothing
    (Pitch.InputKey (fromIntegral key))

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

transpose_c :: Pitch.Octave -> Double -> Cmd.CmdL ()
transpose_c oct steps =
    PitchTrack.transpose_selection oct (Pitch.Chromatic steps)

transpose_d :: Pitch.Octave -> Double -> Cmd.CmdL ()
transpose_d oct steps =
    PitchTrack.transpose_selection oct (Pitch.Diatonic steps)

transpose_all :: Pitch.Octave -> Pitch.Transpose -> Cmd.CmdL()
transpose_all octs steps = ModifyEvents.all_blocks $ PitchTrack.pitch_tracks $
    PitchTrack.transpose octs steps

-- * to_relative

-- | Convert the selected absolute pitch track into a relative one by
-- subtracting all the notes from the given base note.
to_relative :: Bool -> Text -> Cmd.CmdL ()
to_relative diatonic note_s =
    ModifyEvents.selection $ \block_id track_id events -> do
        -- This is tricky because it's converting a pitch track to a control
        -- track, and hoping the same calls apply.
        let base = Pitch.Note note_s
        title <- Track.track_title <$> State.get_track track_id
        case TrackInfo.parse_control title of
            Right (TrackInfo.Pitch scale_id Nothing) -> do
                scale <- Cmd.get_scale "LPitch.to_relative" scale_id
                m_key <- Perf.get_key block_id (Just track_id)
                State.set_track_title track_id $ add_control $
                    if diatonic then Score.c_diatonic else Score.c_chromatic
                ModifyEvents.failable_texts
                    (relative_event diatonic scale m_key base)
                    block_id track_id events
                -- relative_events diatonic scale note block_id track_id events
            control_type ->
                Cmd.throw $ "not the default pitch track: " ++ show control_type

-- | Make an absolute pitch call relative to the given base degree.
relative_event :: Bool -> Scale.Scale -> Maybe Pitch.Key -> Pitch.Note
    -> Text -> Either String Text
relative_event diatonic scale m_key base = PitchTrack.modify_expr $ \text ->
    case scale_diff scale m_key diatonic base (Pitch.Note text) of
        Left err -> Left (show err)
        Right note -> Right $ Pitch.note_text note

-- TODO unimplemented, it would have to a be a Scale method
scale_diff :: Scale.Scale -> Maybe Pitch.Key -> Bool
    -> Pitch.Note -> Pitch.Note -> Either Scale.ScaleError Pitch.Note
scale_diff = undefined

-- | Make a control track title.
add_control :: Score.Control -> Text
add_control control = TrackInfo.unparse_control $
    TrackInfo.Control (Just (TrackLang.Symbol "add")) (Score.untyped control)

set_note :: Pitch.Note -> Event.Event -> Event.Event
set_note note = PitchTrack.modify f
    where f event = event { PitchTrack.event_val = Pitch.note_text note }

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
