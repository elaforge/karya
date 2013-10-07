-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Get track-specific Cmds.
module Cmd.Track where
import Util.Control
import qualified Util.Log as Log
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Edit as Edit
import qualified Cmd.Info as Info
import qualified Cmd.Keymap as Keymap
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.TrackInfo as TrackInfo
import qualified Instrument.MidiDb as MidiDb
import Types


track_cmd :: Cmd.Cmd
track_cmd msg = do
    cmds <- get_track_cmds
    Cmd.sequence_cmds cmds msg

-- | Get cmds according to the currently focused block and track.
get_track_cmds :: Cmd.CmdId [Cmd.Cmd]
get_track_cmds = do
    -- If this fails, it means the the track type can't be determined and there
    -- will be no track cmds.
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    maybe_track_id <- State.event_track_at block_id tracknum
    track <- Cmd.require =<< Info.lookup_track_type block_id tracknum

    maybe_info <- maybe (return Nothing) (lookup_midi_info block_id)
        maybe_track_id
    track_title <- maybe (return Nothing) (fmap Just . State.get_track_title)
        maybe_track_id
    let icmds = case (track_title, maybe_info) of
            (Just title, Just inst) | TrackInfo.is_note_track title ->
                Cmd.inst_cmds $ MidiDb.info_code inst
            _ -> []
    edit_state <- Cmd.gets Cmd.state_edit
    let edit_mode = Cmd.state_edit_mode edit_state
    let with_input = NoteEntry.cmds_with_input
            (Cmd.state_kbd_entry edit_state) (MidiDb.info_patch <$> maybe_info)
        tcmds = track_cmds edit_mode track
    let edit_input_cmd = Edit.edit_input $ case Info.track_type track of
            Info.Note {} -> False
            _ -> True
    kcmds <- keymap_cmds track
    -- The order is important:
    -- - Per-instrument cmds can override all others.
    --
    -- - The note cmds make sure that kbd entry can take over the kbd, and midi
    -- thru gets first whack at incoming midi.
    --
    -- - Track cmds are also wanting to take over the whole keyboard.
    --
    -- - Keymap cmds are "background" and only apply if no more special mode is
    -- active, so they go last.
    --
    -- Instrument cmds aren't under 'NoteEntry.cmds_with_input' so they don't
    -- get 'Pitch.Input's.  This is because if they are creating their own
    -- kbd entry then they will want the underlying keystrokes, as drum
    -- mappings do.  If they want 'Pitch.Input's, they can call
    -- 'NoteEntry.cmds_with_input'.
    return $ icmds ++ edit_input_cmd : with_input (input_cmds edit_mode track)
        : tcmds ++ kcmds

lookup_midi_info :: (Cmd.M m) => BlockId -> TrackId -> m (Maybe Cmd.MidiInfo)
lookup_midi_info block_id track_id =
    justm (Perf.lookup_instrument block_id (Just track_id)) $ \inst ->
    Cmd.lookup_instrument inst

-- | Cmds that use InputNotes, and hence must be called with
-- 'NoteEntry.cmds_with_input'.
input_cmds :: Cmd.EditMode -> Info.Track -> [Cmd.Cmd]
input_cmds edit_mode track = universal ++ case Info.track_type track of
    Info.Note {} -> case edit_mode of
        Cmd.RawEdit -> [NoteTrack.cmd_raw_edit]
        Cmd.ValEdit -> [NoteTrack.cmd_val_edit]
        _ -> []
    Info.Pitch {} -> case edit_mode of
        Cmd.RawEdit -> [PitchTrack.cmd_raw_edit]
        Cmd.ValEdit -> [PitchTrack.cmd_val_edit]
        _ -> []
    Info.Control {} -> case edit_mode of
        Cmd.ValEdit
            | is_tempo -> [ControlTrack.cmd_tempo_val_edit]
            | otherwise -> [ControlTrack.cmd_val_edit]
        _ -> []
    where
    universal =
        [ PitchTrack.cmd_record_note_status, MidiThru.cmd_midi_thru
        , NoteEntry.edit_append
        ]
    is_tempo = TrackInfo.is_tempo_track $
        State.track_title (Info.track_info track)

-- | Track-specific Cmds.
track_cmds :: Cmd.EditMode -> Info.Track -> [Cmd.Cmd]
track_cmds edit_mode track = case Info.track_type track of
    Info.Note {} -> case edit_mode of
        Cmd.MethodEdit -> [NoteTrack.cmd_method_edit]
        _ -> []
    Info.Pitch {} -> case edit_mode of
        Cmd.MethodEdit -> [PitchTrack.cmd_method_edit]
        _ -> []
    Info.Control {} -> case edit_mode of
        Cmd.RawEdit -> [ControlTrack.cmd_raw_edit]
        Cmd.MethodEdit -> [ControlTrack.cmd_method_edit]
        _ -> []

-- | Track-specific keymaps.
keymap_cmds :: (Cmd.M m) => Info.Track -> m [Cmd.Cmd]
keymap_cmds track = case Info.track_type track of
    Info.Note {} -> do
        let (cmd_map, warns) = NoteTrackKeymap.make_keymap
        forM_ warns $ \warn -> Log.warn $ "NoteTrackKeymap: " ++ warn
        return [Keymap.make_cmd cmd_map]
    _ -> return []
