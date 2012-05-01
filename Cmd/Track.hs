-- | Get track-specific Cmds.
module Cmd.Track where
import Util.Control
import qualified Util.Log as Log
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
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
    Cmd.run_subs cmds msg

-- | Get cmds according to the currently focused block and track.
get_track_cmds :: Cmd.CmdId [Cmd.Cmd]
get_track_cmds = do
    -- No track cmds if the track type can't be determined.
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    maybe_track_id <- State.event_track_at block_id tracknum
    track <- Cmd.require =<< Info.lookup_track_type block_id tracknum

    icmds <- maybe [] id <$> case maybe_track_id of
        Just track_id -> lookup_instrument_cmds block_id track_id
        Nothing -> return Nothing
    edit_state <- Cmd.gets Cmd.state_edit
    let edit_mode = Cmd.state_edit_mode edit_state
    let note_cmd = NoteEntry.cmds_with_note (Cmd.state_kbd_entry edit_state)
            (with_note_cmds edit_mode track)
        tcmds = track_cmds edit_mode track
    kcmds <- keymap_cmds track
    -- The order is important:
    -- Per-instrument cmds can override all others.
    -- The note cmds make sure that kbd entry can take over the kbd, and midi
    -- thru gets first whack at incoming midi.
    -- Track cmds are also wanting to take over the whole keyboard.
    -- Keymap cmds are "background" and only apply if no more special mode is
    -- active, so they go last.
    return $ icmds ++ note_cmd : tcmds ++ kcmds

lookup_instrument_cmds :: (Cmd.M m) => BlockId -> TrackId
    -> m (Maybe [Cmd.Cmd])
lookup_instrument_cmds block_id track_id =
    justm (Perf.lookup_instrument block_id (Just track_id)) $ \inst ->
    justm (Cmd.lookup_instrument_info inst) $ \info ->
    return $ Just $ Cmd.inst_cmds (MidiDb.info_code info)

-- | Cmds that use InputNotes, and hence must be called with
-- 'NoteEntry.cmds_with_note'.
with_note_cmds :: Cmd.EditMode -> Info.Track -> [Cmd.Cmd]
with_note_cmds edit_mode track = universal ++ case Info.track_type track of
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
    universal = [PitchTrack.cmd_record_note_status, MidiThru.cmd_midi_thru]
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
