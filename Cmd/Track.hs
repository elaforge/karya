-- | Get track-specific Cmds.
module Cmd.Track where
import Util.Control
import qualified Util.Log as Log
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.EditUtil as EditUtil
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
    -- If this fails, it means the the track type can't be determined and there
    -- will be no track cmds.
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.require =<< Cmd.get_insert_tracknum
    maybe_track_id <- State.event_track_at block_id tracknum
    track <- Cmd.require =<< Info.lookup_track_type block_id tracknum

    maybe_inst <- maybe (return Nothing) (lookup_instrument block_id)
        maybe_track_id
    track_title <- maybe (return Nothing) (fmap Just . State.get_track_title)
        maybe_track_id
    let icmds = case (track_title, maybe_inst) of
            (Just title, Just inst) | TrackInfo.is_note_track title ->
                Cmd.inst_cmds $ MidiDb.info_code inst
            _ -> []
    edit_state <- Cmd.gets Cmd.state_edit
    let edit_mode = Cmd.state_edit_mode edit_state
    let note_cmd = NoteEntry.cmds_with_note (Cmd.state_kbd_entry edit_state)
            (MidiDb.info_patch <$> maybe_inst) (note_cmds edit_mode track)
        tcmds = track_cmds edit_mode track
    let edit_input_cmd = EditUtil.edit_input $ case Info.track_type track of
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
    return $ icmds ++ edit_input_cmd : note_cmd : tcmds ++ kcmds

instrument_cmds :: (Cmd.M m) => TrackId -> Cmd.MidiInfo -> m (Maybe [Cmd.Cmd])
instrument_cmds track_id inst =
    ifM (not . TrackInfo.is_note_track <$> State.get_track_title track_id)
        (return Nothing)
        (return $ Just $ Cmd.inst_cmds $ MidiDb.info_code inst)

lookup_instrument :: (Cmd.M m) => BlockId -> TrackId -> m (Maybe Cmd.MidiInfo)
lookup_instrument block_id track_id =
    justm (Perf.lookup_instrument block_id (Just track_id)) $ \inst ->
    Cmd.lookup_instrument inst

-- | Cmds that use InputNotes, and hence must be called with
-- 'NoteEntry.cmds_with_note'.
note_cmds :: Cmd.EditMode -> Info.Track -> [Cmd.Cmd]
note_cmds edit_mode track = universal ++ case Info.track_type track of
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
