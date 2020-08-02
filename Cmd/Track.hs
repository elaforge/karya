-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Get track-specific Cmds.  Essentially, this detects the type of track
-- under the selection, and then returns cmds from one of "Cmd.NoteTrack",
-- "Cmd.PitchTrack", or "Cmd.ControlTrack".  This also handles per-instrument
-- and per-scale cmds.
module Cmd.Track (
    track_cmd
    , event_and_note_step
) where
import qualified Control.Monad.Except as Except

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Edit as Edit
import qualified Cmd.Info as Info
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.NoteTrackKeymap as NoteTrackKeymap
import qualified Cmd.Perf as Perf
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseTitle as ParseTitle
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Ui.Ui as Ui

import           Global
import           Types


track_cmd :: Msg.Msg -> Cmd.CmdId Cmd.Status
track_cmd msg = do
    cmds <- get_track_cmds `Except.catchError` \exc -> do
        case exc of
            Ui.Abort -> return ()
            Ui.Error stack msg ->
                Log.write $ Log.msg_call_stack stack Log.Warn Nothing
                    ("getting track cmds: " <> msg)
        return []
    Cmd.sequence_cmds cmds msg

-- | Get cmds according to the currently focused block and track.
get_track_cmds :: Cmd.CmdId [Msg.Msg -> Cmd.CmdId Cmd.Status]
get_track_cmds = do
    -- If this fails, it means the the track type can't be determined and there
    -- will be no track cmds.
    block_id <- Cmd.get_focused_block
    tracknum <- Cmd.abort_unless =<< Cmd.get_insert_tracknum
    track_id <- Cmd.abort_unless =<< Ui.event_track_at block_id tracknum
    mb_resolved <- lookup_inst block_id track_id
    track_type <- ParseTitle.track_type <$> Ui.get_track_title track_id
    has_note_children <- Info.has_note_children block_id tracknum

    edit_state <- Cmd.gets Cmd.state_edit
    let edit_mode = Cmd.state_edit_mode edit_state
    let with_input = NoteEntry.cmds_with_input
            (Cmd.state_kbd_entry edit_state)
            (fmap snd . Cmd.midi_instrument =<< mb_resolved)
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
    return $ concat
        [ case (track_type, mb_resolved) of
            (ParseTitle.NoteTrack, Just resolved) ->
                map Cmd.call $ Cmd.inst_cmds $ Common.common_code $
                    Inst.inst_common $ Cmd.inst_instrument resolved
            _ -> []
        , (:[]) $ Edit.handle_floating_input $ case track_type of
            ParseTitle.NoteTrack -> False
            _ -> True
        , (:[]) $ with_input (input_cmds edit_mode track_type has_note_children)
        , track_type_cmds edit_mode track_type
        , keymap_cmds track_type
        ]

lookup_inst :: Cmd.M m => BlockId -> TrackId -> m (Maybe Cmd.ResolvedInstrument)
lookup_inst block_id track_id =
    justm (Perf.lookup_instrument (block_id, Just track_id)) $ \inst ->
    Cmd.lookup_instrument inst

-- | Cmds that use InputNotes, and hence must be called with
-- 'NoteEntry.cmds_with_input'.
input_cmds :: Cmd.EditMode -> ParseTitle.Type -> Bool
    -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
input_cmds edit_mode track_type has_note_children =
    universal ++ case track_type of
        ParseTitle.NoteTrack
            | has_note_children -> []
            | otherwise -> case edit_mode of
                Cmd.ValEdit -> [NoteTrack.cmd_val_edit]
                _ -> []
        ParseTitle.PitchTrack -> case edit_mode of
            Cmd.ValEdit -> [PitchTrack.cmd_val_edit]
            _ -> []
        ParseTitle.ControlTrack -> case edit_mode of
            Cmd.ValEdit -> [ControlTrack.cmd_val_edit]
            _ -> []
        ParseTitle.TempoTrack -> case edit_mode of
            Cmd.ValEdit -> [ControlTrack.cmd_tempo_val_edit]
            _ -> []
    where
    universal =
        [ PitchTrack.cmd_record_note_status
        , MidiThru.cmd_midi_thru
        , NoteEntry.floating_input_insert
        ]

-- | Track-specific Cmds.
track_type_cmds :: Cmd.EditMode -> ParseTitle.Type
    -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
track_type_cmds edit_mode = \case
    ParseTitle.NoteTrack -> case edit_mode of
        Cmd.MethodEdit -> [NoteTrack.cmd_method_edit]
        _ -> []
    ParseTitle.PitchTrack -> case edit_mode of
        Cmd.MethodEdit -> [PitchTrack.cmd_method_edit]
        _ -> []
    ParseTitle.ControlTrack -> case edit_mode of
        Cmd.MethodEdit -> [ControlTrack.cmd_method_edit]
        _ -> []
    ParseTitle.TempoTrack -> case edit_mode of
        Cmd.MethodEdit -> [ControlTrack.cmd_method_edit]
        _ -> []

-- | Track-specific keymaps.
keymap_cmds :: ParseTitle.Type -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
keymap_cmds = \case
    ParseTitle.NoteTrack -> [Cmd.call $ Cmd.Keymap $ NoteTrackKeymap.keymap]
    _ -> []


-- * misc

-- | Like 'TimeStep.event_step', step to start and end of events.  But also
-- step to the start and end of the events of a parent note track, if any.
event_and_note_step :: Cmd.M m => m TimeStep.TimeStep
event_and_note_step = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    maybe_track <- Info.lookup_track_type block_id tracknum
    note_tracknum <- case Info.track_type <$> maybe_track of
        Nothing -> return Nothing
        Just (Info.Note {}) -> return Nothing
        Just (Info.Pitch Nothing) -> return Nothing
        Just (Info.Pitch (Just note)) -> return $ Just $ Ui.track_tracknum note
        Just (Info.Control tracks) -> firstJusts (map note_tracknum_of tracks)
    let tracknums = TimeStep.TrackNums $
            [tracknum] ++ maybe [] (:[]) note_tracknum
    return $ TimeStep.from_list
        [ TimeStep.EventStart tracknums, TimeStep.EventEnd tracknums
        , TimeStep.BlockEdge
        ]
    where
    note_tracknum_of track = ifM (is_note track)
        (return (Just (Ui.track_tracknum track))) (return Nothing)
    is_note = fmap ParseTitle.is_note_track . Ui.get_track_title
        . Ui.track_id
