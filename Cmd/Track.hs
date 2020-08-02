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
    mb_track_id <- Ui.event_track_at block_id tracknum
    -- TODO this is overkill, use ParseTitle.track_type
    -- Well, except 'input_cmds' checks if Note children is null.
    track <- Cmd.abort_unless =<< Info.lookup_track_type block_id tracknum
    mb_resolved <- maybe (return Nothing) (lookup_inst block_id) mb_track_id
    mb_title <- traverse Ui.get_track_title mb_track_id
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
        [ case (mb_title, mb_resolved) of
            (Just title, Just resolved) | ParseTitle.is_note_track title ->
                map Cmd.call $ Cmd.inst_cmds $ Common.common_code $
                    Inst.inst_common $ Cmd.inst_instrument resolved
            _ -> []
        , (:[]) $ Edit.handle_floating_input $
            case Info.track_type track of
                Info.Note {} -> False
                _ -> True
        , (:[]) $ with_input (input_cmds edit_mode track)
        , track_type_cmds edit_mode track
        , keymap_cmds track
        ]

lookup_inst :: Cmd.M m => BlockId -> TrackId -> m (Maybe Cmd.ResolvedInstrument)
lookup_inst block_id track_id =
    justm (Perf.lookup_instrument (block_id, Just track_id)) $ \inst ->
    Cmd.lookup_instrument inst

-- | Cmds that use InputNotes, and hence must be called with
-- 'NoteEntry.cmds_with_input'.
input_cmds :: Cmd.EditMode -> Info.Track -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
input_cmds edit_mode track = universal ++ case Info.track_type track of
    Info.Note _ children
        | null children -> case edit_mode of
            Cmd.ValEdit -> [NoteTrack.cmd_val_edit]
            _ -> []
        | otherwise -> []
    Info.Pitch {} -> case edit_mode of
        Cmd.ValEdit -> [PitchTrack.cmd_val_edit]
        _ -> []
    Info.Control {} -> case edit_mode of
        Cmd.ValEdit
            | is_tempo -> [ControlTrack.cmd_tempo_val_edit]
            | otherwise -> [ControlTrack.cmd_val_edit]
        _ -> []
    where
    universal =
        [ PitchTrack.cmd_record_note_status
        , MidiThru.cmd_midi_thru
        , NoteEntry.floating_input_insert
        ]
    is_tempo = ParseTitle.is_tempo_track $
        Ui.track_title (Info.track_info track)

-- | Track-specific Cmds.
track_type_cmds :: Cmd.EditMode -> Info.Track
    -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
track_type_cmds edit_mode track = case Info.track_type track of
    Info.Note {} -> case edit_mode of
        Cmd.MethodEdit -> [NoteTrack.cmd_method_edit]
        _ -> []
    Info.Pitch {} -> case edit_mode of
        Cmd.MethodEdit -> [PitchTrack.cmd_method_edit]
        _ -> []
    Info.Control {} -> case edit_mode of
        Cmd.MethodEdit -> [ControlTrack.cmd_method_edit]
        _ -> []

-- | Track-specific keymaps.
keymap_cmds :: Info.Track -> [Msg.Msg -> Cmd.CmdId Cmd.Status]
keymap_cmds track = case Info.track_type track of
    Info.Note {} -> [Cmd.call $ Cmd.Keymap $ NoteTrackKeymap.keymap]
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
