{- | Event editing commands.
-}
module Cmd.Edit where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Key as Key
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Midi.Midi as Midi

import qualified Perform.Pitch as Pitch
import qualified Derive.Twelve as Twelve

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Selection as Selection

import qualified Perform.Midi.Instrument as Instrument

import qualified App.Config as Config


-- * insert note events

-- | A physically played key, either on the midi keyboard or letter keyboard.
-- This isn't the same as the midi note number because the midi note number
-- represents a certain tempered pitch, while this is still user input.  It
-- has yet to be mapped to a pitch (and may not be, for unpitched sounds).
type NoteNumber = Int

-- | Turn edit mode on and off, changing the color of the edit_box as
-- a reminder.
cmd_toggle_edit :: Cmd.CmdId
cmd_toggle_edit = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    Cmd.modify_state $ \st -> st { Cmd.state_edit_mode = not edit_mode }
    sync_edit_box_status
    return Cmd.Done

sync_edit_box_status :: (Monad m) => Cmd.CmdT m ()
sync_edit_box_status = do
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    block_ids <- fmap (Map.keys . State.state_blocks) State.get
    mapM_ (flip State.set_edit_box (edit_color edit_mode)) block_ids

edit_color True = Config.edit_color
edit_color False = Config.box_color

-- | Insert an event with the given pitch at the current insert point.
-- This actually takes a nn, which represents the position on the
-- keyboard and should be mapped to the correct pitch.
cmd_insert_pitch :: NoteNumber -> Cmd.CmdId
cmd_insert_pitch nn = pitch_from_kbd nn >>= insert_pitch
    >> return Cmd.Done

-- | Send a midi thru msg for the given nn.  Intended for midi "thru" for kbd
-- entry.
cmd_note_on :: (Monad m) =>
    Instrument.Addr -> NoteNumber -> Cmd.CmdT m Cmd.Status
cmd_note_on (wdev, chan) nn = do
    pitch <- pitch_from_kbd nn
    let msg = Midi.ChannelMessage chan (Midi.NoteOn (Pitch.midi_nn pitch) 100)
    Cmd.midi wdev msg
    return Cmd.Continue

cmd_note_off :: (Monad m) =>
    Instrument.Addr -> NoteNumber -> Cmd.CmdT m Cmd.Status
cmd_note_off (wdev, chan) nn = do
    pitch <- pitch_from_kbd nn
    let msg = Midi.ChannelMessage chan (Midi.NoteOff (Pitch.midi_nn pitch) 0)
    Cmd.midi wdev msg
    return Cmd.Continue

-- | Send midi thru, remapping notes and controllers to the given Addr.
cmd_midi_thru :: Instrument.Addr -> Cmd.Cmd
cmd_midi_thru (wdev, chan) msg = do
    chan_msg <- case msg of
        Msg.Midi (Midi.ReadMessage _dev _ts (Midi.ChannelMessage _chan msg)) ->
            return msg
        _ -> Cmd.abort
    Cmd.midi wdev (Midi.ChannelMessage chan chan_msg)
    return Cmd.Continue

cmd_insert_midi_note :: (Monad m) => Msg.Msg -> Cmd.CmdT m Cmd.Status
cmd_insert_midi_note msg = do
    key <- case msg of
        Msg.Midi (Midi.ReadMessage _dev _ts (Midi.ChannelMessage _chan
            (Midi.NoteOn key _vel))) ->
                return key
        _ -> Cmd.abort
    insert_pitch (Pitch.from_midi_nn ("midi " ++ show key) key)
    return Cmd.Done

-- ** implementation

-- read_dev_to_write_dev (Midi.ReadDevice name) = Midi.WriteDevice name

pitch_from_kbd nn = do
    oct <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    return $ Pitch.from_midi_nn ("kbd " ++ show nn) (nn + 12*oct)

-- | Actually convert the given pitch to an event and insert it.
insert_pitch :: (Monad m) => Pitch.Pitch -> Cmd.CmdT m ()
insert_pitch pitch = do
    (insert_pos, tracknum, track_id) <- Selection.get_insert_pos
    end_pos <- Selection.step_from tracknum insert_pos TimeStep.Advance
    -- assert (end_pos >= insert_pos)

    Log.debug $ "insert pitch " ++ show pitch ++ " at "
        ++ show (track_id, insert_pos)
    let event = Config.event (Twelve.pitch_event pitch) (end_pos - insert_pos)
    State.insert_events track_id [(insert_pos, event)]

-- * controller entry

-- | Receive keystrokes to edit a controller track.
cmd_controller_entry :: Msg.Msg -> Cmd.CmdId
cmd_controller_entry msg = do
    key <- Cmd.require (Msg.key msg)
    char <- Cmd.require $ case key of
        Key.KeyChar char -> Just char
        _ -> Nothing
    keys_down <- fmap Map.keys Cmd.keys_down
    when (any (Maybe.isNothing . Cmd.modifier_key) keys_down) $
        Cmd.abort

    (insert_pos, _, track_id) <- Selection.get_insert_pos
    track <- State.get_track track_id

    let text = Maybe.fromMaybe "" $ fmap Event.event_text
            (Track.event_at (Track.track_events track) insert_pos)
        event = Config.event (text ++ [char]) (TrackPos 0)
    Log.debug $ "modify control event at " ++ show insert_pos ++ " "
        ++ show text ++ " ++ " ++ show char
    State.insert_events track_id [(insert_pos, event)]
    return Cmd.Done


-- * other event cmds

-- | If the insertion selection is a point, remove any event under it.  If it's
-- a range, remove all events within its half-open extent.
cmd_remove_events :: Cmd.CmdId
cmd_remove_events = do
    (track_ids, sel) <- Selection.selected_tracks Config.insert_selnum
    let start = Block.sel_start_pos sel
        dur = Block.sel_duration sel
    forM_ track_ids $ \track_id -> if dur == TrackPos 0
        then State.remove_event track_id start
        else State.remove_events track_id start (start + dur)
    return Cmd.Done

cmd_meter_step :: Int -> Cmd.CmdId
cmd_meter_step rank = do
    let step = (TimeStep.UntilMark
            (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank rank))
    Cmd.modify_state $ \st -> st { Cmd.state_step = step }
    sync_step_status
    return Cmd.Done

sync_step_status :: (Monad m) => Cmd.CmdT m ()
sync_step_status = do
    step <- fmap Cmd.state_step Cmd.get_state
    Cmd.set_status status_step (Just (show_step step))

show_step (TimeStep.Absolute pos) = "abs:" ++ show pos
show_step (TimeStep.UntilMark mlists match) =
    "until:" ++ show_marklists mlists ++ "/" ++ show_match match
show_step (TimeStep.MarkDistance mlists match) =
    "dist:" ++ show_marklists mlists ++ "/" ++ show_match match

show_match (TimeStep.MatchRank rank) = "r" ++ show rank

show_marklists TimeStep.AllMarklists = "all"
show_marklists (TimeStep.NamedMarklists mlists) = Seq.join "," mlists

cmd_modify_octave :: (Monad m) => (Int -> Int) -> Cmd.CmdM m
cmd_modify_octave f = do
    Cmd.modify_state $ \st -> st
        { Cmd.state_kbd_entry_octave = f (Cmd.state_kbd_entry_octave st) }
    sync_octave_status
    return Cmd.Done

sync_octave_status :: (Monad m) => Cmd.CmdT m ()
sync_octave_status = do
    octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    Cmd.set_status status_octave (Just (show octave))

status_octave = "8ve"
status_step = "step"

-- * util
