{- | Event editing commands.

    Local state:

    - pitch transpose: Constant value to add to entered pitch numbers.
    Blocks and tracks may override this, of course.  It's better as
    a state than as setting the keymap since that's easier to save.
-}
module Cmd.Edit where
import Control.Monad
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

import qualified Perform.Midi.Instrument as Instrument

import qualified App.Config as Config


-- * insert note events

-- | A physically played key, either on the midi keyboard or letter keyboard.
-- This isn't the same as the midi note number because the midi note number
-- represents a certaint tempered pitch, and this one is still user input; it
-- has yet to be mapped to a pitch (and may not be, for unpitched sounds).
type NoteNumber = Int

-- | Turn edit mode on and off, changing the color of the edit_box as
-- a reminder.
cmd_toggle_edit :: Cmd.CmdId
cmd_toggle_edit = do
    view_id <- Cmd.get_focused_view
    view <- State.get_view view_id
    edit <- fmap Cmd.state_edit_mode Cmd.get_state
    Cmd.modify_state $ \st -> st { Cmd.state_edit_mode = not edit }
    State.set_edit_box (Block.view_block view) (edit_color (not edit))
    return Cmd.Done

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
    (track_id, insert_pos) <- get_insert_pos
    Log.debug $ "insert pitch " ++ show pitch ++ " at "
        ++ show (track_id, insert_pos)
    let event = Config.event (Twelve.pitch_event pitch) (TrackPos 16)
    State.insert_events track_id [(insert_pos, event)]

-- * controller entry

cmd_controller_entry :: Msg.Msg -> Cmd.CmdId
cmd_controller_entry msg = do
    key <- Cmd.require (Msg.key msg)
    char <- Cmd.require $ case key of
        Key.KeyChar char -> Just char
        _ -> Nothing
    (track_id, insert_pos) <- get_insert_pos
    track <- State.get_track track_id

    let text = Maybe.fromMaybe "" $ fmap Event.event_text
            (Track.event_at (Track.track_events track) insert_pos)
        event = Config.event (text ++ [char]) (TrackPos 1)
    Log.debug $ "modify control event at " ++ show insert_pos ++ " "
        ++ show text ++ " ++ " ++ show char
    State.insert_events track_id [(insert_pos, event)]
    return Cmd.Done


-- * other event cmds

-- | If the insertion selection is a point, remove any event under it.  If it's
-- a range, remove all events within its half-open extent.
cmd_remove_events :: Cmd.CmdId
cmd_remove_events = do
    (track_ids, sel) <- selected_tracks Config.insert_selnum
    let start = Block.sel_start_pos sel
        dur = Block.sel_duration sel
    forM_ track_ids $ \track_id -> if dur == TrackPos 0
        then State.remove_event track_id start
        else State.remove_events track_id start (start + dur)
    return Cmd.Done

cmd_set_step :: TimeStep.TimeStep -> Cmd.CmdId
cmd_set_step step = do
    Cmd.modify_state $ \st -> st { Cmd.state_step = step }
    return Cmd.Done

cmd_meter_step :: Int -> Cmd.CmdId
cmd_meter_step rank = do
    cmd_set_step (TimeStep.UntilMark
        (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank rank))
    -- TODO this should go to a global state display either in the logviewer
    -- or seperate
    Log.notice $ "set step: meter " ++ show rank
    return Cmd.Done

-- * util

-- | Specialized 'selected_tracks' that gets the pos and track of the upper
-- left corner of the insert selection.
get_insert_pos :: (Monad m) => Cmd.CmdT m (Track.TrackId, TrackPos)
get_insert_pos = do
    (track_ids, sel) <- selected_tracks Config.insert_selnum
    track_id <- Cmd.require (track_ids `Seq.at` 0)
    return (track_id, Block.sel_start_pos sel)

selected_tracks :: (Monad m) =>
    Block.SelNum -> Cmd.CmdT m ([Track.TrackId], Block.Selection)
selected_tracks selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum
    view <- State.get_view view_id
    let start = Block.sel_start_track sel
    tracks <- mapM (event_track_at (Block.view_block view))
        [start .. start + Block.sel_tracks sel - 1]
    return (Maybe.catMaybes tracks, sel)

event_track_at block_id tracknum = do
    track <- State.track_at block_id tracknum
    case track of
        Just (Block.TId track_id _) -> return (Just track_id)
        _ -> return Nothing

edit_color True = Config.edit_color
edit_color False = Config.box_color
