{- | Event editing commands.

    Local state:

    - pitch transpose: Constant value to add to entered pitch numbers.
    Blocks and tracks may override this, of course.  It's better as
    a state than as setting the keymap since that's easier to save.
-}
module Cmd.Edit where
import Control.Monad
import qualified Data.Maybe as Maybe

import qualified App.Config as Config
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
-- import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Midi.Midi as Midi

import qualified Perform.Pitch as Pitch
import qualified Derive.Twelve as Twelve

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep


-- | Turn edit mode on and off, changing the color of the edit_box as
-- a reminder.
cmd_toggle_edit :: Cmd.CmdM
cmd_toggle_edit = do
    view_id <- Cmd.get_active_view
    view <- State.get_view view_id
    edit <- fmap Cmd.state_edit_mode Cmd.get_state
    Cmd.modify_state $ \st -> st { Cmd.state_edit_mode = not edit }
    State.set_edit_box (Block.view_block view) (edit_color (not edit))
    return Cmd.Done

-- | Insert an event with the given pitch at the current insert point.
-- This actually takes a pitch_num, which represents the position on the
-- keyboard and should be mapped to the correct pitch.
cmd_insert_pitch :: Int -> Cmd.CmdM
cmd_insert_pitch pitch_num = do
    octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    insert_pitch $
        Pitch.from_midi_nn ("kbd " ++ show pitch_num) (pitch_num + 12*octave)

insert_pitch :: Pitch.Pitch -> Cmd.CmdM
insert_pitch pitch = do
    (track_ids, sel) <- selected_tracks Config.insert_selnum
    track_id <- Cmd.require $ case track_ids of
        (x:_) -> Just x
        _ -> Nothing
    let insert_pos = Block.sel_start_pos sel
    Log.debug $ "insert pitch " ++ show pitch ++ " at "
        ++ show (track_id, insert_pos)
    let event = Config.event (Twelve.pitch_event pitch) (TrackPos 16)
    State.insert_events track_id [(insert_pos, event)]
    return Cmd.Done

cmd_midi_thru msg = do
    (dev, chan, msg) <- case msg of
        Msg.Midi (Midi.ReadMessage dev _ts (Midi.ChannelMessage chan msg)) ->
            return (dev, chan, msg)
        _ -> Cmd.abort
    Cmd.midi (read_dev_to_write_dev dev) (Midi.ChannelMessage chan msg)
    return Cmd.Continue

read_dev_to_write_dev (Midi.ReadDevice name) = Midi.WriteDevice name

cmd_insert_midi_note msg = do
    key <- case msg of
        Msg.Midi (Midi.ReadMessage _dev _ts (Midi.ChannelMessage _chan
            (Midi.NoteOn key _vel))) ->
                return key
        _ -> Cmd.abort
    insert_pitch (Pitch.from_midi_nn ("midi " ++ show key) key)
    return Cmd.Done

-- | If the insertion selection is a point, remove any event under it.  If it's
-- a range, remove all events within its half-open extent.
cmd_remove_events :: Cmd.CmdM
cmd_remove_events = do
    (track_ids, sel) <- selected_tracks Config.insert_selnum
    let start = Block.sel_start_pos sel
        dur = Block.sel_duration sel
    forM_ track_ids $ \track_id -> if dur == TrackPos 0
        then State.remove_event track_id start
        else State.remove_events track_id start (start + dur)
    return Cmd.Done

cmd_set_current_step :: TimeStep.TimeStep -> Cmd.CmdM
cmd_set_current_step step = do
    Cmd.modify_state $ \st -> st { Cmd.state_current_step = step }
    return Cmd.Done

cmd_meter_step :: Int -> Cmd.CmdM
cmd_meter_step rank = do
    cmd_set_current_step (TimeStep.UntilMark
        (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank rank))
    -- TODO this should go to a global state display either in the logviewer
    -- or seperate
    Log.notice $ "set step: meter " ++ show rank
    return Cmd.Done

-- * util

selected_tracks :: (Monad m) =>
    Block.SelNum -> Cmd.CmdT m ([Track.TrackId], Block.Selection)
selected_tracks selnum = do
    view_id <- Cmd.get_active_view
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
