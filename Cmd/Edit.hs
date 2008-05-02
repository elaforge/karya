module Cmd.Edit where
import Control.Monad
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
-- import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd

import qualified App.Config as Config


newtype PitchClass = PitchClass Int
    deriving (Show)

-- | Turn edit mode on and off, changing the color of the track_box as
-- a reminder.
cmd_toggle_edit :: Cmd.CmdM
cmd_toggle_edit = do
    view_id <- Cmd.get_active_view
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    edit <- fmap Cmd.state_edit_mode Cmd.get_state

    Cmd.modify_state $ \st -> st { Cmd.state_edit_mode = not edit }
    State.set_block_config (Block.view_block view) $ (Block.block_config block)
        { Block.config_track_box_color = edit_color (not edit) }
    return Cmd.Done

-- | Insert an event with the given pitch at the current insert point.
cmd_insert_pitch :: PitchClass -> Cmd.CmdM
cmd_insert_pitch pitch = do
    -- edit <- fmap Cmd.state_edit_mode Cmd.get_state
    -- when (not edit) Cmd.abort

    (track_ids, sel) <- selected_tracks Config.insert_selnum
    track_id <- Cmd.require $ case track_ids of
        (x:_) -> Just x
        _ -> Nothing
    let insert_pos = Block.sel_start_pos sel
    Log.debug $ "insert pitch " ++ show pitch ++ " at "
        ++ show (track_id, insert_pos)
    let event = Config.event (pitch_class_note pitch) (TrackPos 16)
    State.insert_events track_id [(insert_pos, event)]
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

pitch_class_note (PitchClass pitch) = show octave ++ pitch_notes !! p
    where
    octave = pitch `div` 12
    p = pitch `mod` 12

pitch_notes =
    ["c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-"]


event_track_at block_id tracknum = do
    track <- State.track_at block_id tracknum
    case track of
        Just (Block.T track_id _) -> return (Just track_id)
        _ -> return Nothing

edit_color True = Config.edit_color
edit_color False = Config.box_color
