{- | Commands dealing with selection / cursor movement.

-}
module Cmd.Selection where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.TimeStep as TimeStep

import qualified App.Config as Config


-- | Advance the given selection by the current step.
-- Require: active block, insert_selection is set
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.
cmd_step_selection :: Block.SelNum -> TimeStep.TimeDirection -> Cmd.CmdId
cmd_step_selection selnum dir = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum

    new_pos <- step_from
        (Block.sel_start_track sel) (Block.sel_start_pos sel) dir
    select_and_scroll view_id selnum
        (Block.point_selection (Block.sel_start_track sel) new_pos)
    return Cmd.Done

-- | All selection setting that wants to automatically scroll the window to
-- follow the selection should use this function.
select_and_scroll :: (Monad m) =>
     Block.ViewId -> Block.SelNum -> Maybe Block.Selection -> Cmd.CmdT m ()
select_and_scroll view_id selnum sel = do
    old_sel <- State.get_selection view_id selnum
    State.set_selection view_id selnum sel
    sync_selection_status view_id
    case (old_sel, sel) of
        (Just sel1, Just sel2) -> auto_scroll view_id sel1 sel2
        _ -> return ()

-- | If @sel2@ has scrolled off the edge of the window, automatically scroll
-- it so that the selection is in view.  @sel1@ is needed to determine the
-- direction of the scroll.
-- TODO implement track scrolling
-- TODO this is sort of screwed up for drag scrolling
-- but don't put much more work into this until I know whether I'm going to
-- move it to c++ or not
auto_scroll :: (Monad m) => Block.ViewId -> Block.Selection
    -> Block.Selection -> Cmd.CmdT m ()
auto_scroll view_id sel1 sel2 = do
    view <- State.get_view view_id
    let zoom = Block.view_zoom view
        view_start = Block.zoom_offset zoom
        view_end = view_start + Block.visible_view_area view
        -- Scroll by 1/4 of the visible screen.
        extra_space = Block.visible_view_area view / 4
        offset = scroll_with_selection sel1 sel2
            (Block.zoom_offset zoom) view_end extra_space
    State.set_zoom view_id (zoom { Block.zoom_offset = offset })
    Cmd.sync_zoom_status view_id

-- | If the moving edge of the selection is going out of the visible area,
-- scroll to put it into view plus a little extra space.  If both edges are
-- moving, don't scroll.
scroll_with_selection (Block.Selection num1 start1 tracks1 dur1)
        (Block.Selection num2 start2 tracks2 dur2) view_start view_end extra
    | start2 >= start1 && end2 > view_end =
        end2 - (view_end - view_start) + extra -- scroll down
    | start2 < view_start =
        start1 - extra -- scroll up
    | otherwise = view_start -- no scroll
    where
    end1 = start1 + dur1
    end2 = start2 + dur2

sync_selection_status view_id = do
    maybe_sel <- State.get_selection view_id Config.insert_selnum
    Cmd.set_view_status view_id "sel" (fmap selection_status maybe_sel)

selection_status :: Block.Selection -> String
selection_status sel =
    showp start ++ if start == end then "" else "-" ++ showp end
    where
    (start, end) = Block.sel_range sel
    showp = Ui.Types.pretty_pos

-- | Advance the insert selection by the current step, which is a popular thing
-- to do.
cmd_advance_insert :: Cmd.CmdId
cmd_advance_insert = cmd_step_selection Config.insert_selnum TimeStep.Advance

-- | Move the selection across tracks by @nshift@.
cmd_shift_selection :: Block.SelNum -> Int -> Cmd.CmdId
cmd_shift_selection selnum nshift = do
    view_id <- Cmd.get_focused_view
    block <- State.block_of_view view_id
    sel <- Cmd.require =<< State.get_selection view_id selnum
    let sel' = shift_selection nshift (length (Block.block_tracks block)) sel
    select_and_scroll view_id selnum (Just sel')
    return Cmd.Done

-- | Set the selection based on a click or drag.
--
-- TODO: support snap
cmd_mouse_selection :: Int -> Block.SelNum -> Cmd.Cmd
cmd_mouse_selection btn selnum msg = do
    (mod, mouse_at) <- Cmd.require (mouse_mod msg)
    msg_btn <- Cmd.require (Cmd.mouse_mod_btn mod)
    keys_down <- Cmd.keys_down
    Log.debug $ "mod btn " ++ show mod ++ " in " ++ show (Map.elems keys_down)
    when (msg_btn /= btn) Cmd.abort

    let down_at = case Map.lookup (Cmd.modifier_map_key mod) keys_down of
            Just (Cmd.MouseMod _btn (Just down_at)) -> down_at
            _ -> mouse_at

    view_id <- Cmd.get_focused_view
    mouse_at <- Cmd.require $ case mod of
        Cmd.MouseMod _ (Just at) -> Just at
        _ -> Nothing
    let sel = selection_from_mouse down_at mouse_at
    Log.debug $ "drag sel from " ++ show down_at ++ " --> " ++ show mouse_at
    select_and_scroll view_id selnum (Just sel)
    return Cmd.Done

mouse_mod :: Msg.Msg -> Maybe (Cmd.Modifier, (Block.TrackNum, TrackPos))
mouse_mod msg = do
    mouse <- Msg.mouse msg
    btn <- case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn -> Just btn
        UiMsg.MouseDrag btn -> Just btn
        UiMsg.MouseUp btn -> Just btn
        _ -> Nothing
    track_pos <- Msg.context_track_pos msg
    return $ (Cmd.MouseMod btn (Just track_pos), track_pos)

-- | Create a selection between the two points.
selection_from_mouse ::
    (Block.TrackNum, TrackPos) -> (Block.TrackNum, TrackPos) -> Block.Selection
selection_from_mouse (track1, pos1) (track2, pos2) =
    Block.Selection (min track1 track2) (min pos1 pos2)
        (abs (track1 - track2) + 1) (abs (pos1 - pos2))

-- | Shift the selection to the right or left, clipping it if it hits the edges
-- of the displayed tracks.
shift_selection :: Block.TrackNum -> Block.TrackNum -> Block.Selection
    -> Block.Selection
shift_selection nshift ntracks sel = sel
        { Block.sel_start_track = start'
        , Block.sel_tracks = min (Block.sel_tracks sel) (ntracks - start')
        }
    where
    start = Block.sel_start_track sel
    max_track = ntracks - 1
    start' = between 0 max_track (nshift + start)

between low high n = min high (max low n)

-- * util

step_from :: (Monad m) => Block.TrackNum -> TrackPos -> TimeStep.TimeDirection
    -> Cmd.CmdT m TrackPos
step_from tracknum pos direction = do
    block <- State.block_of_view =<< Cmd.get_focused_view
    step <- Cmd.get_current_step
    ruler_id <- Cmd.require (relevant_ruler block tracknum)
    ruler <- State.get_ruler ruler_id
    let msg = case direction of
            TimeStep.Advance -> "advance to "
            TimeStep.Rewind -> "rewind from "
    case TimeStep.stepper direction step (Ruler.ruler_marklists ruler) pos of
        Nothing -> do
            Log.notice $ "can't " ++ msg ++ show step ++ " from " ++ show pos
            Cmd.abort
        Just next_pos -> return next_pos

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler that has all the given marklist names.  This includes ruler
-- tracks and the rulers of event tracks.
relevant_ruler :: Block.Block -> Block.TrackNum -> Maybe Ruler.RulerId
relevant_ruler block tracknum = Seq.at (Block.ruler_ids_of in_order) 0
    where
    in_order = map snd $ dropWhile ((/=tracknum) . fst) $ reverse $
        zip [0..] (Block.block_tracks block)

-- | Specialized 'selected_tracks' that gets the pos and track of the upper
-- left corner of the insert selection.
get_insert_pos :: (Monad m) =>
    Cmd.CmdT m (TrackPos, Block.TrackNum, Track.TrackId)
get_insert_pos = do
    (track_ids, sel) <- selected_tracks Config.insert_selnum
    track_id <- Cmd.require (track_ids `Seq.at` 0)
    return (Block.sel_start_pos sel, Block.sel_start_track sel, track_id)

-- | Get the events in the selection, along with their tracks.
selected_events :: (Monad m) =>
    Block.SelNum -> Cmd.CmdT m [(Track.TrackId, [Track.PosEvent])]
selected_events selnum = do
    (track_ids, sel) <- selected_tracks selnum
    tracks <- mapM State.get_track track_ids
    return [(track_id, events_in_sel sel track)
        | (track_id, track) <- zip track_ids tracks]

events_in_sel sel track =
    Track.events_in_range start end (Track.track_events track)
    where (start, end) = Block.sel_range sel

-- | Get selected event tracks along with the selection.  The tracks are
-- returned in the same order that they occur in the block.
selected_tracks :: (Monad m) =>
    Block.SelNum -> Cmd.CmdT m ([Track.TrackId], Block.Selection)
selected_tracks selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum
    view <- State.get_view view_id
    let start = Block.sel_start_track sel
    tracks <- mapM (State.event_track_at (Block.view_block view))
        [start .. start + Block.sel_tracks sel - 1]
    return (Maybe.catMaybes tracks, sel)
