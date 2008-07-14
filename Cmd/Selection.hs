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
        (Just sel0, Just sel1) -> auto_scroll view_id sel0 sel1
        _ -> return ()

-- | If @sel1@ has scrolled off the edge of the window, automatically scroll
-- it so that the selection is in view.  @sel0@ is needed to determine the
-- direction of the scroll.
auto_scroll :: (Monad m) => Block.ViewId -> Block.Selection
    -> Block.Selection -> Cmd.CmdT m ()
auto_scroll view_id sel0 sel1 = do
    view <- State.get_view view_id
    let zoom = Block.view_zoom view
    let zoom_offset = calc_time_offset view sel0 sel1
    let track_offset = calc_track_offset view sel0 sel1
    State.set_zoom view_id (zoom { Block.zoom_offset = zoom_offset })
    State.set_track_scroll view_id track_offset
    Cmd.sync_zoom_status view_id

calc_time_offset view sel0 sel1 = time_scroll_with_selection
        sel0 sel1 (Block.zoom_offset zoom) view_end extra_space
    where
    zoom = Block.view_zoom view
    visible_area = Block.visible_time_area view
    view_start = Block.zoom_offset zoom
    view_end = view_start + visible_area
    -- Scroll by 1/4 of the visible screen.
    -- TODO handle out of range drag by scrolling at a constant rate
    extra_space = visible_area / 4

calc_track_offset view sel0 sel1 = max 0 (offset - ruler_width)
    where
    widths = map Block.track_view_width (Block.view_tracks view)
    -- Pesky ruler track doesn't count towards the track scroll.
    ruler_width = Seq.mhead 0 widths
    offset = track_scroll_with_selection sel0 sel1
        (Block.view_track_scroll view + ruler_width)
        (Block.visible_track_area view) widths

-- | If the moving edge of the selection is going out of the visible area,
-- scroll to put it into view plus a little extra space.  If both edges are
-- moving, don't scroll.
time_scroll_with_selection :: Block.Selection -> Block.Selection
    -> TrackPos -> TrackPos -- ^ bounds of the visible track area
    -> TrackPos -- ^ amount an out of range selection should scroll
    -> TrackPos -- ^ time offset should be this much
time_scroll_with_selection old_sel new_sel view_start view_end extra
    | scroll_down && scroll_up = view_start
    | scroll_down = view_start + (new_end - view_end) + extra
    | scroll_up = new_start - extra
    | otherwise = view_start
    where
    (old_start, old_end) = Block.sel_range old_sel
    (new_start, new_end) = Block.sel_range new_sel
    scroll_down = new_end > old_end && new_end > view_end
    scroll_up = new_start < old_start && new_start < view_start

track_scroll_with_selection old_sel new_sel view_start view_end widths
    | scroll_right && scroll_left = view_start
    | scroll_right = view_start + (new_end - view_end)
    | scroll_left = new_start
    | otherwise = view_start
    where
    track_range sel = (sum (take start widths), sum (take end widths))
        where
        start = Block.sel_start_track sel
        end = start + Block.sel_tracks sel
    (old_start, old_end) = track_range old_sel
    (new_start, new_end) = track_range new_sel
    scroll_right = new_end > old_end && new_end > view_end
    scroll_left = new_start < old_start && new_start < view_start

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
selection_from_mouse (track0, pos0) (track1, pos1) = Block.Selection
        (max 0 (min track0 track1)) (max (TrackPos 0) (min p0 p1))
        (abs (track0 - track1) + 1) (abs (p0 - p1))
    where
    p0 = max 0 pos0
    p1 = max 0 pos1

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
