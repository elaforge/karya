{- | Commands dealing with selection / cursor movement.
-}
module Cmd.Selection where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Control as Control
import qualified Util.Seq as Seq
import qualified Util.Log as Log
import qualified Util.Num as Num

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Schema.Default as Default

import qualified App.Config as Config


-- * cmds

-- | Advance the given selection by the current step.
-- Require: active block, insert_selection is set
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.  If @extend@ is true, extend the current
-- selection instead of setting a new point selection.
cmd_step_selection :: (Monad m) => Types.SelNum -> TimeStep.TimeDirection
    -> Bool -> Cmd.CmdT m ()
cmd_step_selection selnum dir extend = do
    view_id <- Cmd.get_focused_view
    Types.Selection start_track start_pos cur_track cur_pos <-
        Cmd.require =<< State.get_selection view_id selnum

    new_pos <- step_from cur_track cur_pos dir
    let new_sel = if extend
            then Types.selection start_track start_pos cur_track new_pos
            else Types.point_selection start_track new_pos
    select_and_scroll view_id selnum new_sel

-- | Advance the insert selection by the current step, which is a popular thing
-- to do.
cmd_advance_insert :: (Monad m) => Cmd.CmdT m ()
cmd_advance_insert =
    cmd_step_selection Config.insert_selnum TimeStep.Advance False

-- | Move the selection across tracks by @shift@, skipping non-event tracks
-- and collapsed tracks.
--
-- If @extend@ is true, extend the current selection instead of setting a new
-- selection.
cmd_shift_selection :: (Monad m) =>
    Types.SelNum -> TrackNum -> Bool -> Cmd.CmdT m ()
cmd_shift_selection selnum shift extend = do
    view_id <- Cmd.get_focused_view
    block <- State.block_of_view view_id
    sel <- Cmd.require =<< State.get_selection view_id selnum
    let sel' = shift_sel block shift sel
    select_and_scroll view_id selnum
        (Just (if extend then merge_sel sel sel' else sel'))


-- | Shift the selection along selectable tracks, clipping if it's out of
-- range.  While the sel_cur_track won't be on a non-selectable track after
-- this, the selection may still include one.
shift_sel :: Block.Block -> TrackNum -> Types.Selection -> Types.Selection
shift_sel block shift sel =
    Types.sel_modify_tracks (Num.clamp 0 max_track . (+shift2)) sel
    where
    new_tracknum = shift_tracknum block (Types.sel_cur_track sel) shift
    shift2 = new_tracknum - Types.sel_cur_track sel
    max_track = length (Block.block_tracks block)

-- | Shift a tracknum to another track, skipping unselectable tracks.
shift_tracknum :: Block.Block -> TrackNum -> Int -> TrackNum
shift_tracknum block tracknum shift
    | shift >= 0 = Seq.mhead tracknum id $
        drop shift $ dropWhile (<tracknum) selectable
    | otherwise = Seq.mhead tracknum id $
        drop (-shift) $ dropWhile (>tracknum) (List.reverse selectable)
    where selectable = selectable_tracks block

-- | Get the tracknums from a block that should be selectable, including
-- the tracknum one past the end.
selectable_tracks :: Block.Block -> [TrackNum]
selectable_tracks block = (++[length (Block.block_tracks block)]) $ do
    (i, track@(Block.BlockTrack { Block.tracklike_id = Block.TId _ _}))
        <- zip [0..] (Block.block_tracks block)
    guard (Block.Collapse `notElem` Block.track_flags track)
    return i

merge_sel :: Types.Selection -> Types.Selection -> Types.Selection
merge_sel (Types.Selection strack spos _ _) (Types.Selection _ _ ctrack cpos) =
    Types.Selection strack spos ctrack cpos

-- | Set the selection based on a click or drag.
cmd_mouse_selection :: (Monad m) =>
    Int -> Types.SelNum -> Bool -> Msg.Msg -> Cmd.CmdT m ()
cmd_mouse_selection btn selnum extend msg = do
    (down_tracknum, down_pos, mouse_tracknum, mouse_pos) <- mouse_drag btn msg
    view_id <- Cmd.get_focused_view
    old_sel <- State.get_selection view_id selnum
    let (start_tracknum, start_pos) = case (extend, old_sel) of
            (True, Just (Types.Selection tracknum pos _ _)) -> (tracknum, pos)
            _ -> (down_tracknum, down_pos)
    let sel = Types.selection start_tracknum start_pos mouse_tracknum mouse_pos
    select_and_scroll view_id selnum sel

-- | Like 'cmd_mouse_selection', but snap the selection to the current time
-- step.
cmd_snap_selection :: (Monad m) => Int -> Types.SelNum -> Bool -> Msg.Msg
    -> Cmd.CmdT m ()
cmd_snap_selection btn selnum extend msg = do
    (down_tracknum, _, mouse_tracknum, mouse_pos) <- mouse_drag btn msg
    block_id <- Cmd.get_focused_block
    step <- Cmd.get_current_step
    snap_pos <- TimeStep.snap step block_id mouse_tracknum mouse_pos
    view_id <- Cmd.get_focused_view
    old_sel <- State.get_selection view_id selnum
    let sel = case old_sel of
            _ | Msg.mouse_down msg && not extend || old_sel == Nothing ->
                Types.selection down_tracknum snap_pos mouse_tracknum snap_pos
            Just (Types.Selection tracknum pos _ _) ->
                Types.selection tracknum pos mouse_tracknum snap_pos
            _ -> error "not reached" -- ghc doesn't realize it is exhaustive
    select_and_scroll view_id selnum sel

-- | Get the dragged range, or abort if this isn't a drag Msg.
mouse_drag :: (Monad m) => Int -> Msg.Msg
    -> Cmd.CmdT m (TrackNum, TrackPos, TrackNum, TrackPos)
mouse_drag btn msg = do
    (mod, (mouse_tracknum, mouse_pos)) <- Cmd.require (mouse_mod msg)
    msg_btn <- Cmd.require (Cmd.mouse_mod_btn mod)
    keys_down <- Cmd.keys_down
    -- The button down should be the same one as expected.
    when (msg_btn /= btn) Cmd.abort
    let (down_tracknum, down_pos) =
            case Map.lookup (Cmd.strip_modifier mod) keys_down of
                Just (Cmd.MouseMod _btn (Just down_at)) -> down_at
                -- If it's not already held down, it starts here.
                _ -> (mouse_tracknum, mouse_pos)
    return (down_tracknum, down_pos, mouse_tracknum, mouse_pos)

-- * implementation

-- | Handly shortcut for cmd_step_selection.
advance :: (Monad m) => Cmd.CmdT m ()
advance = cmd_step_selection Config.insert_selnum TimeStep.Advance False

-- ** auto scroll

-- | Anyone who wants to set a selection and automatically scroll the window to
-- follow the selection should use this function.
select_and_scroll :: (Monad m) =>
     ViewId -> Types.SelNum -> Maybe Types.Selection -> Cmd.CmdT m ()
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
auto_scroll :: (Monad m) => ViewId -> Types.Selection
    -> Types.Selection -> Cmd.CmdT m ()
auto_scroll view_id sel0 sel1 = do
    view <- State.get_view view_id
    let zoom = Block.view_zoom view
    let zoom_offset = calc_time_offset view sel0 sel1
    let track_offset = calc_track_offset view sel0 sel1
    State.set_zoom view_id (zoom { Types.zoom_offset = zoom_offset })
    State.set_track_scroll view_id track_offset
    Cmd.sync_zoom_status view_id

calc_time_offset view sel0 sel1 = time_scroll_with_selection
        sel0 sel1 (Types.zoom_offset zoom) view_end extra_space
    where
    zoom = Block.view_zoom view
    visible_time = Block.visible_time view
    view_start = Types.zoom_offset zoom
    view_end = view_start + visible_time
    -- Scroll by 1/4 of the visible screen.
    -- TODO handle out of range drag by scrolling at a constant rate
    extra_space = visible_time / 4

calc_track_offset view sel0 sel1 = max 0 (offset - ruler_width)
    where
    widths = map Block.track_view_width (Block.view_tracks view)
    -- Pesky ruler track doesn't count towards the track scroll.
    ruler_width = Seq.mhead 0 id widths
    offset = track_scroll_with_selection sel0 sel1
        (Block.view_track_scroll view + ruler_width)
        (Block.visible_track view) widths

-- | If the moving edge of the selection is going out of the visible area,
-- scroll to put it into view plus a little extra space.  If both edges are
-- moving, don't scroll.
time_scroll_with_selection :: Types.Selection -> Types.Selection
    -> TrackPos -> TrackPos -- ^ bounds of the visible track area
    -> TrackPos -- ^ amount an out of range selection should scroll
    -> TrackPos -- ^ time offset should be this much
time_scroll_with_selection old_sel new_sel view_start view_end extra
    | scroll_down && scroll_up = view_start
    | scroll_down = view_start + (new_end - view_end) + extra
    | scroll_up = new_start - extra
    | otherwise = view_start
    where
    (old_start, old_end) = Types.sel_range old_sel
    (new_start, new_end) = Types.sel_range new_sel
    scroll_down = new_end > old_end && new_end > view_end
    scroll_up = new_start < old_start && new_start < view_start

track_scroll_with_selection old_sel new_sel view_start view_end widths
    | scroll_right && scroll_left = view_start
    | scroll_right = view_start + (new_end - view_end)
    | scroll_left = new_start
    | otherwise = view_start
    where
    track_range sel = (sum (take start widths), sum (take end widths))
        where (start, end) = Types.sel_track_range sel
    (old_start, old_end) = track_range old_sel
    (new_start, new_end) = track_range new_sel
    scroll_right = new_end > old_end && new_end > view_end
    scroll_left = new_start < old_start && new_start < view_start


-- ** status

sync_selection_status :: (Monad m) => ViewId -> Cmd.CmdT m ()
sync_selection_status view_id = do
    maybe_sel <- State.get_selection view_id Config.insert_selnum
    Cmd.set_view_status view_id "sel" (fmap selection_status maybe_sel)
    block_id <- State.block_id_of_view view_id
    Control.when_just maybe_sel $
        Default.set_inst_status block_id . Types.sel_cur_track

selection_status :: Types.Selection -> String
selection_status sel = Types.pretty_pos start
    ++ if start == end then "" else "-" ++ Types.pretty_pos end
    where (start, end) = Types.sel_range sel

-- ** mouse

mouse_mod :: Msg.Msg -> Maybe (Cmd.Modifier, (TrackNum, TrackPos))
mouse_mod msg = do
    mouse <- Msg.mouse msg
    btn <- case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn -> Just btn
        UiMsg.MouseDrag btn -> Just btn
        UiMsg.MouseUp btn -> Just btn
        _ -> Nothing
    track_pos <- Msg.context_track_pos msg
    return $ (Cmd.MouseMod btn (Just track_pos), track_pos)

-- * util

step_from :: (Monad m) => TrackNum -> TrackPos -> TimeStep.TimeDirection
    -> Cmd.CmdT m TrackPos
step_from tracknum pos direction = do
    block_id <- Cmd.get_focused_block
    step <- Cmd.get_current_step
    next <- TimeStep.step_from step direction block_id tracknum pos
    let msg = case direction of
            TimeStep.Advance -> "advance to "
            TimeStep.Rewind -> "rewind from "
    case next of
        Nothing -> do
            Log.notice $ "can't " ++ msg ++ show step ++ " from " ++ show pos
            Cmd.abort
        Just p -> return p

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler that has all the given marklist names.  This includes ruler
-- tracks and the rulers of event tracks.
relevant_ruler :: Block.Block -> TrackNum -> Maybe RulerId
relevant_ruler block tracknum = Seq.at (Block.ruler_ids_of in_order) 0
    where
    in_order = map (Block.tracklike_id . snd) $ dropWhile ((/=tracknum) . fst) $
        reverse $ zip [0..] (Block.block_tracks block)


-- I return a whole bunch of stuff and let the caller decide which it wants.
type SelInfo = (BlockId, TrackNum, TrackId, TrackPos)

-- | Get the "insert position", which is the upper left corner of the insert
-- selection.  Abort if it's not an event track.
--
-- I return a whole bunch of stuff and let the caller decide which it wants.
get_insert :: (Monad m) => Cmd.CmdT m (BlockId, TrackNum, TrackId, TrackPos)
get_insert = do
    (block_id, tracknum, pos) <- get_insert_any
    track_id <- Cmd.require =<< State.event_track_at block_id tracknum
    return (block_id, tracknum, track_id, pos)

-- | Return the leftmost tracknum and trackpos, even if it's not an event
-- track.
get_insert_any :: (Monad m) => Cmd.CmdT m (BlockId, TrackNum, TrackPos)
get_insert_any = do
    (view_id, sel) <- get_selection Config.insert_selnum
    block_id <- State.block_id_of_view view_id
    return (block_id, Types.sel_start_track sel, Types.sel_start_pos sel)


-- | Get the start and end of the selection, along with the events that fall
-- within it, by track.
events :: (Monad m) =>
    Bool -- ^ If true, a point selection will get the event on or before it.
    -> Cmd.CmdT m (TrackPos, TrackPos, [(TrackId, [Track.PosEvent])])
events = events_selnum Config.insert_selnum

events_selnum :: (Monad m) => Types.SelNum
    -> Bool -- ^ If true, a point selection will get the event on or before it.
    -> Cmd.CmdT m (TrackPos, TrackPos, [(TrackId, [Track.PosEvent])])
events_selnum selnum point_prev = do
    (_, track_ids, start, end) <- tracks_selnum selnum
    tracks <- mapM State.get_track track_ids
    let get_events = if point_prev && start == end
            then event_before else events_in_range
        track_events = [(track_id, get_events start end track)
            | (track_id, track) <- zip track_ids tracks]
    return (start, end, track_events)

-- | A variant of 'selected_events'.  Get events starting with the one at or
-- before the selection, and include the first one after the selection.
events_around :: (Monad m) =>
    Cmd.CmdT m [(TrackId, [Track.PosEvent], Maybe Track.PosEvent)]
events_around = events_around_selnum Config.insert_selnum

events_around_selnum :: (Monad m) => Types.SelNum
    -> Cmd.CmdT m [(TrackId, [Track.PosEvent], Maybe Track.PosEvent)]
events_around_selnum selnum = do
    (_, track_ids, start, end) <- tracks_selnum selnum
    forM track_ids $ \track_id -> do
        track <- State.get_track track_id
        let (_, evts) = Track.events_at_before start (Track.track_events track)
        let (within, after) =
                break ((if start >= end then (>start) else (>=end)) . fst) evts
        return (track_id, within, Seq.mhead Nothing Just after)

events_in_range, event_before :: TrackPos -> TrackPos -> Track.Track
    -> [Track.PosEvent]
events_in_range start end track =
    Track.events_in_range start end (Track.track_events track)
event_before start _ track = maybe [] (:[]) $
    Track.event_before start (Track.track_events track)

-- | Get selected event tracks along with the selection.  The tracks are
-- returned in ascending order.  Only event tracks are returned.
tracks :: (Monad m) => Cmd.CmdT m ([TrackNum], [TrackId], TrackPos, TrackPos)
tracks = tracks_selnum Config.insert_selnum

tracks_selnum :: (Monad m) =>
    Types.SelNum -> Cmd.CmdT m ([TrackNum], [TrackId], TrackPos, TrackPos)
tracks_selnum selnum = do
    (view_id, sel) <- get_selection selnum
    block_id <- State.block_id_of_view view_id
    tracklikes <- mapM (State.track_at block_id) (Types.sel_tracknums sel)
    let (tracknums, track_ids) = unzip
            [(i, track_id) | (i, Just (Block.TId track_id _))
                <- zip (Types.sel_tracknums sel) tracklikes]
    let (start, end) = Types.sel_range sel
    return (tracknums, track_ids, start, end)

-- | Get the requested selnum in the focused view.
get_selection :: (Monad m) =>
    Types.SelNum -> Cmd.CmdT m (ViewId, Types.Selection)
get_selection selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum
    return (view_id, sel)
