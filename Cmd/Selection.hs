{- | Commands dealing with selection and cursor movement.
-}
module Cmd.Selection where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

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
    | shift == 0 = tracknum
    | shift > 0 = find_track (dropWhile (<tracknum) selectable)
    | otherwise = find_track (dropWhile (>tracknum) (List.reverse selectable))
    where
    selectable = selectable_tracks block
    find_track [] = tracknum
    find_track tracks@(first:_) = Seq.mhead tracknum id $ drop abs_shift tracks
        where abs_shift = if tracknum /= first then abs shift - 1 else abs shift

-- | Get the tracknums from a block that should be selectable.
selectable_tracks :: Block.Block -> [TrackNum]
selectable_tracks block = do
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

-- | Figure out how much to scroll to keep the selection visible and with
-- reasonable space around it.
--
-- Anyone who wants to set a selection and automatically scroll the window to
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
    zoom_offset <- auto_time_scroll view sel0 sel1
    track_offset <- auto_track_scroll view sel0 sel1

    State.set_zoom view_id $
        (Block.view_zoom view) { Types.zoom_offset = zoom_offset }
    State.set_track_scroll view_id track_offset
    Cmd.sync_zoom_status view_id

-- TODO this scrolls too fast when dragging.  Detect a drag and scroll at
-- a rate determined by how far past the bottom the pointer is.
auto_time_scroll :: (Monad m) => Block.View -> Types.Selection
    -> Types.Selection -> Cmd.CmdT m TrackPos
auto_time_scroll view sel0 sel1 = do
    block_id <- Cmd.get_focused_block
    step <- Cmd.get_current_step
    let steps = if Types.sel_cur_pos sel1 >= Types.sel_cur_pos sel0
            then steps_visible else -steps_visible
    next <- TimeStep.step_n steps step block_id
        (Types.sel_cur_track sel1) (Types.sel_cur_pos sel1)
    return $ get_time_offset max_visible view (Types.sel_cur_pos sel1) next
    where
    -- Try to keep this many timesteps in the scroll direction in view.
    steps_visible = 3
    -- Never scroll so much there isn't at least this percent of visible area
    -- in the anti-scroll direction.
    max_visible = 0.2

get_time_offset :: TrackPos -> Block.View -> TrackPos -> TrackPos -> TrackPos
get_time_offset max_visible view sel_pos scroll_to
    | scroll_to >= sel_pos = if scroll_to <= view_end then view_start
        else min (sel_pos - visible * max_visible) (scroll_to - visible)
    | otherwise = if scroll_to >= view_start then view_start
        else max (sel_pos - visible * (1-max_visible)) scroll_to
    where
    visible = Block.visible_time view
    view_start = Types.zoom_offset (Block.view_zoom view)
    view_end = view_start + visible

auto_track_scroll :: (Monad m) => Block.View -> Types.Selection
    -> Types.Selection -> Cmd.CmdT m Types.Width
auto_track_scroll view sel0 sel1 = do
    return $ get_track_offset
        view (Types.sel_cur_track sel0) (Types.sel_cur_track sel1)

get_track_offset :: Block.View -> TrackNum -> TrackNum -> Types.Width
get_track_offset view prev_tracknum cur_tracknum
    | cur_tracknum >= prev_tracknum = max view_start (track_end - visible)
    | otherwise = min view_start track_start
    where
    -- Pesky ruler track doesn't count towards the track scroll.
    widths = map Block.track_view_width (drop 1 (Block.view_tracks view))
    track_start = sum (take (cur_tracknum-1) widths)
    track_end = sum (take cur_tracknum widths)
    view_start = Block.view_track_scroll view
    visible = Block.view_visible_track view


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
    (view_id, sel) <- get
    block_id <- State.block_id_of_view view_id
    return (block_id, Types.sel_start_track sel, Types.sel_start_pos sel)


-- | Get the start and end of the selection, along with the events that fall
-- within it, by track.
events :: (Monad m) =>
    Bool -- ^ If true, a point selection will get the event on or before it.
    -> Cmd.CmdT m [(TrackId, (TrackPos, TrackPos), [Track.PosEvent])]
events = events_selnum Config.insert_selnum

events_selnum :: (Monad m) => Types.SelNum
    -> Bool -- ^ If true, a point selection will get the event on or before it.
    -> Cmd.CmdT m [(TrackId, (TrackPos, TrackPos), [Track.PosEvent])]
    -- ^ (track_id, (sel_start, sel_end), events_around)
events_selnum selnum point_prev = do
    (_, track_ids, start, end) <- tracks_selnum selnum
    tracks <- mapM State.get_track track_ids
    let prev = point_prev && start == end
    let get_events = if prev then event_before else events_in_range
        get_range events = case events of
            [e@(pos, _)] | prev -> (pos, Track.event_end e)
            _ -> (start, end)
    return $ do
        (track_id, track) <- zip track_ids tracks
        let events = get_events start end track
        return $ (track_id, get_range events, events)
    where
    events_in_range start end track =
        Track.events_in_range start end (Track.track_events track)
    event_before start _ track = maybe [] (:[]) $
        Track.event_before start (Track.track_events track)

-- | A variant of 'selected_events'.  If the selection is a point, the
-- \"within\" list will contain the event at or before the selection.
events_around :: (Monad m) =>
    Cmd.CmdT m [(TrackId, [Track.PosEvent], [Track.PosEvent], [Track.PosEvent])]
events_around = events_around_selnum Config.insert_selnum

events_around_selnum :: (Monad m) => Types.SelNum
    -> Cmd.CmdT m [(TrackId,
        [Track.PosEvent], [Track.PosEvent], [Track.PosEvent])]
    -- ^ (track_id, before, within, after)
events_around_selnum selnum = do
    (_, track_ids, start, end) <- tracks_selnum selnum
    (_, sel) <- get_selnum selnum
    forM track_ids $ \track_id -> do
        track <- State.get_track track_id
        let split = if Types.sel_is_point sel then Track.split_at_before
                else Track.split
            (before, rest) = split start (Track.track_events track)
            (within, after) = break
                ((if start==end then (>end) else (>=end))  . fst) rest
        return (track_id, before, within, after)

-- | Get selected event tracks along with the selection.  The tracks are
-- returned in ascending order.  Only event tracks are returned.
tracks :: (Monad m) => Cmd.CmdT m ([TrackNum], [TrackId], TrackPos, TrackPos)
tracks = tracks_selnum Config.insert_selnum

tracks_selnum :: (Monad m) =>
    Types.SelNum -> Cmd.CmdT m ([TrackNum], [TrackId], TrackPos, TrackPos)
tracks_selnum selnum = do
    (view_id, sel) <- get_selnum selnum
    block_id <- State.block_id_of_view view_id
    tracklikes <- mapM (State.track_at block_id) (Types.sel_tracknums sel)
    let (tracknums, track_ids) = unzip
            [(i, track_id) | (i, Just (Block.TId track_id _))
                <- zip (Types.sel_tracknums sel) tracklikes]
    let (start, end) = Types.sel_range sel
    return (tracknums, track_ids, start, end)

-- | This is like 'tracks' except it also includes tracks merged into the
-- selected tracks.
merged_tracks :: (Monad m) =>
    Cmd.CmdT m ([TrackNum], [TrackId], TrackPos, TrackPos)
merged_tracks = merged_tracks_selnum Config.insert_selnum

merged_tracks_selnum :: (Monad m) => Types.SelNum
    -> Cmd.CmdT m ([TrackNum], [TrackId], TrackPos, TrackPos)
merged_tracks_selnum selnum = do
    (tracknums, track_ids, start, end) <- tracks_selnum selnum
    block_id <- Cmd.get_focused_block
    tracks <- mapM (State.get_block_track block_id) tracknums
    let merged_track_ids = concatMap Block.track_merged tracks
    block <- State.get_block block_id
    let merged = tracknums_of block merged_track_ids
    let (all_tracknums, all_track_ids) = unzip $ List.sort $ List.nub $
            merged ++ zip tracknums track_ids
    return (all_tracknums, all_track_ids, start, end)

tracknums_of :: Block.Block -> [TrackId] -> [(TrackNum, TrackId)]
tracknums_of block track_ids = do
    (tracknum, Block.TId tid _) <-
        zip [0..] (Block.block_tracklike_ids block)
    guard (tid `elem` track_ids)
    return (tracknum, tid)

-- | Get the requested selnum in the focused view.
get_selnum :: (Monad m) => Types.SelNum -> Cmd.CmdT m (ViewId, Types.Selection)
get_selnum selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum
    return (view_id, sel)

get :: (Monad m) => Cmd.CmdT m (ViewId, Types.Selection)
get = get_selnum Config.insert_selnum

is_point :: (Monad m) => Cmd.CmdT m Bool
is_point = is_point_selnum Config.insert_selnum

is_point_selnum :: (Monad m) => Types.SelNum -> Cmd.CmdT m Bool
is_point_selnum selnum = do
    (_, sel) <- get_selnum selnum
    return (Types.sel_is_point sel)
