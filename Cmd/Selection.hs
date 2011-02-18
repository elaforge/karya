{- | Commands dealing with selection and cursor movement.

    As is typical, when it comes to selecting events, the selection represents
    a half-open range.  However, reflecting the orientation of events,
    a negative event at the start of the range won't be included, and
    a negative event at he end of the range will be included.  This is natural
    for events with negative duration, since they are weighted at the end.

    This behaviour is actually implemented in the low level "Ui.Track"
    functions.
-}
module Cmd.Selection where
import Prelude hiding (lookup)
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Score as Score

import qualified Perform.Transport as Transport

import qualified App.Config as Config


-- * cmds

-- | Advance the given selection by the current step.
-- Require: active block, insert_selection is set
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.  If @extend@ is true, extend the current
-- selection instead of setting a new point selection.
cmd_step_selection :: (Cmd.M m) => Types.SelNum -> TimeStep.Direction
    -> Bool -> m ()
cmd_step_selection selnum dir extend = do
    view_id <- Cmd.get_focused_view
    Types.Selection start_track start_pos cur_track cur_pos <-
        Cmd.require =<< State.get_selection view_id selnum

    step <- Cmd.get_current_step
    new_pos <- step_from cur_track cur_pos dir step
    let new_sel = if extend
            then Types.selection start_track start_pos cur_track new_pos
            else Types.point_selection start_track new_pos
    select_and_scroll view_id selnum new_sel

-- | Advance the insert selection by the current step, which is a popular thing
-- to do.
cmd_advance_insert :: (Cmd.M m) => m ()
cmd_advance_insert =
    cmd_step_selection Config.insert_selnum TimeStep.Advance False

-- | Move the selection across tracks by @shift@, skipping non-event tracks
-- and collapsed tracks.
--
-- If @extend@ is true, extend the current selection instead of setting a new
-- selection.
cmd_shift_selection :: (Cmd.M m) => Types.SelNum -> TrackNum -> Bool -> m ()
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
    find_track tracks@(first:_) =
        maybe tracknum id $ Seq.head $ drop abs_shift tracks
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
cmd_mouse_selection :: (Cmd.M m) =>
    Int -> Types.SelNum -> Bool -> Msg.Msg -> m ()
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
cmd_snap_selection :: (Cmd.M m) => Int -> Types.SelNum -> Bool
    -> Msg.Msg -> m ()
cmd_snap_selection btn selnum extend msg = do
    (down_tracknum, _, mouse_tracknum, mouse_pos) <- mouse_drag btn msg
    block_id <- Cmd.get_focused_block
    step <- Cmd.get_current_step
    view_id <- Cmd.get_focused_view
    old_sel <- State.get_selection view_id selnum
    snap_pos <- TimeStep.snap step block_id mouse_tracknum
        (fmap Types.sel_cur_pos old_sel) mouse_pos
    let sel = case old_sel of
            _ | Msg.mouse_down msg && not extend || old_sel == Nothing ->
                Types.selection down_tracknum snap_pos mouse_tracknum snap_pos
            Just (Types.Selection tracknum pos _ _) ->
                Types.selection tracknum pos mouse_tracknum snap_pos
            -- ghc doesn't realize it is exhaustive
            _ -> error "Cmd.Selection: not reached"
    select_and_scroll view_id selnum sel

-- | Get the dragged range, or abort if this isn't a drag Msg.
mouse_drag :: (Cmd.M m) => Int -> Msg.Msg
    -> m (TrackNum, ScoreTime, TrackNum, ScoreTime)
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
advance :: (Cmd.M m) => m ()
advance = cmd_step_selection Config.insert_selnum TimeStep.Advance False

select :: (Cmd.M m) => ViewId -> Types.SelNum -> Maybe Types.Selection -> m ()
select view_id selnum sel = do
    State.set_selection view_id selnum sel
    sync_selection_status view_id

-- ** auto scroll

-- | Figure out how much to scroll to keep the selection visible and with
-- reasonable space around it.
--
-- Anyone who wants to set a selection and automatically scroll the window to
-- follow the selection should use this function.
select_and_scroll :: (Cmd.M m) =>
     ViewId -> Types.SelNum -> Maybe Types.Selection -> m ()
select_and_scroll view_id selnum sel = do
    State.set_selection view_id selnum sel
    sync_selection_status view_id
    when_just sel (auto_scroll view_id)

-- | If @new@ has scrolled off the edge of the window, automatically scroll
-- it so that the selection is in view.
auto_scroll :: (Cmd.M m) => ViewId -> Types.Selection -> m ()
auto_scroll view_id sel = do
    view <- State.get_view view_id
    let zoom_offset = auto_time_scroll view sel
        track_offset = auto_track_scroll view sel
    State.set_zoom view_id $
        (Block.view_zoom view) { Types.zoom_offset = zoom_offset }
    State.set_track_scroll view_id track_offset
    Cmd.sync_zoom_status view_id

-- TODO this scrolls too fast when dragging.  Detect a drag and scroll at
-- a rate determined by how far past the bottom the pointer is.
auto_time_scroll :: Block.View -> Types.Selection -> ScoreTime
auto_time_scroll view sel
    | scroll_to >= view_end = scroll_to - visible + space
    | scroll_to < view_start = scroll_to - space
    | otherwise = view_start
    where
    visible = Block.visible_time view
    view_start = Types.zoom_offset (Block.view_zoom view)
    view_end = view_start + visible
    scroll_to = Types.sel_cur_pos sel
    space = ScoreTime
        (visible_pixels / Types.zoom_factor (Block.view_zoom view))
    visible_pixels = 30

auto_track_scroll :: Block.View -> Types.Selection -> Types.Width
auto_track_scroll view sel
    | track_end > view_end = track_end - visible
    | track_start < view_start = track_start
    | otherwise = view_start
    where
    -- Pesky ruler track doesn't count towards the track scroll.
    widths = map Block.track_view_width (drop 1 (Block.view_tracks view))
    track_start = sum (take (cur_tracknum-1) widths)
    track_end = sum (take cur_tracknum widths)
    view_start = Block.view_track_scroll view
    view_end = view_start + visible
    visible = Block.view_visible_track view
    cur_tracknum = Types.sel_cur_track sel


-- ** status

sync_selection_status :: (Cmd.M m) => ViewId -> m ()
sync_selection_status view_id = do
    maybe_sel <- State.get_selection view_id Config.insert_selnum
    Cmd.set_view_status view_id "sel" (fmap selection_status maybe_sel)
    block_id <- State.block_id_of_view view_id
    when_just maybe_sel $
        Info.set_inst_status block_id . Types.sel_cur_track

selection_status :: Types.Selection -> String
selection_status sel = Pretty.show_float (Just 3) start
    ++ if start == end then "" else "-" ++ Pretty.show_float (Just 3) end
    where (start, end) = Types.sel_range sel

-- ** mouse

mouse_mod :: Msg.Msg -> Maybe (Cmd.Modifier, (TrackNum, ScoreTime))
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

step_from :: (Cmd.M m) => TrackNum -> ScoreTime -> TimeStep.Direction
    -> TimeStep.TimeStep -> m ScoreTime
step_from tracknum pos direction step = do
    block_id <- Cmd.get_focused_block
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

-- * get selection info

{- Getting the selection may seem pretty simple, but there are a number of
    orthogonal flavors:

    - Return a raw Types.Selection, or return its
    (ViewId, BlockId, TrackId, ScoreTime) context.

    - Get a single point from a selection, or a range on a single track, or
    a range of tracks.

    - Get an arbitrary Types.SelNum or use the Config.insert_selnum.

    - Return a Maybe or abort on Nothing.

    - Return for any track, or return a TrackId and abort if it's not an event
    track.

    - Used an arbitrary ViewId, BlockId, or use the focused view.

    And then there is a whole other dimension of converting selections, which
    are in ScoreTime, to RealTime.  The selection can be converted either
    relative to its block's tempo, or relative to a calling block's tempo,
    namely the root block.

    Also, when a selection is interpreted as a point (for instance, for
    operations like \"play from selection\"), there is a choice of taking the
    point from the beginning of the selection, the end, or the 'sel_cur_pos',
    which is the dragged-to point.  The convention, established by
    'point_pos' and 'point_track', is to take the first point.
-}

point_pos :: Types.Selection -> ScoreTime
point_pos sel = min (Types.sel_start_pos sel) (Types.sel_cur_pos sel)

point_track :: Types.Selection -> TrackNum
point_track sel = min (Types.sel_start_track sel) (Types.sel_cur_track sel)

-- | A point on a track.
type Point = (BlockId, TrackNum, TrackId, ScoreTime)
type AnyPoint = (BlockId, TrackNum, ScoreTime)

-- | Get the "insert position", which is the start track and position of the
-- insert selection.  Abort if it's not an event track.
get_insert :: (Cmd.M m) => m Point
get_insert = Cmd.require =<< lookup_insert

lookup_insert :: (Cmd.M m) => m (Maybe Point)
lookup_insert = fmap (fmap snd) $ lookup_selnum_insert Config.insert_selnum

-- | Return the leftmost tracknum and trackpos, even if it's not an event
-- track.
get_any_insert :: (Cmd.M m) => m (ViewId, AnyPoint)
get_any_insert = Cmd.require =<< lookup_any_selnum_insert Config.insert_selnum

lookup_selnum_insert :: (Cmd.M m) => Types.SelNum -> m (Maybe (ViewId, Point))
lookup_selnum_insert selnum =
    justm (lookup_any_selnum_insert selnum) $
    \(view_id, (block_id, tracknum, pos)) ->
    justm (State.event_track_at block_id tracknum) $ \track_id ->
    return $ Just (view_id, (block_id, tracknum, track_id, pos))

-- | The most general insertion point function.
lookup_any_selnum_insert :: (Cmd.M m) => Types.SelNum
    -> m (Maybe (ViewId, AnyPoint))
lookup_any_selnum_insert selnum =
    justm (lookup_selnum selnum) $ \(view_id, sel) -> do
        block_id <- State.block_id_of_view view_id
        return $ Just (view_id, (block_id, point_track sel, point_pos sel))

-- | Given a block, get the selection on it, if any.  If there are multiple
-- views, take the one with the alphabetically first ViewId.
--
-- I'm not sure how to choose, but the first one seems reasonable for now.
lookup_block_insert :: (State.M m) => BlockId -> m (Maybe Point)
lookup_block_insert block_id = do
    view_ids <- Map.keys <$> State.get_views_of block_id
    case view_ids of
        [] -> return Nothing
        view_id : _ ->
            justm (State.get_selection view_id Config.insert_selnum) $ \sel ->
            justm (sel_track block_id sel) $ \track_id ->
            return $ Just (block_id, point_track sel, track_id, point_pos sel)

-- | Get the point track of a selection.
sel_track :: (State.M m) => BlockId -> Types.Selection -> m (Maybe TrackId)
sel_track block_id sel = State.event_track_at block_id (point_track sel)

-- ** plain Selection

-- | Get the insertion selection in the focused view.
get :: (Cmd.M m) => m (ViewId, Types.Selection)
get = get_selnum Config.insert_selnum

-- | Get the requested selnum in the focused view.
get_selnum :: (Cmd.M m) => Types.SelNum -> m (ViewId, Types.Selection)
get_selnum selnum = Cmd.require =<< lookup_selnum selnum

lookup :: (Cmd.M m) => m (Maybe (ViewId, Types.Selection))
lookup = lookup_selnum Config.insert_selnum

lookup_selnum :: (Cmd.M m) => Types.SelNum
    -> m (Maybe (ViewId, Types.Selection))
lookup_selnum selnum =
    justm Cmd.lookup_focused_view $ \view_id ->
    justm (State.get_selection view_id selnum) $ \sel ->
    return $ Just (view_id, sel)

-- | This is sort of like a monad transformer, but the Maybe is on the inside
-- instead of the outside.
--
-- What I really want here is MaybeT, but it requres explicit lifting...
justm :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
justm op1 op2 = maybe (return Nothing) op2 =<< op1

-- ** selections in RealTime

-- TODO too much hardcoded use of the focused selection means this might not
-- be flexible enough.  Fix it if necessary.  Why are selections such a pain?

-- | Get the real time range of the focused selection.  If there's a root
-- block, then it will be in relative to that root, otherwise it's equivalent
-- to 'local_realtime'.
realtime :: (Cmd.M m) => m (RealTime, RealTime)
realtime = do
    maybe_root_id <- State.lookup_root_id
    case maybe_root_id of
        Nothing -> local_realtime
        Just root_id -> relative_realtime root_id

-- | This is like 'get_insert', except get the selection on the root block,
-- falling back to the current one if there is none.
get_root_insert :: (Cmd.M m) => m Point
get_root_insert = maybe get_insert return =<< rootsel
    where
    rootsel = justm State.lookup_root_id $ \root_id ->
        lookup_block_insert root_id

-- | Get the current selection in RealTime relative to another block.
--
-- If a block is called in multiple places, a score time on it may occur at
-- multiple real times.  Pick the real time from the given selection which is
-- closest to the real time of the selection on the given root block.
--
-- If there's no selection on the root block then return the RealTime from the
-- block's first occurrance.
relative_realtime :: (Cmd.M m) => BlockId -> m (RealTime, RealTime)
relative_realtime root_id = do
    (view_id, sel) <- get
    block_id <- State.block_id_of_view view_id
    track_id <- Cmd.require =<< sel_track block_id sel
    maybe_root_sel <- lookup_block_insert root_id
    perf <- Cmd.get_performance root_id
    let root_pos = point_to_real (Cmd.perf_tempo perf) maybe_root_sel
    let warp = Cmd.perf_closest_warp perf block_id track_id root_pos
    let (start, end) = Types.sel_range sel
    return (Score.warp_pos start warp, Score.warp_pos end warp)

-- | Get the RealTime range of the current selection, as derived from current
-- selection's block.  This means that the top should be 0.
local_realtime :: (Cmd.M m) => m (RealTime, RealTime)
local_realtime = do
    (view_id, sel) <- get
    block_id <- State.block_id_of_view view_id
    track_id <- Cmd.require =<< sel_track block_id sel
    perf <- Cmd.get_performance block_id
    let (start, end) = Types.sel_range sel
    let warp = Cmd.perf_closest_warp perf block_id track_id 0
    return (Score.warp_pos start warp, Score.warp_pos end warp)

-- | This is like 'relative_realtime' but gets a RealTime relative to a Point,
-- not a range.
relative_realtime_point :: Cmd.Performance -> Maybe Point -> Point -> RealTime
relative_realtime_point perf maybe_root_sel (block_id, _, track_id, pos) =
    Score.warp_pos pos warp
    where
    root_pos = point_to_real (Cmd.perf_tempo perf) maybe_root_sel
    warp = Cmd.perf_closest_warp perf block_id track_id root_pos

point_to_real :: Transport.TempoFunction -> Maybe Point -> RealTime
point_to_real _ Nothing = 0
point_to_real tempo (Just (block_id, _, track_id, pos)) =
    maybe 0 id $ Seq.head $ tempo block_id track_id pos

-- ** select events

-- | Selected events per track.  Gives events previous to, within, and after
-- the selection.  As usual, previous events are in descending order.  The
-- event range is also returned, which may not be the same as the selection
-- range because these functions may select more events than lie strictly
-- within the selection.
type SelectedAround = [(TrackId, (ScoreTime, ScoreTime),
    ([Track.PosEvent], [Track.PosEvent], [Track.PosEvent]))]
type SelectedEvents = [(TrackId, (ScoreTime, ScoreTime), [Track.PosEvent])]

-- | 'events_around' is the default selection behaviour.
events :: (Cmd.M m) => m SelectedEvents
events = fmap extract_events events_around

events_around :: (Cmd.M m) => m SelectedAround
events_around = events_around_selnum Config.insert_selnum

-- | Select events whose @pos@ lie strictly within the selection range.
strict_events_around :: (Cmd.M m) => Types.SelNum -> m SelectedAround
strict_events_around selnum = do
    (_, track_ids, start, end) <- tracks_selnum selnum
    tracks <- mapM State.get_track track_ids
    return [(track_id, (start, end),
        Track.split_range start end (Track.track_events track))
            | (track_id, track) <- zip track_ids tracks]

-- | Get events in the selection, but if no events are selected, expand it
-- to include a previous positive event or a following negative one.  If both
-- are present, the positive event is favored.  If neither are present, select
-- nothing.
--
-- This is the standard definition of a selection, and should be used in all
-- standard selection using commands.
events_around_selnum :: (Cmd.M m) => Types.SelNum -> m SelectedAround
events_around_selnum selnum = do
    selected <- strict_events_around selnum
    return $ do
        (track_id, range, evts) <- selected
        let evts2 = expand evts
        let range2 = expand_range evts2 range
        return (track_id, range2, evts2)
    where
    expand (before, [], after)
        | take_prev = (drop 1 before, take 1 before, after)
        | take_next = (before, take 1 after, drop 1 after)
        | otherwise = (before, [], after)
        where
        take_prev = maybe False Track.event_positive (Seq.head before)
        take_next = maybe False Track.event_negative (Seq.head after)
    expand selected = selected
    expand_range (_, [evt], _) _ = (Track.event_min evt, Track.event_max evt)
    expand_range _ range = range

extract_events :: SelectedAround -> SelectedEvents
extract_events = map $ \(track_id, range, (_, within, _)) ->
    (track_id, range, within)

-- ** select tracks

-- | Get selected event tracks along with the selection.  The tracks are
-- returned in ascending order.  Only event tracks are returned, and tracks
-- merged into the selected tracks are included.
tracks :: (Cmd.M m) => m ([TrackNum], [TrackId], ScoreTime, ScoreTime)
tracks = tracks_selnum Config.insert_selnum

-- | Selected tracks, including merged tracks.
tracks_selnum :: (Cmd.M m) => Types.SelNum
    -> m ([TrackNum], [TrackId], ScoreTime, ScoreTime)
tracks_selnum selnum = do
    (tracknums, track_ids, start, end) <- strict_tracks_selnum selnum
    block_id <- Cmd.get_focused_block
    tracks <- mapM (State.get_block_track block_id) tracknums
    let merged_track_ids = concatMap Block.track_merged tracks
    block <- State.get_block block_id
    let merged = tracknums_of block merged_track_ids
    let (all_tracknums, all_track_ids) = unzip $ List.sort $ List.nub $
            merged ++ zip tracknums track_ids
    return (all_tracknums, all_track_ids, start, end)

-- | Selected tracks, not including merged tracks.
strict_tracks_selnum :: (Cmd.M m) =>
    Types.SelNum -> m ([TrackNum], [TrackId], ScoreTime, ScoreTime)
strict_tracks_selnum selnum = do
    (view_id, sel) <- get_selnum selnum
    block_id <- State.block_id_of_view view_id
    tracklikes <- mapM (State.track_at block_id) (Types.sel_tracknums sel)
    let (tracknums, track_ids) = unzip
            [(i, track_id) | (i, Just (Block.TId track_id _))
                <- zip (Types.sel_tracknums sel) tracklikes]
    let (start, end) = Types.sel_range sel
    return (tracknums, track_ids, start, end)

tracknums_of :: Block.Block -> [TrackId] -> [(TrackNum, TrackId)]
tracknums_of block track_ids = do
    (tracknum, Block.TId tid _) <-
        zip [0..] (Block.block_tracklike_ids block)
    guard (tid `elem` track_ids)
    return (tracknum, tid)
