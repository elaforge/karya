-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Midi.Mmc as Mmc
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Score as Score
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Types


-- * cmds

-- | Set the given selection.
--
-- This is the Cmd level of State.set_selection and should be called by
-- any Cmd that wants to set the selection.
set :: Cmd.M m => ViewId -> Maybe Types.Selection -> m ()
set view_id = set_selnum view_id Config.insert_selnum

set_selnum :: Cmd.M m => ViewId -> Types.SelNum -> Maybe Types.Selection
    -> m ()
set_selnum view_id selnum maybe_sel = do
    State.set_selection view_id selnum maybe_sel
    when (selnum == Config.insert_selnum) $ case maybe_sel of
        Just sel | Types.sel_is_point sel -> do
            set_subs view_id sel
            whenJustM (Cmd.gets (Cmd.state_sync . Cmd.state_play)) $
                mmc_goto_sel view_id sel
        _ -> return ()

mmc_goto_sel :: Cmd.M m => ViewId -> Types.Selection -> Cmd.SyncConfig -> m ()
mmc_goto_sel view_id sel sync = do
    block_id <- State.block_id_of view_id
    maybe_track_id <- State.event_track_at block_id (Types.sel_cur_track sel)
    whenJustM (root_realtime block_id maybe_track_id (Types.sel_cur_pos sel)) $
        Cmd.midi (Cmd.sync_device sync) . mmc_goto sync

mmc_goto :: Cmd.SyncConfig -> RealTime -> Midi.Message
mmc_goto sync pos = Mmc.encode (Cmd.sync_device_id sync) $
    Mmc.goto_seconds (Cmd.sync_frame_rate sync) (RealTime.to_seconds pos)

-- | Set a selection in the current view.
set_current :: Cmd.M m => Types.SelNum -> Maybe Types.Selection -> m ()
set_current selnum maybe_sel = do
    view_id <- Cmd.get_focused_view
    set_selnum view_id selnum maybe_sel

-- | For point selections, set a play position selection on the equivalent
-- time in sub-blocks.  This makes it easier to edit the super-block relative
-- to the sub-block.
--
-- TODO if multiple calls overlap, I should draw multiple selections, but
-- State.set_selection doesn't support that, even though the underlying
-- BlockC.set_selection does.
set_subs :: Cmd.M m => ViewId -> Types.Selection -> m ()
set_subs view_id sel = do
    view_ids <- State.all_view_ids
    forM_ view_ids $ \vid ->
        State.set_selection vid Config.play_position_selnum Nothing
    block_id <- State.block_id_of view_id
    maybe_track_id <- State.event_track_at block_id (Types.sel_cur_track sel)
    whenJust maybe_track_id $ \track_id ->
        mapM_ (uncurry set_block) =<<
            Perf.sub_pos block_id track_id (Types.sel_cur_pos sel)

set_block :: State.M m => BlockId -> [(TrackId, TrackTime)] -> m ()
set_block _ [] = return ()
set_block block_id ((_, pos) : _) = do
    view_ids <- Map.keys <$> State.views_of block_id
    forM_ view_ids $ \view_id ->
        State.set_selection view_id Config.play_position_selnum
            (Just (Types.selection 0 pos 999 pos))

-- | Figure out how much to scroll to keep the selection visible and with
-- reasonable space around it.
--
-- Anyone who wants to set a selection and automatically scroll the window to
-- follow the selection should use this function.
set_and_scroll :: Cmd.M m => ViewId -> Types.SelNum -> Types.Selection -> m ()
set_and_scroll view_id selnum sel = do
    old <- State.get_selection view_id selnum
    set_selnum view_id selnum (Just sel)
    auto_scroll view_id old sel

-- ** modify existing selection

-- | Advance the insert selection by the current step, which is a popular thing
-- to do.
advance :: Cmd.M m => m ()
advance = step TimeStep.Advance False

-- | Advance the given selection by the current step.
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.  If @extend@ is true, extend the current
-- selection instead of setting a new point selection.
step :: Cmd.M m => TimeStep.Direction -> Bool -> m ()
step dir extend = do
    st <- Cmd.get_current_step
    step_with (TimeStep.direction dir) extend st

step_with :: Cmd.M m => Int -> Bool -> TimeStep.TimeStep -> m ()
step_with steps extend step = do
    view_id <- Cmd.get_focused_view
    Types.Selection start_track start_pos cur_track cur_pos <-
        Cmd.abort_unless =<< State.get_selection view_id Config.insert_selnum
    new_pos <- step_from cur_track cur_pos steps step
    let new_sel = if extend
            then Types.selection start_track start_pos cur_track new_pos
            else Types.point_selection cur_track new_pos
    set_and_scroll view_id Config.insert_selnum new_sel

-- | Move the selection across tracks by @shift@, skipping non-event tracks
-- and collapsed tracks.
--
-- If @extend@ is true, extend the current selection instead of setting a new
-- selection.
shift :: Cmd.M m => Bool -> TrackNum -> m ()
shift extend shift = do
    view_id <- Cmd.get_focused_view
    block <- State.block_of view_id
    sel <- Cmd.abort_unless =<< State.get_selection view_id Config.insert_selnum
    let new_sel = State.shift_selection block shift sel
    set_and_scroll view_id Config.insert_selnum
        (if extend then merge_sel sel new_sel else new_sel)

-- | Shift a selection right or left.
data Shift = R | L deriving (Show)

-- | Find the first track before or after the current one whose title matches
-- a predicate.
find_track :: Cmd.M m => Shift -> (Text -> Bool) -> m TrackNum
find_track shift stop = do
    (view_id, sel) <- get
    block_id <- State.block_id_of view_id
    tracks <- TrackTree.tracks_of block_id
    let maybe_next = Seq.head $ dropWhile (not . stop . State.track_title)
            (order sel tracks)
    return $ case maybe_next of
        Nothing -> 0
        Just next -> State.track_tracknum next - Types.sel_cur_track sel
    where
    order sel = case shift of
        R -> dropWhile ((<= Types.sel_cur_track sel) . State.track_tracknum)
        L -> dropWhile ((>= Types.sel_cur_track sel) . State.track_tracknum)
            . reverse

-- | Progressive selection: select the rest of the track, then the entire
-- track, then the whole block.
--
-- TrackNum 0, assumed to be a ruler, is omitted, since selecting the ruler is
-- not only not useful, it tends to make cmds that want to get a TrackId abort.
cmd_track_all :: Cmd.M m => Types.SelNum -> m ()
cmd_track_all selnum = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.abort_unless =<< State.get_selection view_id selnum
    block_id <- State.block_id_of view_id
    block_end <- State.block_end block_id
    tracks <- length . Block.block_tracks <$> State.get_block block_id
    set_selnum view_id selnum (Just (select_track_all block_end tracks sel))

select_track_all :: TrackTime -> TrackNum -> Types.Selection -> Types.Selection
select_track_all block_end_ tracks sel
    | sel == select_tracks = select_all
    | sel == select_rest = select_tracks
    | otherwise = select_rest
    where
    -- Keep sel_cur_pos at the current position, or set it to the top.
    -- This is so 'auto_scroll' won't jump to the bottom of the block.
    select_rest =
        sel { Types.sel_cur_pos = start, Types.sel_start_pos = block_end }
        where start = fst $ Types.sel_range sel
    select_tracks =
        sel { Types.sel_cur_pos = 0, Types.sel_start_pos = block_end }
    select_all = Types.selection 1 0 tracks block_end
    -- Otherwise a select-all won't include an event at the end of the block.
    block_end = block_end_ + ScoreTime.eta

merge_sel :: Types.Selection -> Types.Selection -> Types.Selection
merge_sel (Types.Selection strack spos _ _) (Types.Selection _ _ ctrack cpos) =
    Types.Selection strack spos ctrack cpos

-- ** set selection from clicks

-- | Select clicked on track.
cmd_select_track :: Cmd.M m => Types.MouseButton -> Types.SelNum -> Msg.Msg
    -> m ()
cmd_select_track btn selnum msg = do
    view_id <- Cmd.get_focused_view
    ((down_tracknum, _), (mouse_tracknum, _)) <- mouse_drag btn msg
    select_tracks selnum view_id down_tracknum mouse_tracknum

select_tracks :: Cmd.M m => Types.SelNum -> ViewId -> TrackNum -> TrackNum
    -> m ()
select_tracks selnum view_id from to = do
    dur <- State.block_event_end =<< State.block_id_of view_id
    set_selnum view_id selnum $ Just (Types.selection from dur to 0)

-- | Set the selection based on a click or drag.
cmd_mouse_selection :: Cmd.M m =>
    Types.MouseButton -> Types.SelNum -> Bool -> Msg.Msg -> m ()
cmd_mouse_selection btn selnum extend msg = do
    ((down_tracknum, down_pos), (mouse_tracknum, mouse_pos))
        <- mouse_drag_pos btn msg
    view_id <- Cmd.get_focused_view
    old_sel <- State.get_selection view_id selnum
    let (start_tracknum, start_pos) = case (extend, old_sel) of
            (True, Just (Types.Selection tracknum pos _ _)) -> (tracknum, pos)
            _ -> (down_tracknum, down_pos)
    let sel = Types.selection start_tracknum start_pos mouse_tracknum mouse_pos
    set_and_scroll view_id selnum sel

-- | Like 'cmd_mouse_selection', but snap the selection to the current time
-- step.
cmd_snap_selection :: Cmd.M m => Types.MouseButton -> Types.SelNum -> Bool
    -> Msg.Msg -> m ()
cmd_snap_selection btn selnum extend msg = do
    ((down_tracknum, _), (mouse_tracknum, mouse_pos)) <- mouse_drag_pos btn msg
    block_id <- Cmd.get_focused_block
    step <- Cmd.get_current_step
    view_id <- Cmd.get_focused_view
    old_sel <- State.get_selection view_id selnum
    snap_pos <- TimeStep.snap step block_id mouse_tracknum
        (Types.sel_cur_pos <$> old_sel) mouse_pos
    snap_pos <- snap_over_threshold view_id block_id mouse_pos snap_pos
    let sel = case old_sel of
            _ | old_sel == Nothing || Msg.mouse_down msg && not extend ->
                Types.selection down_tracknum snap_pos mouse_tracknum snap_pos
            Just (Types.Selection tracknum pos _ _) ->
                Types.selection tracknum pos mouse_tracknum snap_pos
            -- ghc doesn't realize it is exhaustive
            _ -> error "Cmd.Selection: not reached"
    set_and_scroll view_id selnum sel
    where
    -- If I'm dragging, only snap if I'm close to a snap point.  Otherwise,
    -- it's easy for the selection to jump way off screen while dragging.
    snap_over_threshold view_id block_id pos snap = do
        zoom <- State.get_zoom view_id
        let over = Types.zoom_to_pixels zoom (abs (snap - pos)) > threshold
        -- Don't go past the end of the ruler.  Otherwise it's easy to
        -- accidentally select too much by dragging to the end of the block.
        end <- State.block_ruler_end block_id
        return $ if not (Msg.mouse_down msg) && over && pos < end
            then pos else snap
    threshold = 20

-- | Like 'mouse_drag' but specialized for drags on the track.
mouse_drag_pos :: Cmd.M m => Types.MouseButton -> Msg.Msg
    -> m ((TrackNum, TrackTime), (TrackNum, TrackTime))
mouse_drag_pos btn msg = do
    ((num1, t1), (num2, t2)) <- mouse_drag btn msg
    case (t1, t2) of
        (UiMsg.Track p1, UiMsg.Track p2) -> return ((num1, p1), (num2, p2))
        _ -> Cmd.abort

-- | Get the clicked or dragged range, or abort if this isn't a drag Msg.
-- This accepts clicks as well, and considers them an empty range.
mouse_drag :: Cmd.M m => Types.MouseButton -> Msg.Msg
    -> m ((TrackNum, UiMsg.Track), (TrackNum, UiMsg.Track))
    -- ^ (mouse down at, mouse currently at)
mouse_drag btn msg = do
    (is_down, mod, mouse_at) <- Cmd.abort_unless (mouse_mod msg)
    msg_btn <- Cmd.abort_unless (Cmd.mouse_mod_btn mod)
    -- The button down should be the same one as expected.
    when (msg_btn /= btn) Cmd.abort
    keys_down <- Cmd.keys_down
    let mouse_down = Map.lookup (Cmd.strip_modifier mod) keys_down
    let down_at = case (is_down, mouse_down) of
            (False, Just (Cmd.MouseMod _ (Just track))) -> track
            -- If it's not already held down, it starts here.
            _ -> mouse_at
    -- MsgCollector is back to clamping at track-1, but leave this in in
    -- case I change my mind again.
    -- -- Clip a drag past the last track to the last track, callers here treat
    -- -- it as the same.
    -- tracks <- State.track_count =<< Cmd.get_focused_block
    -- let clamp (tnum, track) = (min (tracks-1) tnum, track)
    -- return (clamp down_at, clamp mouse_at)
    return (down_at, mouse_at)

-- * implementation

-- ** auto scroll

-- | If the selection has scrolled off the edge of the window, automatically
-- scroll it so that the \"current\" end of the selection is in view.
auto_scroll :: Cmd.M m => ViewId -> Maybe Types.Selection -> Types.Selection
    -> m ()
auto_scroll view_id old new = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    let zoom_offset = auto_time_scroll view
            (Types.sel_cur_pos <$> old) (Types.sel_cur_pos new)
        track_offset = auto_track_scroll block view new
    State.set_zoom view_id $
        (Block.view_zoom view) { Types.zoom_offset = zoom_offset }
    State.set_track_scroll view_id track_offset

-- TODO this scrolls too fast when dragging.  Detect a drag and scroll at
-- a rate determined by how far past the bottom the pointer is.
auto_time_scroll :: Block.View -> Maybe TrackTime -> TrackTime -> TrackTime
auto_time_scroll view prev_pos pos
    -- Don't scroll if the cur pos hasn't changed.  Otherwise, selecting the
    -- whole track and shifting tracks scrolls the block down to the bottom.
    | Just pos == prev_pos = view_start
    | pos >= view_end = pos - visible + space
    | pos < view_start = pos - space
    | otherwise = view_start
    where
    visible = Block.visible_time view
    view_start = Types.zoom_offset (Block.view_zoom view)
    view_end = view_start + visible
    space = ScoreTime.double
        (visible_pixels / Types.zoom_factor (Block.view_zoom view))
    visible_pixels = 30

-- | Find the track scroll that would put the given selection into view.
auto_track_scroll :: Block.Block -> Block.View -> Types.Selection
    -> Types.Width
auto_track_scroll block view sel
    | track_end > view_end = track_end - visible
    | track_start < view_start = track_start
    | otherwise = view_start
    where
    -- Pesky ruler track doesn't count towards the track scroll.
    widths = map Block.display_track_width (drop 1 (Block.block_tracks block))
    track_start = sum (take (cur_tracknum-1) widths)
    track_end = sum (take cur_tracknum widths)
    view_start = Block.view_track_scroll view
    view_end = view_start + visible
    -- Visible does include the pesky ruler.
    visible = Block.view_visible_track view - maybe 0
        Block.display_track_width (Seq.head (Block.block_tracks block))
    cur_tracknum = Types.sel_cur_track sel


-- ** mouse

-- | (mouse_down, mouse_modifier, (mouse_track, mouse_pos))
mouse_mod :: Msg.Msg -> Maybe (Bool, Cmd.Modifier, (TrackNum, UiMsg.Track))
mouse_mod msg = do
    mouse <- Msg.mouse msg
    (down, btn) <- case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn -> Just (True, btn)
        UiMsg.MouseDrag btn -> Just (False, btn)
        UiMsg.MouseUp btn -> Just (False, btn)
        _ -> Nothing
    track <- Msg.context_track msg
    return (down, Cmd.MouseMod btn (Just track), track)

-- * util

step_from :: Cmd.M m => TrackNum -> TrackTime -> Int -> TimeStep.TimeStep
    -> m TrackTime
step_from tracknum pos steps step = do
    block_id <- Cmd.get_focused_block
    end <- max <$> State.block_ruler_end block_id
        <*> State.block_event_end block_id
    next <- TimeStep.step_from steps step block_id tracknum pos
    return $ case next of
        Just next | 0 <= next && next <= end -> next
        _ -> pos

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
    (ViewId, BlockId, TrackId, TrackTime) context.

    - Get a single point from a selection, or a range on a single track, or
    a range of tracks.

    - Get an arbitrary Types.SelNum or use the Config.insert_selnum.

    - Return a Maybe or abort on Nothing.

    - Return for any track, or return a TrackId and abort if it's not an event
    track.

    - Used an arbitrary ViewId, BlockId, or use the focused view.

    And then there is a whole other dimension of converting selections, which
    are in TrackTime, to RealTime.  The selection can be converted either
    relative to its block's tempo, or relative to a calling block's tempo,
    namely the root block.

    Also, when a selection is interpreted as a point (for instance, for
    operations like \"play from selection\"), there is a choice of taking the
    point from the beginning of the selection, the end, or the 'sel_cur_pos',
    which is the dragged-to point.  The convention, established by
    'point' and 'point_track', is to take the first point.
-}

-- | Get the \"point\" position of a Selection.
point :: Types.Selection -> TrackTime
point sel = point_pos (Types.sel_start_pos sel) (Types.sel_cur_pos sel)

-- | Given a selection start and end, give the \"point\" position for it.
point_pos :: TrackTime -> TrackTime -> TrackTime
point_pos = min

point_track :: Types.Selection -> TrackNum
point_track sel = min (Types.sel_start_track sel) (Types.sel_cur_track sel)

-- | A point on a track.
type Point = (BlockId, TrackNum, TrackId, TrackTime)
type AnyPoint = (BlockId, TrackNum, TrackTime)

-- | Get the "insert position", which is the start track and position of the
-- insert selection.  Abort if it's not an event track.
get_insert :: Cmd.M m => m Point
get_insert = Cmd.abort_unless =<< lookup_insert

lookup_insert :: Cmd.M m => m (Maybe Point)
lookup_insert = fmap (fmap snd) $ lookup_selnum_insert Config.insert_selnum

lookup_selnum_insert :: Cmd.M m => Types.SelNum -> m (Maybe (ViewId, Point))
lookup_selnum_insert selnum =
    justm (lookup_any_selnum_insert selnum) $
    \(view_id, (block_id, tracknum, pos)) ->
    justm (State.event_track_at block_id tracknum) $ \track_id ->
    return $ Just (view_id, (block_id, tracknum, track_id, pos))

-- | Return the leftmost tracknum and trackpos, even if it's not an event
-- track.
get_any_insert :: Cmd.M m => m (ViewId, AnyPoint)
get_any_insert = Cmd.abort_unless =<< lookup_any_insert

lookup_any_insert :: Cmd.M m => m (Maybe (ViewId, AnyPoint))
lookup_any_insert = lookup_any_selnum_insert Config.insert_selnum

-- | The most general insertion point function.
lookup_any_selnum_insert :: Cmd.M m => Types.SelNum
    -> m (Maybe (ViewId, AnyPoint))
lookup_any_selnum_insert selnum =
    justm (lookup_selnum selnum) $ \(view_id, sel) -> do
        block_id <- State.block_id_of view_id
        return $ Just (view_id, (block_id, point_track sel, point sel))

-- | Given a block, get the selection on it, if any.  If there are multiple
-- views, take the one with the alphabetically first ViewId.
--
-- I'm not sure how to choose, but the first one seems reasonable for now.
lookup_block_insert :: State.M m => BlockId -> m (Maybe Point)
lookup_block_insert block_id = do
    view_ids <- Map.keys <$> State.views_of block_id
    case view_ids of
        [] -> return Nothing
        view_id : _ ->
            justm (State.get_selection view_id Config.insert_selnum) $ \sel ->
            justm (sel_track block_id sel) $ \track_id ->
            return $ Just (block_id, point_track sel, track_id, point sel)

-- | Get the point track of a selection.
sel_track :: State.M m => BlockId -> Types.Selection -> m (Maybe TrackId)
sel_track block_id sel = State.event_track_at block_id (point_track sel)

-- ** plain Selection

-- | Get the insertion selection in the focused view.
get :: Cmd.M m => m (ViewId, Types.Selection)
get = get_selnum Config.insert_selnum

-- | Get the requested selnum in the focused view.
get_selnum :: Cmd.M m => Types.SelNum -> m (ViewId, Types.Selection)
get_selnum selnum = Cmd.abort_unless =<< lookup_selnum selnum

lookup :: Cmd.M m => m (Maybe (ViewId, Types.Selection))
lookup = lookup_selnum Config.insert_selnum

lookup_selnum :: Cmd.M m => Types.SelNum
    -> m (Maybe (ViewId, Types.Selection))
lookup_selnum selnum =
    justm Cmd.lookup_focused_view $ \view_id ->
    justm (State.get_selection view_id selnum) $ \sel ->
    return $ Just (view_id, sel)

range :: Cmd.M m => m (TrackTime, TrackTime)
range = Types.sel_range . snd <$> get

-- ** selections in RealTime

-- TODO too much hardcoded use of the focused selection means this might not
-- be flexible enough.  Fix it if necessary.  Why are selections such a pain?

-- | Get the real time range of the focused selection.  If there's a root
-- block, then it will be in relative to that root, otherwise it's equivalent
-- to 'local_realtime'.
realtime :: Cmd.M m => m (BlockId, RealTime, RealTime)
realtime = do
    maybe_root_id <- State.lookup_root_id
    case maybe_root_id of
        Nothing -> local_realtime
        Just root_id -> do
            (s, e) <- relative_realtime root_id
            return (root_id, s, e)

-- | RealTime of the given ScoreTime, relative to the root block.  This is
-- the closest we get to an absolute real time.
root_realtime :: Cmd.M m => BlockId -> Maybe TrackId -> ScoreTime
    -> m (Maybe RealTime)
root_realtime block_id maybe_track_id pos =
    State.get_root_id >>= \root_block ->
    justm (Cmd.lookup_performance root_block) $ \perf ->
    Perf.lookup_realtime perf block_id maybe_track_id pos

-- | This is like 'get_insert', except get the selection on the root block,
-- falling back to the current one if there is none.
get_root_insert :: Cmd.M m => m Point
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
relative_realtime :: Cmd.M m => BlockId -> m (RealTime, RealTime)
relative_realtime root_id = do
    (view_id, sel) <- get
    block_id <- State.block_id_of view_id
    track_id <- Cmd.abort_unless =<< sel_track block_id sel
    maybe_root_sel <- lookup_block_insert root_id
    perf <- Cmd.get_performance root_id
    let root_pos = point_to_real (Cmd.perf_tempo perf) maybe_root_sel
    let warp = Cmd.perf_closest_warp perf block_id track_id root_pos
    let (start, end) = Types.sel_range sel
    return (Score.warp_pos warp start, Score.warp_pos warp end)

-- | Get the RealTime range of the current selection, as derived from current
-- selection's block.  This means that the top should be 0.
local_realtime :: Cmd.M m => m (BlockId, RealTime, RealTime)
local_realtime = do
    (view_id, sel) <- get
    block_id <- State.block_id_of view_id
    track_id <- Cmd.abort_unless =<< sel_track block_id sel
    perf <- Cmd.get_performance block_id
    let (start, end) = Types.sel_range sel
    let warp = Cmd.perf_closest_warp perf block_id track_id 0
    return (block_id, Score.warp_pos warp start, Score.warp_pos warp end)

-- | This is like 'relative_realtime' but gets a RealTime relative to a Point,
-- not a range.
relative_realtime_point :: Cmd.Performance -> Maybe Point -> Point -> RealTime
relative_realtime_point perf maybe_root_sel (block_id, _, track_id, pos) =
    Score.warp_pos warp pos
    where
    root_pos = point_to_real (Cmd.perf_tempo perf) maybe_root_sel
    warp = Cmd.perf_closest_warp perf block_id track_id root_pos

point_to_real :: Transport.TempoFunction -> Maybe Point -> RealTime
point_to_real _ Nothing = 0
point_to_real tempo (Just (block_id, _, track_id, pos)) =
    fromMaybe 0 $ Seq.head $ tempo block_id track_id pos

-- ** select events

-- | Selected events per track.  Gives events previous to, within, and after
-- the selection.  As usual, previous events are in descending order.  The
-- event range is also returned, which may not be the same as the selection
-- range because these functions may select more events than lie strictly
-- within the selection.
type SelectedAround = [(TrackId, (TrackTime, TrackTime),
    ([Event.Event], [Event.Event], [Event.Event]))]
type SelectedEvents = [(TrackId, (TrackTime, TrackTime), [Event.Event])]

-- | 'events_around' is the default selection behaviour.
events :: Cmd.M m => m SelectedEvents
events = fmap extract_events events_around
    where
    extract_events :: SelectedAround -> SelectedEvents
    extract_events = map $ \(track_id, range, (_, within, _)) ->
        (track_id, range, within)

events_around :: Cmd.M m => m SelectedAround
events_around = events_around_selnum Config.insert_selnum

-- | Select events whose @pos@ lie strictly within the selection range.
strict_events_around :: Cmd.M m => Types.SelNum -> m SelectedAround
strict_events_around selnum = do
    (_, _, track_ids, start, end) <- tracks_selnum selnum
    tracks <- mapM State.get_track track_ids
    let split events =
            (Events.descending pre, Events.ascending within,
                Events.ascending post)
            where (pre, within, post) = Events.split_range start end events
    return [(track_id, (start, end), split (Track.track_events track))
        | (track_id, track) <- zip track_ids tracks]

-- | Get events in the selection, but if no events are selected, expand it
-- to include a previous positive event or a following negative one.  If both
-- are present, the positive event is favored.  If neither are present, select
-- nothing.  And, if it's a point selection that coincides with the position
-- of an event, that event will be selected.
--
-- This is the standard definition of a selection, and should be used in all
-- standard selection using commands.
events_around_selnum :: Cmd.M m => Types.SelNum -> m SelectedAround
events_around_selnum selnum = do
    selected <- strict_events_around selnum
    return $ do
        (track_id, range, evts) <- selected
        let evts2 = expand (fst range) evts
        let range2 = expand_range evts2 range
        return (track_id, range2, evts2)
    where
    expand sel_start (before, [], after)
        | start_equal = (before, take 1 after, drop 1 after)
        | take_prev = (drop 1 before, take 1 before, after)
        | take_next = (before, take 1 after, drop 1 after)
        | otherwise = (before, [], after)
        where
        start_equal = maybe False ((==sel_start) . Event.start) (Seq.head after)
        take_prev = maybe False Event.positive (Seq.head before)
        take_next = maybe False Event.negative (Seq.head after)
    expand _ selected = selected
    expand_range (_, [evt], _) _ = Event.range evt
    expand_range _ range = range

-- ** select tracks

-- | @(block_id, [tracknums], [track_ids], start, end)@
--
-- The TrackNums are sorted, and the TrackIds are likewise in left-to-right
-- order.  Both lists are never empty.
type Tracks = (BlockId, [TrackNum], [TrackId], TrackTime, TrackTime)

-- | Get selected event tracks along with the selection.  The tracks are
-- returned in ascending order.  Only event tracks are returned, and tracks
-- merged into the selected tracks are included.
tracks :: Cmd.M m => m Tracks
tracks = tracks_selnum Config.insert_selnum

-- | Selected tracks, including merged tracks.
tracks_selnum :: Cmd.M m => Types.SelNum -> m Tracks
tracks_selnum selnum = do
    (block_id, tracknums, track_ids, start, end) <- strict_tracks_selnum selnum
    tracks <- mapM (State.get_block_track_at block_id) tracknums
    let merged_track_ids = mconcatMap Block.track_merged tracks
    block <- State.get_block block_id
    let merged = tracknums_of block (Set.toList merged_track_ids)
    let (all_tracknums, all_track_ids) = unzip $ List.sort $ List.nub $
            merged ++ zip tracknums track_ids
    return (block_id, all_tracknums, all_track_ids, start, end)

-- | Selected tracks, not including merged tracks.
strict_tracks_selnum :: Cmd.M m => Types.SelNum -> m Tracks
strict_tracks_selnum selnum = do
    (view_id, sel) <- get_selnum selnum
    block_id <- State.block_id_of view_id
    tracks <- State.track_count block_id
    let tracknums = Types.sel_tracknums tracks sel
    tracklikes <- mapM (State.track_at block_id) tracknums
    (tracknums, track_ids) <- return $ unzip
        [(i, track_id) | (i, Just (Block.TId track_id _))
            <- zip tracknums tracklikes]
    let (start, end) = Types.sel_range sel
    return (block_id, tracknums, track_ids, start, end)

tracknums_of :: Block.Block -> [TrackId] -> [(TrackNum, TrackId)]
tracknums_of block track_ids = do
    (tracknum, Block.TId tid _) <-
        zip [0..] (Block.block_tracklike_ids block)
    guard (tid `elem` track_ids)
    return (tracknum, tid)
