-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Commands dealing with selection and cursor movement.

    As is typical, when it comes to selecting events, the selection represents
    a half-open range.  However, reflecting the orientation of events,
    a negative event at the start of the range won't be included, and
    a negative event at he end of the range will be included.  This is natural
    for events with negative duration, since they are weighted at the end.

    This behaviour is actually implemented in the low level "Ui.Events"
    functions.
-}
module Cmd.Selection where
import           Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Warp as Warp
import qualified Midi.Midi as Midi
import qualified Midi.Mmc as Mmc
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Zoom as Zoom

import           Global
import           Types


-- * cmds

getView :: Cmd.M m => ViewId -> m Sel.Selection
getView = Cmd.abortUnless <=< lookupView

lookupView :: Ui.M m => ViewId -> m (Maybe Sel.Selection)
lookupView viewId = Ui.getSelection viewId Config.insertSelnum

setView :: Ui.M m => ViewId -> Maybe Sel.Selection -> m ()
setView viewId = Ui.setSelection viewId Config.insertSelnum

{- | Set or unset the selection.

    This is the Cmd level of Ui.set_selection and should be called by
    any Cmd that wants to set the selection.

    The insert selnum always has an orientation, so if 'Sel.orientation' is
    'Sel.None', default to 'Cmd.state_note_orientation'.  Otherwise, update
    the note orientation, so the visible orientation and actual orientation stay
    in sync.
-}
set :: Cmd.M m => ViewId -> Maybe Sel.Selection -> m ()
set viewId maybeSel = do
    setSelnum viewId Config.insertSelnum maybeSel
    recordHistory

setWithoutHistory :: Cmd.M m => ViewId -> Maybe Sel.Selection -> m ()
setWithoutHistory viewId = setSelnum viewId Config.insertSelnum

setSelnum :: Cmd.M m => ViewId -> Sel.Num -> Maybe Sel.Selection -> m ()
setSelnum viewId selnum maybeSel
    | selnum == Config.insertSelnum = do
        maybeSel <- traverse inferOrientation maybeSel
        Ui.setSelection viewId selnum maybeSel
        whenJust maybeSel $ \sel -> when (Sel.isPoint sel) $ do
            setSubs viewId sel
            whenJustM (Cmd.gets (Cmd.stateSync . Cmd.statePlay)) $
                mmcGotoSel viewId sel
    | otherwise = Ui.setSelection viewId selnum maybeSel
    where
    -- If the new selection has an opinion about orientation, update
    -- Cmd.state_note_orientation.
    inferOrientation sel = case Sel.orientation sel of
        Sel.None -> do
            o <- getOrientation
            pure $ sel { Sel.orientation = o }
        Sel.Positive -> do
            Cmd.modifyEditState $ \state ->
                state { Cmd.stateNoteOrientation = Types.Positive }
            pure sel
        Sel.Negative -> do
            Cmd.modifyEditState $ \state ->
                state { Cmd.stateNoteOrientation = Types.Negative }
            pure sel

getOrientation :: Cmd.M m => m Sel.Orientation
getOrientation =
    Cmd.gets $ convert . Cmd.stateNoteOrientation . Cmd.stateEdit
    where
    convert Types.Positive = Sel.Positive
    convert Types.Negative = Sel.Negative

mmcGotoSel :: Cmd.M m => ViewId -> Sel.Selection -> Cmd.SyncConfig -> m ()
mmcGotoSel viewId sel sync = do
    blockId <- Ui.blockIdOf viewId
    maybeTrackId <- Ui.eventTrackAt blockId (Sel.curTrack sel)
    whenJustM (rootRealtime blockId maybeTrackId (Sel.curPos sel)) $
        Cmd.midi (Cmd.syncDevice sync) . mmcGoto sync

mmcGoto :: Cmd.SyncConfig -> RealTime -> Midi.Message
mmcGoto sync pos = Mmc.encode (Cmd.syncDeviceId sync) $
    Mmc.gotoSeconds (Cmd.syncFrameRate sync) (RealTime.toSeconds pos)

-- | Set a selection in the current view.
setCurrent :: Cmd.M m => Sel.Num -> Maybe Sel.Selection -> m ()
setCurrent selnum maybeSel = do
    viewId <- Cmd.getFocusedView
    setSelnum viewId selnum maybeSel

-- | For point selections, set a play position selection on the equivalent
-- time in sub-blocks.  This makes it easier to edit the super-block relative
-- to the sub-block.
--
-- TODO if multiple calls overlap, I should draw multiple selections, but
-- Ui.set_selection doesn't support that, even though the underlying
-- BlockC.set_selection does.
setSubs :: Cmd.M m => ViewId -> Sel.Selection -> m ()
setSubs viewId sel = do
    viewIds <- Ui.allViewIds
    forM_ viewIds $ \vid ->
        Ui.setSelection vid Config.playPositionSelnum Nothing
    blockId <- Ui.blockIdOf viewId
    maybeTrackId <- Ui.eventTrackAt blockId (Sel.curTrack sel)
    whenJust maybeTrackId $ \trackId ->
        mapM_ (uncurry setBlock) =<<
            Perf.subPos blockId trackId (Sel.curPos sel)

setBlock :: Ui.M m => BlockId -> [(TrackId, TrackTime)] -> m ()
setBlock _ [] = pure ()
setBlock blockId ((_, pos) : _) = do
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    forM_ viewIds $ \viewId ->
        Ui.setSelection viewId Config.playPositionSelnum $ Just $
            Sel.Selection
                { startTrack = 0, startPos = pos
                , curTrack = 999, curPos = pos
                , orientation = Sel.Positive
                }

-- ** modify existing selection

-- | Update the selection's orientation to match 'Cmd.state_note_orientation'.
updateOrientation :: Cmd.M m => m ()
updateOrientation = whenJustM lookupContext $ \(Context viewId _ sel) ->
    set viewId $ Just $ sel { Sel.orientation = Sel.None }

-- | Collapse the selection to a point at its (cur_track, cur_pos).
toPoint :: Cmd.M m => Bool -> m ()
toPoint toCurPos = do
    (viewId, old) <- getViewSel
    selectable <- Ui.selectableTracks <$>
        (Ui.getBlock =<< Ui.blockIdOf viewId)
    let closest = fromMaybe (Sel.curTrack old) $
            findAtBefore (Sel.curTrack old) selectable
    let new
            | toCurPos = old
                { Sel.startTrack = closest, Sel.curTrack = closest
                , Sel.startPos = Sel.curPos old
                }
            | otherwise = old
                { Sel.startTrack = closest, Sel.curTrack = closest
                , Sel.curPos = Sel.startPos old
                }
    -- auto_scroll doesn't scroll if Sel.cur_pos hasn't moved, but in this
    -- case I do want it to move, so I jump to it when collapsing to_cur_pos.
    -- So trick auto_scroll by pretending the old cur_pos was different.
    let old2 = if Sel.curPos old == Sel.curPos new
        then old { Sel.curPos = Sel.startPos old } else old
    autoScroll viewId (Just old2) new
    set viewId (Just new)

findAtBefore :: Ord a => a -> [a] -> Maybe a
findAtBefore n = Lists.last . takeWhile (<=n)

-- | Advance the insert selection by the current step, which is a popular thing
-- to do.
advance :: Cmd.M m => m ()
advance = step TimeStep.Advance defaultMove

-- | How to move a selection.
data Move
    = Extend -- ^ Extend the existing selection.
    | Move -- ^ Move the existing selection by the step amount.
    | Replace -- ^ Replace the existing selection with a point selection.
    deriving (Show)

-- | Use this Move mode when you don't have a more specific idea.
defaultMove :: Move
defaultMove = Move

-- | Advance the given selection by the current step.
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.
step :: Cmd.M m => TimeStep.Direction -> Move -> m ()
step dir move = do
    st <- Cmd.getCurrentStep
    stepWith (TimeStep.direction dir) move st

stepWith :: Cmd.M m => Int -> Move -> TimeStep.TimeStep -> m ()
stepWith steps move step = do
    viewId <- Cmd.getFocusedView
    old@(Sel.Selection startTrack startPos curTrack curPos _) <-
        getView viewId
    newPos <- stepFrom curTrack curPos steps step
    let newSel = case move of
            Extend -> Sel.Selection
                { startTrack = startTrack, startPos = startPos
                , curTrack = curTrack, curPos = newPos
                , orientation = Sel.None
                }
            Move -> Sel.move (newPos - curPos) old
            Replace -> Sel.point curTrack newPos Sel.None
    setWithoutHistory viewId (Just newSel)
    autoScroll viewId (Just old) newSel

-- | Move the selection across tracks by @shift@, possibly skipping non-event
-- tracks and collapsed tracks.
shift :: Cmd.M m => Bool -> Move -> Int -> m ()
shift skipUnselectable move shift = modify $ \block old ->
    let new = Ui.shiftSelection skipUnselectable block shift old
    in case move of
        Extend -> Sel.merge old new
        Move -> new
        Replace -> Sel.point (Sel.curTrack new) (Sel.curPos new)
            (Sel.orientation new)

-- | Unlike 'shift', this uses 'Sel.union', which means that the selection will
-- always expand, instead of only expanding if the current track is moving away
-- from the start track.  This is because I use this as a way to expand the
-- selection rather than move it.
jumpToTrack :: Cmd.M m => Move -> TrackNum -> m ()
jumpToTrack move tracknum = modify $ \_ old ->
    let new = Sel.modifyTracks (+ (tracknum - Sel.curTrack old)) old
    in case move of
        Extend -> Sel.union old (Sel.modifyTracks (const tracknum) old)
        Move -> new
        Replace -> Sel.point (Sel.curTrack new) (Sel.curPos new)
            (Sel.orientation new)

modify :: Cmd.M m => (Block.Block -> Sel.Selection -> Sel.Selection) -> m ()
modify f = do
    viewId <- Cmd.getFocusedView
    block <- Ui.blockOf viewId
    sel <- getView viewId
    let new = f block sel
    setWithoutHistory viewId (Just new)
    autoScroll viewId (Just sel) new

data Direction = R | L deriving (Eq, Show)

findNoteTrack :: Cmd.M m => Direction -> Bool -> m (Maybe TrackNum)
findNoteTrack dir oneBefore  = do
    tracks <- getTracksFromSelection oneBefore dir
    pure $ Ui.trackTracknum <$>
        find (ParseTitle.isNoteTrack . Ui.trackTitle) tracks
    where
    find f
        | oneBefore = if dir == R then findBefore f else List.find f
        | otherwise = List.find f

-- | Find the element before the predicate matches, or the last element if it
-- never matches.
findBefore :: (a -> Bool) -> [a] -> Maybe a
findBefore p = go
    where
    go (x1 : xs@(x2 : _))
        | p x2 = Just x1
        | otherwise = go xs
    go [x] = Just x
    go [] = Nothing

-- | Get tracks either starting from the right of the selection, or the left.
-- Unselectable tracks are omitted.
getTracksFromSelection :: Cmd.M m => Bool -- ^ If True, start from the R or
    -- L edge of the selection, rather than 'Sel.cur_track'.
    -> Direction -> m [Ui.TrackInfo]
getTracksFromSelection fromEdge dir = do
    (viewId, sel) <- getViewSel
    let tracknum = if fromEdge
            then (if dir == R then snd else fst) (Sel.trackRange sel)
            else Sel.curTrack sel
    blockId <- Ui.blockIdOf viewId
    tracks <- filter (Block.trackSelectable . Ui.trackBlock) <$>
        TrackTree.tracksOf blockId
    pure $ case dir of
        R -> dropWhile ((<= tracknum) . Ui.trackTracknum) tracks
        L -> dropWhile ((>= tracknum) . Ui.trackTracknum) (reverse tracks)

-- | Progressive selection: select the rest of the track, then the entire
-- track, then the whole block.
--
-- TrackNum 0, assumed to be a ruler, is omitted, since selecting the ruler is
-- not only not useful, it tends to make cmds that want to get a TrackId abort.
cmdTrackAll :: Cmd.M m => m ()
cmdTrackAll = do
    (viewId, sel) <- getViewSel
    blockId <- Ui.blockIdOf viewId
    blockEnd <- Ui.blockEnd blockId
    tracks <- Ui.trackCount blockId
    set viewId $ Just (selectTrackAll blockEnd tracks sel)

selectTrackAll :: TrackTime -> TrackNum -> Sel.Selection -> Sel.Selection
selectTrackAll blockEnd tracks sel
    | sel == selectTracks = selectAll
    | sel == selectRest = selectTracks
    | otherwise = selectRest
    where
    selectRest = untilEnd $ sel { Sel.curPos = Sel.min sel }
    selectTracks = untilEnd $ sel { Sel.curPos = 0 }
    selectAll = untilEnd $ trackSelection 1 (tracks - 1)
    untilEnd = selectUntilEnd blockEnd
    trackSelection from to = Sel.Selection
        { startTrack = from, startPos = 0
        , curTrack = to, curPos = 0
        , orientation = Sel.orientation sel
        }

-- | Extend the selection horizontally to encompass all tracks.  If it already
-- does, reset the track selection to the previous one.
cmdToggleExtendTracks :: Cmd.M m => m ()
cmdToggleExtendTracks = do
    (viewId, sel) <- getViewSel
    tracks <- Ui.trackCount =<< Ui.blockIdOf viewId
    let expanded = sel { Sel.curTrack = 1, Sel.startTrack = tracks - 1 }
    if sel /= expanded
        then set viewId $ Just expanded
        else previousSelection False

-- ** set selection from clicks

-- | Select clicked on track.
cmdSelectTrack :: Cmd.M m => Types.MouseButton -> Msg.Msg -> m ()
cmdSelectTrack btn msg = do
    viewId <- Cmd.getFocusedView
    ((downTracknum, _), (mouseTracknum, _)) <- mouseDrag btn msg
    selectTracks viewId downTracknum mouseTracknum

selectTracks :: Cmd.M m => ViewId -> TrackNum -> TrackNum -> m ()
selectTracks viewId from to = do
    blockEnd <- Ui.blockEnd =<< Ui.blockIdOf viewId
    set viewId $ Just $ selectUntilEnd blockEnd $
        Sel.Selection
            { startTrack = from, startPos = 0
            , curTrack = to, curPos = 0
            , orientation = Sel.Positive
            }

-- | Extend the selection to the end of then block.  This sets 'Sel.start_pos',
-- with the assumption that 'Sel.cur_pos' is onscreen.  This is so
-- 'auto_scroll' won't jump to the bottom of the block.
selectUntilEnd :: TrackTime -> Sel.Selection -> Sel.Selection
selectUntilEnd blockEnd sel =
    sel { Sel.startPos = blockEnd + ScoreTime.eta }
    -- Without ScoreTime.eta, a select-all won't include an event at the end of
    -- the block.

-- | Set the selection based on a click or drag.
cmdMouseSelection :: Cmd.M m => Types.MouseButton -> Bool -> Msg.Msg -> m ()
cmdMouseSelection btn extend msg = do
    ((downTracknum, downPos), (mouseTracknum, mousePos))
        <- mouseDragPos btn msg
    viewId <- Cmd.getFocusedView
    oldSel <- lookupView viewId
    let (startTracknum, startPos) = case (extend, oldSel) of
            (True, Just (Sel.Selection tracknum pos _ _ _)) -> (tracknum, pos)
            _ -> (downTracknum, downPos)
    when (Msg.mouseDown msg) recordHistory
    let sel = Sel.Selection
            { startTrack = startTracknum, startPos = startPos
            , curTrack = mouseTracknum, curPos = mousePos
            , orientation = Sel.None
            }
    setAndScroll viewId sel

-- | Like 'cmd_mouse_selection', but snap the selection to the current time
-- step.
cmdSnapSelection :: Cmd.M m => Types.MouseButton -> Bool -> Msg.Msg -> m ()
cmdSnapSelection btn extend msg = do
    ((downTracknum, _), (mouseTracknum, mousePos)) <- mouseDragPos btn msg
    blockId <- Cmd.getFocusedBlock
    step <- Cmd.getCurrentStep
    viewId <- Cmd.getFocusedView
    oldSel <- lookupView viewId
    snapPos <- TimeStep.snap step blockId mouseTracknum
        (Sel.curPos <$> oldSel) mousePos
    snapPos <- snapOverThreshold viewId blockId mousePos snapPos
    let sel = case oldSel of
            _ | oldSel == Nothing || Msg.mouseDown msg && not extend ->
                Sel.Selection
                    { startTrack = downTracknum, startPos = snapPos
                    , curTrack = mouseTracknum, curPos = snapPos
                    , orientation = Sel.None
                    }
            Just (Sel.Selection tracknum pos _ _ _) -> Sel.Selection
                { startTrack = tracknum, startPos = pos
                , curTrack = mouseTracknum, curPos = snapPos
                , orientation = Sel.None
                }
            -- ghc doesn't realize it is exhaustive
            _ -> error "Cmd.Selection: not reached"
    when (Msg.mouseDown msg) recordHistory
    setAndScroll viewId sel
    where
    -- If I'm dragging, only snap if I'm close to a snap point.  Otherwise,
    -- it's easy for the selection to jump way off screen while dragging.
    snapOverThreshold viewId blockId pos snap = do
        zoom <- Ui.getZoom viewId
        let over = Zoom.toPixels zoom (abs (snap - pos)) > threshold
        -- Don't go past the end of the ruler.  Otherwise it's easy to
        -- accidentally select too much by dragging to the end of the block.
        end <- Ui.blockRulerEnd blockId
        pure $ if not (Msg.mouseDown msg) && over && pos < end
            then pos else snap
    threshold = 20

-- | Like 'mouse_drag' but specialized for drags on the track.
mouseDragPos :: Cmd.M m => Types.MouseButton -> Msg.Msg
    -> m ((TrackNum, TrackTime), (TrackNum, TrackTime))
mouseDragPos btn msg = do
    ((num1, t1), (num2, t2)) <- mouseDrag btn msg
    case (t1, t2) of
        (UiMsg.Track p1, UiMsg.Track p2) -> pure ((num1, p1), (num2, p2))
        _ -> Cmd.abort

-- | Get the clicked or dragged range, or abort if this isn't a drag Msg.
-- This accepts clicks as well, and considers them an empty range.
mouseDrag :: Cmd.M m => Types.MouseButton -> Msg.Msg
    -> m ((TrackNum, UiMsg.Track), (TrackNum, UiMsg.Track))
    -- ^ (mouse down at, mouse currently at)
mouseDrag btn msg = do
    (isDown, mod, mouseAt) <- Cmd.abortUnless (mouseMod msg)
    when (isDown && fst mouseAt == 0) Cmd.abort
    msgBtn <- Cmd.abortUnless (Cmd.mouseModBtn mod)
    -- The button down should be the same one as expected.
    when (msgBtn /= btn) Cmd.abort
    keysDown <- Cmd.keysDown
    let mouseDown = Map.lookup (Cmd.stripModifier mod) keysDown
    let downAt = case (isDown, mouseDown) of
            (False, Just (Cmd.MouseMod _ (Just track))) -> track
            -- If it's not already held down, it starts here.
            _ -> mouseAt
    -- MsgCollector is back to clamping at track-1, but leave this in in
    -- case I change my mind again.
    -- -- Clip a drag past the last track to the last track, callers here treat
    -- -- it as the same.
    -- tracks <- Ui.track_count =<< Cmd.get_focused_block
    -- let clamp (tnum, track) = (min (tracks-1) tnum, track)
    -- return (clamp down_at, clamp mouse_at)
    pure (first (max 1) downAt, first (max 1) mouseAt)

-- * implementation

-- ** auto scroll

-- | Figure out how much to scroll to keep the selection visible and with
-- reasonable space around it.
--
-- Anyone who wants to set a selection and automatically scroll the window to
-- follow the selection should use this function.
setAndScroll :: Cmd.M m => ViewId -> Sel.Selection -> m ()
setAndScroll viewId sel = do
    old <- lookupView viewId
    autoScroll viewId old sel
    set viewId (Just sel)

-- | If the selection has scrolled off the edge of the window, automatically
-- scroll it so that the \"current\" end of the selection is in view.
autoScroll :: Cmd.M m => ViewId -> Maybe Sel.Selection -> Sel.Selection -> m ()
autoScroll viewId old new = do
    view <- Ui.getView viewId
    block <- Ui.getBlock (Block.viewBlock view)
    end <- Ui.blockEnd (Block.viewBlock view)
    let timeOffset = Num.clamp 0 (end - Block.visibleTime view) $
            autoTimeScroll view (Sel.curPos <$> old) (Sel.curPos new)
        trackOffset = autoTrackScroll block view new
    Ui.modifyZoom viewId $ \zoom -> zoom { Zoom.offset = timeOffset }
    Ui.setTrackScroll viewId trackOffset

-- TODO this scrolls too fast when dragging.  Detect a drag and scroll at
-- a rate determined by how far past the bottom the pointer is.
autoTimeScroll :: Block.View -> Maybe TrackTime -> TrackTime -> TrackTime
autoTimeScroll view prevPos pos
    -- Don't scroll if the cur pos hasn't changed.  Otherwise, selecting the
    -- whole track and shifting tracks scrolls the block down to the bottom.
    | Just pos == prevPos = viewStart
    | pos >= viewEnd = pos - visible + space
    | pos < viewStart = pos - space
    | otherwise = viewStart
    where
    visible = Block.visibleTime view
    viewStart = Zoom.offset (Block.viewZoom view)
    viewEnd = viewStart + visible
    space = ScoreTime.fromDouble $
        visiblePixels / Zoom.factor (Block.viewZoom view)
    visiblePixels = 30

-- | Find the track scroll that would put the given selection into view.
autoTrackScroll :: Block.Block -> Block.View -> Sel.Selection -> Types.Width
autoTrackScroll block view sel
    | trackEnd > viewEnd = trackEnd - visible
    | trackStart < viewStart = trackStart
    | otherwise = viewStart
    where
    -- Pesky ruler track doesn't count towards the track scroll.
    widths = map Block.displayTrackWidth (drop 1 (Block.blockTracks block))
    trackStart = Num.sum (take (curTracknum-1) widths)
    trackEnd = Num.sum (take curTracknum widths)
    viewStart = Block.viewTrackScroll view
    viewEnd = viewStart + visible
    -- Visible does include the pesky ruler.
    visible = Block.visibleTrack view - maybe 0
        Block.displayTrackWidth (Lists.head (Block.blockTracks block))
    curTracknum = Sel.curTrack sel


-- ** mouse

-- | (mouse_down, mouse_modifier, (mouse_track, mouse_pos))
mouseMod :: Msg.Msg -> Maybe (Bool, Cmd.Modifier, (TrackNum, UiMsg.Track))
mouseMod msg = do
    mouse <- Msg.mouse msg
    (down, btn) <- case UiMsg.mouseState mouse of
        UiMsg.MouseDown btn -> Just (True, btn)
        UiMsg.MouseDrag btn -> Just (False, btn)
        UiMsg.MouseUp btn -> Just (False, btn)
        _ -> Nothing
    track <- Msg.contextTrack msg
    pure (down, Cmd.MouseMod btn (Just track), track)

-- * util

stepFrom :: Cmd.M m => TrackNum -> TrackTime -> Int -> TimeStep.TimeStep
    -> m TrackTime
stepFrom tracknum pos steps step = do
    blockId <- Cmd.getFocusedBlock
    end <- max <$> Ui.blockRulerEnd blockId <*> Ui.blockEventEnd blockId
    next <- TimeStep.stepFrom steps step blockId tracknum pos
    pure $ case next of
        Just next | 0 <= next && next <= end -> next
        _ -> pos

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler that has all the given marklist names.  This includes ruler
-- tracks and the rulers of event tracks.
relevantRuler :: Block.Block -> TrackNum -> Maybe RulerId
relevantRuler block tracknum = Lists.at (Block.rulerIdsOf inOrder) 0
    where
    inOrder = map (Block.tracklikeId . snd) $ dropWhile ((/=tracknum) . fst) $
        reverse $ zip [0..] (Block.blockTracks block)

-- * get selection info

{- Getting the selection may seem pretty simple, but there are a number of
    orthogonal flavors:

    - Return a raw Sel.Selection, or return its
    (ViewId, BlockId, TrackId, TrackTime) context.

    - Get a single point from a selection, or a range on a single track, or
    a range of tracks.

    - Get an arbitrary Sel.Num or use the Config.insert_selnum.

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
    'sel_point' and 'sel_point_track', is to use the active end of the
    selection.
-}

-- | Get the \"point\" position of a Selection.
selPoint :: Sel.Selection -> TrackTime
selPoint = Sel.curPos

-- | When multiple tracks are selected, only one can be the point.  This
-- is 'Sel.cur_track', and in fact must be, because 'Sel.start_track' may
-- be an invalid tracknum.  That is so a selection can maintain its shape even
-- if it momentarily goes out of bounds.
selPointTrack :: Sel.Selection -> TrackNum
selPointTrack = Sel.curTrack

-- | A point on a track.
type Point = (BlockId, TrackNum, TrackId, TrackTime)
type AnyPoint = (BlockId, TrackNum, TrackTime)

-- | Get the "insert position", which is the start track and position of the
-- insert selection.  Abort if it's not an event track.
getInsert :: Cmd.M m => m Point
getInsert = Cmd.abortUnless =<< lookupInsert

lookupInsert :: Cmd.M m => m (Maybe Point)
lookupInsert = fmap (fmap snd) $ lookupSelnumInsert Config.insertSelnum

lookupSelnumInsert :: Cmd.M m => Sel.Num -> m (Maybe (ViewId, Point))
lookupSelnumInsert selnum =
    justm (lookupAnySelnumInsert selnum) $
    \(viewId, (blockId, tracknum, pos)) ->
    justm (Ui.eventTrackAt blockId tracknum) $ \trackId ->
    pure $ Just (viewId, (blockId, tracknum, trackId, pos))

-- | Return the leftmost tracknum and trackpos, even if it's not an event
-- track.
getAnyInsert :: Cmd.M m => m (ViewId, AnyPoint)
getAnyInsert = Cmd.abortUnless =<< lookupAnyInsert

lookupAnyInsert :: Cmd.M m => m (Maybe (ViewId, AnyPoint))
lookupAnyInsert = lookupAnySelnumInsert Config.insertSelnum

-- | The most general insertion point function.
lookupAnySelnumInsert :: Cmd.M m => Sel.Num -> m (Maybe (ViewId, AnyPoint))
lookupAnySelnumInsert selnum =
    justm (lookupContextSelnum selnum) $ \(Context viewId blockId sel) ->
        pure $ Just (viewId, (blockId, selPointTrack sel, selPoint sel))

-- | Given a block, get the selection on it, if any.  If there are multiple
-- views, take the one with the alphabetically first ViewId.
--
-- I'm not sure how to choose, but the first one seems reasonable for now.
lookupBlockInsert :: Ui.M m => BlockId -> m (Maybe Point)
lookupBlockInsert blockId = do
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    case viewIds of
        [] -> pure Nothing
        viewId : _ ->
            justm (lookupView viewId) $ \sel ->
            justm (selTrack blockId sel) $ \trackId ->
            pure $ Just
                (blockId, selPointTrack sel, trackId, selPoint sel)

-- | Get the point track of a selection.
selTrack :: Ui.M m => BlockId -> Sel.Selection -> m (Maybe TrackId)
selTrack blockId sel = Ui.eventTrackAt blockId (selPointTrack sel)

-- ** plain Selection

-- | The selection is in the Ui.State, but focused view is in Cmd.State.
-- Extracting it out lets me depend only on Ui.M, not Cmd.M.  Also it decouples
-- selection functions from the current Ui.State and Cmd.State.
data Context = Context
    { ctxViewId :: !ViewId
    , ctxBlockId :: !BlockId
    , ctxSelection :: !Sel.Selection
    } deriving (Show)

ctxViewSelection :: Context -> (ViewId, Sel.Selection)
ctxViewSelection ctx = (ctxViewId ctx, ctxSelection ctx)

context :: Cmd.M m => m Context
context = contextSelnum Config.insertSelnum

contextSelnum :: Cmd.M m => Sel.Num -> m Context
contextSelnum selnum = Cmd.abortUnless =<< lookupContextSelnum selnum

lookupContext :: Cmd.M m => m (Maybe Context)
lookupContext = lookupContextSelnum Config.insertSelnum

lookupContextSelnum :: Cmd.M m => Sel.Num -> m (Maybe Context)
lookupContextSelnum selnum =
    justm Cmd.lookupFocusedView $ \viewId ->
    justm (Ui.getSelection viewId selnum) $ \sel -> do
        blockId <- Ui.blockIdOf viewId
        pure $ Just $ Context viewId blockId sel

get :: Cmd.M m => m Sel.Selection
get = Cmd.abortUnless =<< lookup

lookup :: Cmd.M m => m (Maybe Sel.Selection)
lookup = fmap ctxSelection <$> lookupContext

getViewSel :: Cmd.M m => m (ViewId, Sel.Selection)
getViewSel = ctxViewSelection <$> context

range :: Cmd.M m => m Events.Range
range = Events.selectionRange <$> get

point :: Cmd.M m => m TrackTime
point = selPoint <$> get

start, end :: Cmd.M m => m TrackTime
start = Sel.min <$> get
end = Sel.max <$> get

-- ** selections in RealTime

-- | Get the real time range of the focused selection.  If there's a root
-- block, then it will be in relative to that root, otherwise it's equivalent
-- to 'local_realtime'.
realtime :: Cmd.M m => m (BlockId, RealTime, RealTime)
realtime = do
    maybeRootId <- Ui.lookupRootId
    case maybeRootId of
        Nothing -> localRealtime
        Just rootId -> do
            (s, e) <- relativeRealtime rootId
            pure (rootId, s, e)

-- | RealTime of the given ScoreTime, relative to the root block.  This is
-- the closest we get to an absolute real time.
rootRealtime :: Cmd.M m => BlockId -> Maybe TrackId -> ScoreTime
    -> m (Maybe RealTime)
rootRealtime blockId maybeTrackId pos =
    Ui.getRootId >>= \rootBlock ->
    justm (Cmd.lookupPerformance rootBlock) $ \perf ->
    Perf.lookupRealtime perf blockId maybeTrackId pos

-- | This is like 'get_insert', except get the selection on the root block,
-- falling back to the current one if there is none.
getRootInsert :: Cmd.M m => m Point
getRootInsert = maybe getInsert pure =<< rootsel
    where
    rootsel = justm Ui.lookupRootId $ \rootId ->
        lookupBlockInsert rootId

-- | Get the current selection in RealTime relative to another block.
--
-- If a block is called in multiple places, a score time on it may occur at
-- multiple real times.  Pick the real time from the given selection which is
-- closest to the real time of the selection on the given root block.
--
-- If there's no selection on the root block then return the RealTime from the
-- block's first occurrance.
relativeRealtime :: Cmd.M m => BlockId -> m (RealTime, RealTime)
relativeRealtime rootId = do
    (viewId, sel) <- getViewSel
    blockId <- Ui.blockIdOf viewId
    trackId <- Cmd.abortUnless =<< selTrack blockId sel
    maybeRootSel <- lookupBlockInsert rootId
    perf <- Cmd.getPerformance rootId
    let rootPos = pointToReal (Cmd.perfTempo perf) maybeRootSel
    let warp = Cmd.perfClosestWarp perf blockId trackId rootPos
    let (start, end) = Sel.range sel
    pure (Warp.warp warp start, Warp.warp warp end)

-- | Get the RealTime range of the current selection, as derived from current
-- selection's block.  This means that the top should be 0.
localRealtime :: Cmd.M m => m (BlockId, RealTime, RealTime)
localRealtime = do
    (viewId, sel) <- getViewSel
    blockId <- Ui.blockIdOf viewId
    trackId <- Cmd.abortUnless =<< selTrack blockId sel
    perf <- Cmd.getPerformance blockId
    let (start, end) = Sel.range sel
    let warp = Cmd.perfClosestWarp perf blockId trackId 0
    pure (blockId, Warp.warp warp start, Warp.warp warp end)

-- | This is like 'relative_realtime' but gets a RealTime relative to a Point,
-- not a range.
relativeRealtimePoint :: Cmd.Performance -> Maybe Point -> Point -> RealTime
relativeRealtimePoint perf maybeRootSel (blockId, _, trackId, pos) =
    Warp.warp warp pos
    where
    rootPos = pointToReal (Cmd.perfTempo perf) maybeRootSel
    warp = Cmd.perfClosestWarp perf blockId trackId rootPos

pointToReal :: Transport.TempoFunction -> Maybe Point -> RealTime
pointToReal _ Nothing = 0
pointToReal tempo (Just (blockId, _, trackId, pos)) =
    fromMaybe 0 $ Lists.head $ tempo blockId trackId pos

-- ** select events

-- | Selected events per track.  Gives events before, within, and after the
-- selection.  As usual, previous events are in descending order.
type SelectedAround = [(TrackId, ([Event.Event], [Event.Event], [Event.Event]))]
type SelectedEvents = [(TrackId, [Event.Event])]

aroundToEvents :: SelectedAround -> SelectedEvents
aroundToEvents = map $ \(trackId, (_, within, _)) -> (trackId, within)

-- | All selected events.  'events_around' is the default selection behaviour.
events :: Cmd.M m => m SelectedEvents
events = ctxEvents =<< context

ctxEvents :: Ui.M m => Context -> m SelectedEvents
ctxEvents ctx = aroundToEvents <$> ctxEventsAround ctx

-- | Like 'events', but only for the 'sel_point_track'.
trackEvents :: Cmd.M m => m (TrackId, [Event.Event])
trackEvents = ctxTrackEvents =<< context

ctxTrackEvents :: Ui.M m => Context -> m (TrackId, [Event.Event])
ctxTrackEvents ctx = do
    events <- aroundToEvents <$> ctxEventsAround ctx
    Ui.require "ctx_events_around output should be 1:1 with TrackIds" $
        Lists.head events

eventsAround :: Cmd.M m => m SelectedAround
eventsAround = ctxEventsAround =<< context

-- | Get events exactly at the point.  This gets both positive and negative
-- events, so each track may have up to 2 events.
eventsAtPoint :: Cmd.M m => m SelectedEvents
eventsAtPoint = do
    trackIds <- trackIds
    pos <- point
    fmap (filter (not . null . snd)) $ forM trackIds $ \trackId -> do
        events <- Ui.getEvents trackId
        pure $ (trackId,) $
            maybe [] (:[]) (Events.at pos Types.Negative events)
            ++ maybe [] (:[]) (Events.at pos Types.Positive events)

-- -- | Like 'event_around', but select as if the selection were a point.
-- -- Suitable for cmds that logically only work on a single event per-track.
-- events_around_point :: Cmd.M m => m SelectedAround
-- events_around_point = do
--     ctx <- context
--     let sel = ctx_selection ctx
--     ctx_events_around $ ctx
--         { ctx_selection = sel
--             { Sel.start_pos = sel_point sel
--             , Sel.cur_pos = sel_point sel
--             }
--         }

-- | Get events in the selection, but if no events are selected, expand it
-- to include a neighboring event, as documented in 'select_neighbor'.
--
-- Normally the range is half-open by event orientation, so a positive event at
-- the end of the selection won't be included.  But there's a special hack
-- where if the end of the selection happens to be the end of the block,
-- a positive event there will be included anyway.  Otherwise it's annoying to
-- select a final event (unless it's negative).
--
-- This is the standard definition of a selection, and should be used in all
-- standard selection using commands.
ctxEventsAround :: Ui.M m => Context -> m SelectedAround
ctxEventsAround ctx = do
    (blockId, _, trackIds, _) <- ctxTracks ctx
    blockEnd <- Ui.blockEnd blockId
    extend blockEnd . zip trackIds . map (splitEventsAround sel) <$>
            mapM Ui.getEvents trackIds
    where
    extend blockEnd
        | blockEnd == Sel.max sel = map untilEnd
        | otherwise = id
    sel = ctxSelection ctx
    untilEnd (trackId, (pre, within, post)) =
        (trackId, (pre, within ++ post, []))

splitEventsAround :: Sel.Selection -> Events.Events
    -> ([Event.Event], [Event.Event], [Event.Event])
splitEventsAround sel events
    | null within = selectNeighbor sel (pre, post)
    | otherwise = (pre, within, post)
    where
    (pre, within, post) =
        (Events.descending pre, Events.ascending within, Events.ascending post)
        where
        (pre, within, post) =
            Events.splitRange (Events.selectionRange sel) events

selectNeighbor :: Sel.Selection -> ([Event.Event], [Event.Event])
    -> ([Event.Event], [Event.Event], [Event.Event])
selectNeighbor sel pair = case pair of
    -- Always pick an overlapping event.
    (pre:pres, posts) | Event.overlaps p pre -> (pres, [pre], posts)
    (pres, post:posts) | Event.overlaps p post -> (pres, [post], posts)
    --  |--->    <---| => take based on orientation
    (pre:pres, post:posts)
        | Event.isPositive pre && Event.isNegative post -> case orient of
            Types.Positive -> (pres, [pre], post:posts)
            Types.Negative -> (pre:pres, [post], posts)
    --  |--->          => take pre
    (pre:pres, posts) | Event.isPositive pre -> (pres, [pre], posts)
    --           <---| => take post
    (pres, post:posts) | Event.isNegative post -> (pres, [post], posts)
    --  <---|    |---> => in the middle, do nothing
    (pres, posts) -> (pres, [], posts)
    where
    p = selPoint sel
    orient = Sel.eventOrientation sel

-- | This is similar to 'events_around', except that the direction is reversed:
-- it favors the next positive event, or the previous negative one.  Also this
-- assumes a point selection and only selects one event per track.
oppositeNeighbor :: Cmd.M m => m [(TrackId, Event.Event)]
oppositeNeighbor = do
    tids <- trackIds
    sel <- get
    events <- forM tids $ \trackId ->
        selectOppositeNeighbor (selPoint sel) (Sel.eventOrientation sel) <$>
            Ui.getEvents trackId
    pure [(trackId, event) | (trackId, Just event) <- zip tids events]

selectOppositeNeighbor :: ScoreTime -> Types.Orientation -> Events.Events
    -> Maybe Event.Event
selectOppositeNeighbor pos orient events =
    case Events.splitLists pos events of
        -- TODO should select overlapping like before
        -- except this messes up zero dur
        -- How did this work before?  It didn't!
        -- (pre:_, _) | Event.overlaps pos pre -> Just pre
        -- (_, post:_) | Event.overlaps pos post -> Just post
        (pre:_, post:_)
            | Event.isPositive post && Event.isNegative pre -> case orient of
                Types.Positive -> Just post
                Types.Negative -> Just pre
        (_, post:_) | Event.isPositive post -> Just post
        (pre:_, _) | Event.isNegative pre -> Just pre
        _ -> Nothing

-- ** select tracks

-- | Per-track selection info.
--
-- The TrackNums are sorted, and the TrackIds are likewise in left-to-right
-- order.  Only TrackNums for event tracks are returned, so both lists should
-- have the same length and correspond if you zip them up.
type Tracks = (BlockId, [TrackNum], [TrackId], Events.Range)

-- | Get selected event tracks along with the selection.  The tracks are
-- returned in ascending order.  Only event tracks are returned, and tracks
-- merged into the selected tracks are included.
tracks :: Cmd.M m => m Tracks
tracks = ctxTracks =<< context

-- | Just the TrackIds part of 'tracks'.
trackIds :: Cmd.M m => m [TrackId]
trackIds = do
    (_, _, trackIds, _) <- tracks
    pure trackIds

tracknums :: Cmd.M m => m (BlockId, [TrackNum])
tracknums = do
    (blockId, tracknums, _, _) <- tracks
    pure (blockId, tracknums)

eventTrack :: Cmd.M m => m (BlockId, TrackId)
eventTrack = ctxEventTrack =<< context

ctxEventTrack :: Ui.M m => Context -> m (BlockId, TrackId)
ctxEventTrack ctx = do
    (blockId, mbTrackId) <- ctxTrack ctx
    trackId <- Ui.require "must select an event track" mbTrackId
    pure (blockId, trackId)

track :: Cmd.M m => m (BlockId, Maybe TrackId)
track = ctxTrack =<< context

ctxTrack :: Ui.M m => Context -> m (BlockId, Maybe TrackId)
ctxTrack ctx = do
    let blockId = ctxBlockId ctx
    maybeTrackId <-
        Ui.eventTrackAt blockId (selPointTrack (ctxSelection ctx))
    pure (blockId, maybeTrackId)

-- | Selected tracks, including merged tracks.
ctxTracks :: Ui.M m => Context -> m Tracks
ctxTracks ctx = do
    (blockId, tracknums, trackIds, range) <- ctxStrictTracks ctx
    tracks <- mapM (Ui.getBlockTrackAt blockId) tracknums
    let mergedTrackIds = mconcatMap Block.trackMerged tracks
    block <- Ui.getBlock blockId
    let merged = tracknumsOf block (Set.toList mergedTrackIds)
    let (allTracknums, allTrackIds) = unzip $ Lists.uniqueSort $
            merged ++ zip tracknums trackIds
    pure (blockId, allTracknums, allTrackIds, range)

-- | Selected tracks, not including merged tracks.
ctxStrictTracks :: Ui.M m => Context -> m Tracks
ctxStrictTracks ctx = do
    let blockId = ctxBlockId ctx
    tracks <- Ui.trackCount blockId
    let tracknums = Sel.tracknums tracks (ctxSelection ctx)
    tracklikes <- mapM (Ui.trackAt blockId) tracknums
    (tracknums, trackIds) <- pure $ unzip
        [ (i, trackId)
        | (i, Just (Block.TId trackId _)) <- zip tracknums tracklikes
        ]
    pure
        ( blockId, tracknums, trackIds
        , Events.selectionRange (ctxSelection ctx)
        )

tracknumsOf :: Block.Block -> [TrackId] -> [(TrackNum, TrackId)]
tracknumsOf block trackIds = do
    (tracknum, Block.TId tid _) <-
        zip [0..] (Block.blockTracklikeIds block)
    guard (tid `elem` trackIds)
    pure (tracknum, tid)

-- * history

-- | Keep only this many selection history entries.  This is hardcoded and low
-- since I'm probably not interested in ancient selections.
keepHistory :: Int
keepHistory = 10

recordHistory :: Cmd.M m => m ()
recordHistory =
    whenJustM lookupContext (modifyHistory . record . ctxViewSelection)
    where
    -- Only record the current position if it has changed.
    record viewSel hist
        | take 1 (Cmd.selPast hist) /= [viewSel] = Cmd.SelectionHistory
            { selPast = take keepHistory $ viewSel : Cmd.selPast hist
            , selFuture = []
            }
        | otherwise = hist

previousSelection :: Cmd.M m => Bool -> m ()
previousSelection changeViews = do
    oldViewId <- Cmd.getFocusedView
    cur <- fmap ctxViewSelection <$> lookupContext
    hist <- Cmd.gets Cmd.stateSelectionHistory
    case dropWhile ((==cur) . Just) $ Cmd.selPast hist of
        (viewId, sel) : past | changeViews || viewId == oldViewId -> do
            modifyHistory $ \hist -> hist
                { Cmd.selPast = past
                , Cmd.selFuture = maybe id (:) cur (Cmd.selFuture hist)
                }
            setWithoutHistory viewId (Just sel)
            when (viewId /= oldViewId) $ Cmd.focus viewId
        _ -> pure ()

nextSelection :: Cmd.M m => Bool -> m ()
nextSelection changeViews = do
    oldViewId <- Cmd.getFocusedView
    cur <- fmap ctxViewSelection <$> lookupContext
    hist <- Cmd.gets Cmd.stateSelectionHistory
    case dropWhile ((==cur) . Just) $ Cmd.selFuture hist of
        (viewId, sel) : future | changeViews || viewId == oldViewId -> do
            modifyHistory $ \hist -> hist
                { Cmd.selPast = maybe id (:) cur (Cmd.selPast hist)
                , Cmd.selFuture = future
                }
            setWithoutHistory viewId (Just sel)
            when (viewId /= oldViewId) $ Cmd.focus viewId
        _ -> pure ()

modifyHistory :: Cmd.M m => (Cmd.SelectionHistory -> Cmd.SelectionHistory)
    -> m ()
modifyHistory f = Cmd.modify $ \state -> state
    { Cmd.stateSelectionHistory = f (Cmd.stateSelectionHistory state) }
