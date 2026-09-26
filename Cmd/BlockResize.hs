-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support to add or remove time in a score, and have it propagate up to
-- callers.
module Cmd.BlockResize (
    updateCallersRulers
    , updateCallers, pushDownRulers
) where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps

import qualified Cmd.NoteTrackParse as NoteTrackParse
import qualified Cmd.Ruler.Extract as Extract
-- import qualified Cmd.Ruler.Modify as Modify
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import qualified Derive.ParseTitle as ParseTitle
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Meter.Meter as Meter
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

import           Global
import           Types


updateCallersRulers :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m [BlockId]
updateCallersRulers blockId pos delta = do
    updates <- updateCallers blockId pos delta
    updateRulers blockId pos delta (concatMap bottoms updates)
    pure $ map fst $ concatMap Foldable.toList updates

-- | The block has changed size by adding or removing time at the given point.
-- Find its callers and update the event durations.  This doesn't update any
-- rulers, so call either 'push_down_rulers' or 'update_callers_rulers'.
updateCallers :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m [Tree.Tree Update]
updateCallers blockId pos delta = do
    modifyTime blockId pos delta
    updates <- callerUpdates [pos] blockId
    applyUpdates delta (concatMap Foldable.toList updates)
    pure updates

pushDownRulers :: Ui.M m => [Tree.Tree Update] -> m ()
pushDownRulers updates =
    mapM_ pushDownRuler $ Lists.unique $ map fst $ concatMap bottoms updates

modifyTime :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m ()
modifyTime blockId pos delta = do
    trackIds <- Ui.trackIdsOf blockId
    forM_ trackIds $ \trackId ->
        Ui.modifyEvents trackId $ moveEvents pos delta . remove
    where
    remove
        | delta < 0 = Events.remove (Events.Range pos (-delta))
        | otherwise = id

bottoms :: Tree.Tree a -> [a]
bottoms (Tree.Node x []) = [x]
bottoms (Tree.Node _ subs) = concatMap bottoms subs


-- * apply_updates

-- | To insert time: remove the given event, move everything below on the same
-- track down, then reinsert.
applyUpdates :: Ui.M m => TrackTime -> [Update] -> m ()
applyUpdates delta = mapM_ apply . Map.toList . mergeUpdates delta
    where
    apply (blockId, tracks) =
        forM_ (Map.toList tracks) $ \(trackId, eventDeltas) ->
            forM_ (reverse eventDeltas) $ \(event, delta) ->
                update blockId trackId event delta
    update blockId trackId event delta = do
        Ui.removeEvent trackId event
        moveEventsChildren blockId trackId (Event.start event) delta
        let dur = Event.duration event + delta
        when (Event.isNegative event) $
            Ui.throw $ "negative events not supported yet: " <> pretty event
        when (Event.isNegative event && dur > 0 || dur < 0) $
            Ui.throw $ "update delta " <> pretty delta
                <> " would invert event: " <> pretty event
        Ui.insertEvent trackId $ Event.duration_ #= dur $ event

moveEventsChildren :: Ui.M m => BlockId -> TrackId -> TrackTime -> TrackTime
    -> m ()
moveEventsChildren blockId trackId start delta = do
    children <- map Ui.trackId <$> TrackTree.getChildrenOf blockId trackId
    forM_ (trackId : children) $ \trackId ->
        Ui.modifyEvents trackId $ moveEvents start delta

moveEvents :: TrackTime -> TrackTime -> Events.Events -> Events.Events
moveEvents pos delta events = pre <> Events.move delta post
    where (pre, post) = Events.split pos events

-- | Merge Updates so the event updates for each track are together and in
-- Event.start order.  TODO if a track appears on multiple blocks it'll get
-- too many updates.
mergeUpdates :: TrackTime -> [Update]
    -> Map BlockId (Map TrackId [(Event.Event, TrackTime)])
mergeUpdates delta =
    fmap (fmap merge . Maps.multimap) . Maps.multimap
    where
    merge offsetEvents = Lists.sortOn (Event.start . fst)
        [ (event, delta * fromIntegral (length offsets))
        | (offsets, events) <- offsetEvents, event <- events
        ]


-- * caller_updates

-- | The track times are splice point offsets, relative to the Event.starts.
type Update = (BlockId, (TrackId, ([TrackTime], [Event.Event])))

-- | How much to delete or insert, and where.
callerUpdates :: Ui.M m => [TrackTime] -> BlockId -> m [Tree.Tree Update]
    -- ^ This tree is upside-down, leaves are the top-level callers.
    --
    -- There will be a branch for each call, so the same block can appear
    -- in multiple branches.  I could merge them, but maybe it's not necessary.
callerUpdates = getCallers
    where
    getCallers offsets callee =
        concatMapM (updatesOf offsets) =<< callersOf callee
    -- TODO ignore calls with clip and Clip... use CallDuration?
    -- maybe I should have a thing that gets CallDuration of all block calls,
    -- and then it can update those.  I'll need it anyway if I want a 1:1
    -- highlight.
    updatesOf offsets (blockId, tracks) = forM tracks $ \(trackId, events) ->
        Tree.Node (blockId, (trackId, (offsets, events))) <$>
            getCallers
                [Event.start e + offset | offset <- offsets, e <- events]
                blockId

-- | All of the events that directly call the given BlockId.
--
-- TODO This just looks at syntax, but I should be able to get a more canonical
-- parent->child tree by looking in TrackWarp.
callersOf :: Ui.M m => BlockId -> m [(BlockId, [(TrackId, [Event.Event])])]
callersOf callee = strip <$> do
    blockTracks <- Ui.allBlockTrackIds
    forM blockTracks $ \(blockId, trackIds) -> do
        trackIds <- filterM isNoteTrack trackIds
        fmap (blockId,) $ forM trackIds $ \trackId -> do
            calls <- NoteTrackParse.trackBlockCalls False blockId trackId
            pure (trackId,
                [event | (event, call :| _) <- calls, call == callee])
    where
    strip = filter (not . null . snd) . map (second (filter (not . null . snd)))

isNoteTrack :: Ui.M m => TrackId -> m Bool
isNoteTrack = fmap ParseTitle.isNoteTrack . Ui.getTrackTitle

-- * update_rulers

{- | For a positive delta, copy the ruler from the callee block to its
    corresponding times in the top blocks, or delete time for a negative delta.
    Then propagate the ruler changes back down with
    'Extract.push_down_recursive'.

    Another approach would be to copy or delete ruler in all the intermediate
    blocks too, which seems like I could then avoid
    'Extract.push_down_recursive',  but I'd still need to renumber if changed
    event durations made the measure count change.
-}
updateRulers :: Ui.M m => BlockId -> TrackTime -> TrackTime -> [Update] -> m ()
updateRulers blockId pos delta topUpdates = do
    fragment <- if delta > 0 then extractMeter blockId pos delta
        else pure []
    let msg = "ruler modification is ambiguous due to multiple updated tracks: "
            <> pretty topUpdates
    inserts <- Ui.require msg $ insertPoints topUpdates
    forM_ (Map.toList inserts) $ \(topBlockId, offsets) -> do
        RulerUtil.localMeter RulerUtil.Block topBlockId $
            Meter.modifySections $ if delta > 0
                then splice fragment offsets
                else delete (-delta) offsets
        pushDownRuler topBlockId

pushDownRuler :: Ui.M m => BlockId -> m ()
pushDownRuler blockId = do
    trackId <- Ui.require ("no note track: " <> pretty blockId) . Lists.head
        =<< filterM isNoteTrack =<< Ui.trackIdsOf blockId
    Extract.pushDownRecursive False blockId trackId

insertPoints :: [Update] -> Maybe (Map BlockId [TrackTime])
insertPoints = traverse check . Maps.multimap
    where
    check [(_, (offsets, events))] =
        Just [offset + Event.start e | offset <- offsets, e <- events]
    check _ = Nothing

splice :: [Meter.MSection] -> [TrackTime] -> [Meter.MSection]
    -> [Meter.MSection]
splice fragment offsets = go (List.sort offsets)
    where
    go [] sections = sections
    go (t:ts) sections = pre <> fragment <> go ts post
        where (pre, post) = Meter.sectionsSplit t sections

delete :: TrackTime -> [TrackTime] -> [Meter.MSection] -> [Meter.MSection]
delete delta offsets = go (List.sort offsets)
    where
    go [] sections = sections
    go (t:ts) sections = pre <> go ts (Meter.sectionsDrop delta post)
        where (pre, post) = Meter.sectionsSplit t sections

extractMeter :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m [Meter.MSection]
extractMeter blockId pos delta = do
    meter <- RulerUtil.getMeter =<< Ui.rulerOf blockId
    pure $ RulerUtil.extract pos (pos + delta) (Meter.meterSections meter)
