-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support to add or remove time in a score, and have it propagate up to
-- callers.
module Cmd.BlockResize (
    update_callers_rulers, update_callers, push_down_rulers
) where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Maps as Maps
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Ruler.Extract as Extract
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.Modify as Modify
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import qualified Derive.ParseTitle as ParseTitle
import Global
import Types


update_callers_rulers :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m [BlockId]
update_callers_rulers block_id pos delta = do
    updates <- update_callers block_id pos delta
    update_rulers block_id pos delta (concatMap bottoms updates)
    return $ map fst $ concatMap Foldable.toList updates

-- | The block has changed size by adding or removing time at the given point.
-- Find its callers and update the event durations.  This doesn't update any
-- rulers, so call either 'push_down_rulers' or 'update_callers_rulers'.
update_callers :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m [Tree.Tree Update]
update_callers block_id pos delta = do
    modify_time block_id pos delta
    updates <- caller_updates [pos] block_id
    apply_updates delta (concatMap Foldable.toList updates)
    return updates

push_down_rulers :: Ui.M m => [Tree.Tree Update] -> m ()
push_down_rulers updates =
    mapM_ push_down_ruler $ Seq.unique $ map fst $ concatMap bottoms updates

modify_time :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m ()
modify_time block_id pos delta = do
    track_ids <- Ui.track_ids_of block_id
    forM_ track_ids $ \track_id ->
        Ui.modify_events track_id $ move_events pos delta . remove
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
apply_updates :: Ui.M m => TrackTime -> [Update] -> m ()
apply_updates delta = mapM_ apply . Map.toList . merge_updates delta
    where
    apply (block_id, tracks) =
        forM_ (Map.toList tracks) $ \(track_id, event_deltas) ->
            forM_ (reverse event_deltas) $ \(event, delta) ->
                update block_id track_id event delta
    update block_id track_id event delta = do
        Ui.remove_event track_id event
        move_events_children block_id track_id (Event.start event) delta
        let dur = Event.duration event + delta
        when (Event.is_negative event) $
            Ui.throw $ "negative events not supported yet: " <> pretty event
        when (Event.is_negative event && dur > 0 || dur < 0) $
            Ui.throw $ "update delta " <> pretty delta
                <> " would invert event: " <> pretty event
        Ui.insert_event track_id $ Event.duration_ #= dur $ event

move_events_children :: Ui.M m => BlockId -> TrackId -> TrackTime -> TrackTime
    -> m ()
move_events_children block_id track_id start delta = do
    children <- map Ui.track_id <$> TrackTree.get_children_of block_id track_id
    forM_ (track_id : children) $ \track_id ->
        Ui.modify_events track_id $ move_events start delta

move_events :: TrackTime -> TrackTime -> Events.Events -> Events.Events
move_events pos delta events = pre <> Events.move delta post
    where (pre, post) = Events.split pos events

-- | Merge Updates so the event updates for each track are together and in
-- Event.start order.  TODO if a track appears on multiple blocks it'll get
-- too many updates.
merge_updates :: TrackTime -> [Update]
    -> Map BlockId (Map TrackId [(Event.Event, TrackTime)])
merge_updates delta =
    fmap (fmap merge . Maps.multimap) . Maps.multimap
    where
    merge offset_events = Seq.sort_on (Event.start . fst)
        [ (event, delta * fromIntegral (length offsets))
        | (offsets, events) <- offset_events, event <- events
        ]


-- * caller_updates

-- | The track times are splice point offsets, relative to the Event.starts.
type Update = (BlockId, (TrackId, ([TrackTime], [Event.Event])))

-- | How much to delete or insert, and where.
caller_updates :: Ui.M m => [TrackTime] -> BlockId -> m [Tree.Tree Update]
    -- ^ This tree is upside-down, leaves are the top-level callers.
    --
    -- There will be a branch for each call, so the same block can appear
    -- in multiple branches.  I could merge them, but maybe it's not necessary.
caller_updates = get_callers
    where
    get_callers offsets callee =
        concatMapM (updates_of offsets) =<< callers_of callee
    -- TODO ignore calls with clip and Clip... use CallDuration?
    -- maybe I should have a thing that gets CallDuration of all block calls,
    -- and then it can update those.  I'll need it anyway if I want a 1:1
    -- highlight.
    updates_of offsets (block_id, tracks) = forM tracks $ \(track_id, events) ->
        Tree.Node (block_id, (track_id, (offsets, events))) <$>
            get_callers
                [Event.start e + offset | offset <- offsets, e <- events]
                block_id

-- | All of the events that directly call the given BlockId.
--
-- TODO This just looks at syntax, but I should be able to get a more canonical
-- parent->child tree by looking in TrackWarp.
callers_of :: Ui.M m => BlockId -> m [(BlockId, [(TrackId, [Event.Event])])]
callers_of callee = strip <$> do
    block_tracks <- Ui.all_block_track_ids
    forM block_tracks $ \(block_id, track_ids) -> do
        track_ids <- filterM is_note_track track_ids
        fmap (block_id,) $ forM track_ids $ \track_id -> do
            calls <- NoteTrack.track_block_calls False block_id track_id
            return (track_id,
                [event | (event, call :| _) <- calls, call == callee])
    where
    strip = filter (not . null . snd) . map (second (filter (not . null . snd)))

is_note_track :: Ui.M m => TrackId -> m Bool
is_note_track = fmap ParseTitle.is_note_track . Ui.get_track_title


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
update_rulers :: Ui.M m => BlockId -> TrackTime -> TrackTime -> [Update] -> m ()
update_rulers block_id pos delta top_updates = do
    meter <- if delta > 0 then extract_meter block_id pos delta
        else return []
    let msg = "ruler modification is ambiguous due to multiple updated tracks: "
            <> pretty top_updates
    inserts <- Ui.require msg $ insert_points top_updates
    forM_ (Map.toList inserts) $ \(top_block_id, offsets) -> do
        RulerUtil.local_block top_block_id $ Modify.meter $
            if delta > 0 then splice meter offsets
                else delete (-delta) offsets
        push_down_ruler top_block_id

push_down_ruler :: Ui.M m => BlockId -> m ()
push_down_ruler block_id = do
    track_id <- Ui.require ("no note track: " <> pretty block_id)
        . Seq.head =<< filterM is_note_track
        =<< Ui.track_ids_of block_id
    Extract.push_down_recursive False block_id track_id

insert_points :: [Update] -> Maybe (Map BlockId [TrackTime])
insert_points = traverse check . Maps.multimap
    where
    check [(_, (offsets, events))] =
        Just [offset + Event.start e | offset <- offsets, e <- events]
    check _ = Nothing

splice :: Meter.LabeledMeter -> [TrackTime] -> Meter.LabeledMeter
    -> Meter.LabeledMeter
splice fragment offsets = go (List.sort offsets) . Meter.with_starts
    where
    go [] meter = map snd meter
    go (t:ts) meter = map snd pre <> fragment <> go ts post
        where (pre, post) = span ((<t) . fst) meter

delete :: TrackTime -> [TrackTime] -> Meter.LabeledMeter -> Meter.LabeledMeter
delete delta offsets = go (List.sort offsets) . Meter.with_starts
    where
    go [] meter = map snd meter
    go (t:ts) meter = map snd pre <> go ts (dropWhile ((< t+delta) . fst) post)
        where (pre, post) = span ((<t) . fst) meter

extract_meter :: Ui.M m => BlockId -> TrackTime -> TrackTime
    -> m Meter.LabeledMeter
extract_meter block_id pos delta = do
    ruler <- Ui.get_ruler =<< Ui.ruler_of block_id
    return $ Meter.extract' pos (pos + delta) $ Meter.marklist_labeled $
        snd $ Ruler.get_meter ruler
