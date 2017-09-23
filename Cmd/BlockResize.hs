-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support to add or remove time in a score, and have it propagate up to
-- callers.
module Cmd.BlockResize where
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import qualified Cmd.NoteTrack as NoteTrack
import qualified Derive.ParseTitle as ParseTitle
import Global
import Types


-- | Presumably the block has changed size.  Find its callers and update the
-- event durations.  This doesn't update any rulers, so that should be done
-- separately.
update_callers :: Ui.M m => BlockId -> TrackTime -> TrackTime -> m ()
update_callers block_id pos delta = do
    modify_time block_id pos delta
    updates <- caller_updates delta block_id
    apply_updates (concatMap Foldable.toList updates)
    -- update_ruler block_id pos delta (concatMap bottoms updates)

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

{-
-- | If delta is positive, clip out the ruler there and insert it in the
-- topmost parents.  If negative, that amount of ruler has been deleted, so
-- delete the corresponding time in the parents.
--
-- Then call Ruler.Extract.push_down to update intermediate blocks.
-- TODO maybe skip that if there aren't any.
update_ruler :: Ui.M m => BlockId -> TrackTime -> TrackTime -> [Update] -> m ()
update_ruler block_id pos delta top_parents
    | delta > 0 = undefined
    | delta < 0 = mapM_ remove . Map.toList $ merge_updates top_parents
    | otherwise = return ()
    where
    -- Sort from high to low start times.
    -- For this I think I Should put all the evnts togeth.r
    remove (block_id, tracks) =
        forM_ (Map.toList tracks) $ \(track_id, events) -> do
            undefined
    -- TODO now I don't know where in the event to remove the ruler.
    -- I can get it because it's the caller's event start + pos.
    -- I could actually use this on the intermediate blocks, and avoid
    -- push_down.
    -- Well, except I want to renumber measures.  Maybe copy all marks in here,
    -- and then at the top level, do a renumber on all affected blocks.
    -- I still have to go top to bottom for that though.

    -- Ruler.Extract.push_down False block_id track_id
-}

-- To copy the ruler I still have to go child to parent.  So it has to be in
-- order, so Map is out.  I just need to make sure I do all the children before
-- the parent, so it should work if I go breadth first, and then collecting at
-- the last BlockId.
-- The other way is to merge the trees, or figure out how to create the tree
-- without duplicates.  Actually that winds up being the same problem, I think,
-- because I have to wait until I see the last child before I know that it
-- doesn't come from a particular parent.

-- update_rulers :: Ui.M m => TrackTime -> TrackTime
--     -> Map BlockId (Map TrackId [(TrackTime, TrackTime)]) -> m ()
-- update_rulers pos delta top_parents
--     | delta < 0 = mapM_ remove . Map.toList top_parents
--     where
--     remove (block_id, tracks) = do
--         let ranges = Util.ranges,e
--         forM_ (Map.toList tracks) $ \(track_id, ranges) -> do
--             RulerUtil.local_block block_id

-- To insert time: remove the given event, move everything below on the same
-- track down, then reinsert.
--
-- The block duration changes by te sum of the deltas under that BlockId.
apply_updates :: Ui.M m => [Update] -> m ()
apply_updates = mapM_ (mapM_ apply) . map Map.toList . Map.elems . merge_updates
    where
    apply (track_id, event_deltas) =
        forM_ (reverse event_deltas) $ \(event, delta) -> do
            Ui.remove_event track_id event
            Ui.modify_events track_id $ move_events (Event.start event) delta
            let dur = Event.duration event + delta
            when (Event.is_negative event) $
                Ui.throw $ "negative events not supported yet: " <> pretty event
            when (Event.is_negative event && dur > 0 || dur < 0) $
                Ui.throw $ "update delta " <> pretty delta
                    <> " would invert event: " <> pretty event
            Ui.insert_event track_id $ Event.duration_ #= dur $ event

move_events :: TrackTime -> TrackTime -> Events.Events -> Events.Events
move_events start delta events = pre <> Events.move delta post
    where (pre, post) = Events.split start events

-- | Merge Updates so the event updates for each track are together and in
-- Event.start order.  TODO if a track appears on multiple blocks it'll get
-- too many updates.
merge_updates :: [Update]
    -> Map BlockId (Map TrackId [(Event.Event, TrackTime)])
merge_updates =
    fmap (fmap merge . Map.fromListWith (++) . map swap) . Map.fromListWith (++)
    where
    swap (delta, (track_id, events)) = (track_id, map (,delta) events)
    merge = Seq.sort_on (Event.start . fst)


-- | TrackTime is how much time to add to or remove from the event.
type Update = (BlockId, [(TrackTime, (TrackId, [Event.Event]))])

-- | How much to delete or insert, and where.
caller_updates :: Ui.M m => TrackTime -> BlockId -> m [Tree.Tree Update]
    -- ^ This tree is upside-down, leaves are the top-level callers.
    --
    -- There will be a branch for each call, so the same block can appear
    -- in multiple branches.  I could merge them, but maybe it's not necessary.
caller_updates = get_callers
    where
    get_callers delta callee = mapM updates_of
        =<< (mapMaybe (annotate delta) <$> callers_of callee)

    -- TODO ignore calls with clip and Clip... use CallDuration?
    -- maybe I should have a thing that gets CallDuration of all block calls,
    -- and then it can update those.  I'll need it anyway if I want a 1:1
    -- highlight.
    updates_of (block_id, delta, tracks) =
        Tree.Node (block_id, map (delta,) tracks) <$>
            get_callers (delta * fromIntegral track_count) block_id
        where track_count = maximum $ 0 : map (length . snd) tracks

    annotate delta (block_id, tracks) = Just (block_id, delta, tracks)

    -- -- The calling block changes by delta for each affected event in it.
    -- -- Actually, it's the maximum of the sum of the ones on the same track.
    -- annotate delta (block_id, tracks)
    --     | track_deltas == 0 = Nothing
    --     | otherwise = Just (block_id, delta * fromIntegral track_deltas, tracks)
    --     where track_deltas = maximum $ 0 : map (length . snd) tracks

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

block_calls :: Ui.M m => BlockId -> TrackId -> m [(Event.Event, BlockId)]
block_calls block_id track_id = map (second NonEmpty.head) <$>
    NoteTrack.track_block_calls False block_id track_id
