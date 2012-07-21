{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Support for converting Tracks into UI blocks.
module Cmd.Integrate.Merge (
    create, merge, make_index
    , get_block_events
    , Edit(..), Modify(..), is_modified
    -- * diff
    , diff_events
#ifdef TESTING
    , fill_block, diff, diff_event, apply
#endif
) where
import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert

import qualified Derive.TrackInfo as TrackInfo
import qualified App.Config as Config
import Types


create :: (State.M m) => BlockId -> [Convert.Track] -> m BlockId
create source_block_id tracks = do
    ruler_id <- State.get_block_ruler source_block_id
    block_id <- Create.block ruler_id
    fill_block block_id tracks
    return block_id

fill_block :: (State.M m) => BlockId -> [Convert.Track] -> m ()
fill_block block_id tracks = do
    mapM_ (create_track block_id) (zip [1..] tracks)
    State.set_skeleton block_id $ Skeleton.make $
        -- +1 to account for the ruler track.
        make_edges (length tracks + 1) note_tracks
    where
    create_track block_id (tracknum, (Convert.Track title events)) = do
        Create.track block_id tracknum title (Events.from_asc_list events)
    note_tracks =
        [tracknum | (tracknum, Convert.Track title _) <- zip [1..] tracks,
            TrackInfo.is_note_track title]

-- | 6 [1, 4] -> [(1, 2), (2, 3), (4, 5)]
make_edges :: TrackNum -> [TrackNum] -> [(TrackNum, TrackNum)]
make_edges track_count = concatMap interpolate . Seq.zip_next
    where
    interpolate (t1, maybe_t2) = zip ts (drop 1 ts)
        where ts = [t1 .. Maybe.fromMaybe track_count maybe_t2 - 1]

-- * merge

-- Merge cases:
-- - hasn't been edited, should be replaced
-- Can tell becasue old derive still has the same events.
--
-- - moved
--
-- Stash the last integrate results in the block.  Then when I get
-- a DeriveComplete, compare the last integrate with the current contents and
-- come up with a list of edits.  Then reintegrate and apply the edits to the
-- new score.
--
-- I can't detect when the source event moves, but I think it's ok to treat
-- that as a delete + new.
--
-- It's easier if integrate only produces one track, but it seems too useful to
-- produce multiple ones.  But then I have to make sure that events will remain
-- on the same track.  Diff could also search multiple tracks.
-- For each new event: search for existing event.
merge :: (Cmd.M m) => [Convert.Track] -> (BlockId, Block.EventIndex) -> m ()
merge tracks (block_id, index) = do
    -- TODO reintegrate uses tracknums
    -- I think reintegrate will get confused if you move tracks, or if
    -- integrate decides to emit an extra track.  So I need a key to link
    -- the integrate generated tracks with the output tracks.
    -- TODO also if the integrate emits fewer tracks they stick around
    current_events <- get_block_events block_id
    let (deletes, edits) = diff_events index current_events
        new_events = apply deletes edits (integrated_events tracks)
    forM_ new_events $ \(tracknum, events) -> do
        track_id <- State.get_event_track_at "Integrate.merge" block_id tracknum
        -- TODO only emit damage for the changed parts
        State.modify_events track_id (const events)

get_block_events :: (State.M m) => BlockId -> m [(TrackNum, Events.PosEvent)]
get_block_events block_id = do
    (tracknums, track_ids) <- unzip <$> block_tracks block_id
    tracks <- mapM State.get_track track_ids
    return [(tracknum, event) | (tracknum, track) <- zip tracknums tracks,
        event <- Events.ascending (Track.track_events track)]

block_tracks :: (State.M m) => BlockId -> m [(TrackNum, TrackId)]
block_tracks block_id = do
    tracks <- Block.block_tracks <$> State.get_block block_id
    return [(tracknum, track_id)
        | (tracknum, Just track_id) <- zip [0..] (map track_of tracks)]
    where track_of = Block.track_id_of . Block.tracklike_id

-- * reintegrate

-- | Create an index from integrated tracks.  Since they are integrated, they
-- should all have stacks, so events without stacks are discarded.
make_index :: [Events.PosEvent] -> Block.EventIndex
make_index events = Map.fromList
    [(stack, event) | (Just stack, event) <- zip (map stack_of events) events]
    where stack_of = Event.event_stack . snd

integrated_events :: [Convert.Track]
    -> [(Event.Stack, TrackNum, Events.PosEvent)]
integrated_events tracks =
    [ (stack, tracknum, event)
    | (tracknum, track) <- zip [1..] tracks
    , event <- Convert.track_events track
    , Just stack <- [Event.event_stack (snd event)]
    ]

-- -- | Like 'make_index', but partition out the events without stacks.
-- extract_index :: [(TrackNum, Events.PosEvent)]
--     -> (Block.EventIndex, [(TrackNum, Events.PosEvent)])
-- extract_index events = (index, map fst without_stacks)
--     where
--     -- This is a real bothersome bit of list fiddling.  Isn't there a more
--     -- graceful way?
--     index = Map.fromList
--         [(stack, event) | (stack, ((_, event), _)) <- with_stacks]
--     (with_stacks, without_stacks) = partition_maybe snd
--         (zip events (map (Event.event_stack . snd . snd) events))

-- ** diff

-- | Find out how to merge new integrated output with user edits by diffing it
-- against the old integrated output.
diff_events :: Block.EventIndex -- ^ results of last integrate
    -> [(TrackNum, Events.PosEvent)]
    -- ^ current events, which is last integrate plus user edits
    -> (Set.Set Event.Stack, [Edit])
    -- ^ set of deleted events, and edited events
diff_events index events = (deletes, edits)
    where
    deletes = Set.difference (Map.keysSet index) $
        Set.fromList (Maybe.mapMaybe (Event.event_stack . snd . snd) events)
    edits = map (diff index) events

diff :: Block.EventIndex -> (TrackNum, Events.PosEvent) -> Edit
diff index (tracknum, new) = case Event.event_stack (snd new) of
    Nothing -> Add tracknum new
    Just stack -> case Map.lookup stack index of
        -- Events with a stack but not in the index shouldn't happen, they
        -- indicate that the index is out of sync with the last
        -- integration.  To be safe, they're counted as an add, and the
        -- stack is deleted.  TODO could this multiply events endlessly?
        Nothing -> Add tracknum (clear_stack new)
        Just old -> Edit stack tracknum (diff_event old new)
    where clear_stack (p, e) = (p, e { Event.event_stack = Nothing })

diff_event :: Events.PosEvent -> Events.PosEvent -> [Modify]
diff_event (old_pos, old_event) (new_pos, new_event) = concat
    [ cmp old_pos new_pos (Position new_pos)
    , cmp (Event.event_duration old_event) (Event.event_duration new_event)
        (Duration (Event.event_duration new_event))
    , diff_text (Event.event_bs old_event) (Event.event_bs new_event)
    ]
    where cmp x y val = if x == y then [] else [val]

-- | Figure out differences between the text of two events.
--
-- A text change is only considered a Prefix if it occurs on a @ | @ boundary.
-- This is because I want to catch a transformer addition but don't want to
-- mangle text that happens to start with the same character.
--
-- I don't check for suffixes because suffixing an event would change
-- a generator to a transformer, which in unlikely.
diff_text :: Event.Text -> Event.Text -> [Modify]
diff_text old new
    | old == new = []
    | old `B.isSuffixOf` new && ends_with_pipe prefix = [Prefix prefix]
    | otherwise = [Set new]
    where
    prefix = B.take (B.length new - B.length old) new
    ends_with_pipe text = "|" `B.isSuffixOf` pre && B.all (==' ') post
        where (pre, post) = B.breakEnd (=='|') text

data Edit =
    Add !TrackNum !Events.PosEvent
    | Edit !Event.Stack !TrackNum ![Modify]
    deriving (Eq, Show)

data Modify = Position !ScoreTime | Duration !ScoreTime
    | Set !B.ByteString | Prefix !B.ByteString
    deriving (Eq, Show)

instance Pretty.Pretty Edit where
    format (Add tracknum event) =
        Pretty.constructor "Add" [Pretty.format tracknum, Pretty.format event]
    format (Edit stack tracknum mods) = Pretty.constructor "Edit"
        [Pretty.format stack, Pretty.format tracknum, Pretty.format mods]

instance Pretty.Pretty Modify where pretty = show

is_modified :: Edit -> Bool
is_modified (Edit _ _ mods) = not (null mods)
is_modified _ = True

-- ** apply

apply :: Set.Set Event.Stack -- ^ events that were deteleted
    -> [Edit]
    -> [(Event.Stack, TrackNum, Events.PosEvent)]
    -- ^ results of current integrate, has stacks
    -> [(TrackNum, Events.Events)]
apply deletes adds_edits = make . Maybe.mapMaybe edit
    where
    make :: [(TrackNum, Events.PosEvent)] -> [(TrackNum, Events.Events)]
    make events = map group $
        collect_pairs fst (Seq.sort_on fst events) (Seq.sort_on fst adds)
    group (tracknum, events, adds) = (tracknum,
        Events.from_list (map snd adds) <> Events.from_list (map snd events))
    edit (stack, tracknum, event)
        | stack `Set.member` deletes = Nothing
        | Just (edit_tracknum, mods) <- Map.lookup stack edit_map =
            if null mods
                then Just (edit_tracknum, unmodified event)
                else Just (edit_tracknum, apply_modifications mods event)
        -- A new event from the integrate.
        | otherwise = Just (tracknum, unmodified event)
    edit_map = Map.fromList
        [(stack, (tracknum, mods)) | (stack, tracknum, mods) <- edits]
    (adds, edits) = Seq.partition_either (map to_either adds_edits)
    to_either (Add tracknum event) = Left (tracknum, event)
    to_either (Edit stack tracknum mods) = Right (stack, tracknum, mods)

-- | Unmodified events get a special style to indicate such.
unmodified :: Events.PosEvent -> Events.PosEvent
unmodified = second $ \event -> event { Event.event_style =
    Config.unmodified_style (Event.event_style event) }

apply_modifications :: [Modify] -> Events.PosEvent -> Events.PosEvent
apply_modifications mods event = List.foldl' go event mods
    where
    go (pos, event) mod = case mod of
        Position p -> (p, event)
        Duration d -> (pos, Event.set_duration d event)
        Set text -> (pos, event { Event.event_bs = text })
        Prefix text ->
            (pos, event { Event.event_bs = text <> Event.event_bs event })

-- * util

-- partition_maybe :: (a -> Maybe k) -> [a] -> ([(k, a)], [a])
-- partition_maybe _ [] = ([], [])
-- partition_maybe f (x:xs) = case f x of
--     Just k -> ((k, x) : js, ns)
--     Nothing -> (js, x:ns)
--     where (js, ns) = partition_maybe f xs

collect_pairs :: (Ord k) => (a -> k) -> [a] -> [a] -> [(k, [a], [a])]
collect_pairs key xs ys = map to_list $ Map.pairs
        (Map.multimap (Seq.key_on key xs)) (Map.multimap (Seq.key_on key ys))
    where to_list (k, x, y) = (k, Maybe.fromMaybe [] x, Maybe.fromMaybe [] y)
