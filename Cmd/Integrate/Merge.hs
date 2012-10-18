{-# LANGUAGE CPP, OverloadedStrings #-}
{- | Merge integrated tracks into existing, possibly hand-edited tracks, using
    the index of the previous integration to figure out which edits were made.

    This proceeds it two steps: first the tracks are matched up.  This takes
    advantage of the two-level (note, controls) hierarchy emitted by
    "Cmd.Integrate.Convert", since each control track is uniquely identified by
    its title, it's safe to just match them up by title.

    However, there's no key to match up the note tracks themselves, so it's
    done purely based on the order of the tracks.  So if the integrate source
    emits more simultaneous notes and Convert puts them on appended tracks all
    will be well, but if it prepends a new track then the later tracks won't
    line up with the index.  This will result in bogus diffs, or just events
    not being found at all and being considered hand-added.

    TODO I'll have to see from experience if this is a problem, and if so, how
    it can be fixed.

    Once tracks are matched, the events are diffed based on the
    'Event.IndexKey'.
-}
module Cmd.Integrate.Merge (
    -- * create
    create_block
    -- * merge
    , merge_block, merge_tracks
    , Edit(..), Modify(..), is_modified
    -- * diff
    , diff_events
#ifdef TESTING
    , make_index
    , diff, diff_event, apply
#endif
) where
import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified App.Config as Config
import Types


-- * block

create_block :: (State.M m) => BlockId -> Convert.Tracks
    -> m (BlockId, [Block.TrackDestination])
create_block source_block_id tracks = do
    ruler_id <- State.block_ruler source_block_id
    block_id <- Create.block ruler_id
    (,) block_id <$> merge_block block_id tracks []

merge_block :: (State.M m) => BlockId -> Convert.Tracks
    -> [Block.TrackDestination] -> m [Block.TrackDestination]
merge_block = merge_tracks


-- * tracks

merge_tracks :: (State.M m) => BlockId -> Convert.Tracks
    -> [Block.TrackDestination] -> m [Block.TrackDestination]
merge_tracks block_id tracks dests = do
    track_ids <- all_block_tracks block_id
    new_dests <- mapMaybeM (merge_pairs block_id)
        (pair_tracks track_ids tracks dests)
    set_skeleton block_id new_dests
    return new_dests

all_block_tracks :: (State.M m) => BlockId -> m [Maybe TrackId]
all_block_tracks block_id =
    map (Block.track_id_of . Block.tracklike_id) . Block.block_tracks <$>
        State.get_block block_id

-- | Merge together TrackPairs, modifying the underlying tracks, and return
-- a TrackDestination.  The head of the TrackPairs is assumed to be the note
-- track, and the rest are its controls.
merge_pairs :: (State.M m) => BlockId -> [TrackPair]
    -> m (Maybe Block.TrackDestination)
merge_pairs block_id pairs = do
    triples <- mapMaybeM (merge_pair block_id) pairs
    return $ case triples of
        [] -> Nothing
        (_, note_id, note_index) : controls ->
            Just $ Block.TrackDestination (note_id, note_index)
                (Map.fromList [(title, (track_id, index))
                    | (title, track_id, index) <- controls])

merge_pair :: (State.M m) => BlockId
    -> TrackPair -> m (Maybe (String, TrackId, Block.EventIndex))
merge_pair block_id pair = case pair of
    (Nothing, Left _) -> return Nothing -- not reached
    (Just (Convert.Track title events), Left tracknum) -> do
        -- Track was deleted or never existed.
        track_id <- Create.track block_id tracknum title
            (Events.from_list (map unmodified events))
        return $ Just (title, track_id, make_index events)
    (Nothing, Right (track_id, _)) -> do
        -- Integrate no longer wants the track.
        clear_generated_events track_id
        return Nothing
    (Just track@(Convert.Track title events), Right dest) -> do
        merge_track track dest
        return $ Just (title, fst dest, make_index events)

-- | Match up new tracks and integrated tracks so I know who to diff against
-- whom.
--
-- Note tracks are simply zipped up, so if a note track is added at the
-- beginning it will look like everything change and the diff won't work
-- correctly.  But control tracks are matched based on name, so they should be
-- robust against controls appearing or disappearing.
--
-- Also figure out TrackNums for index tracks that don't exist.  An index track
-- can not exist because it was never there, or because it was index but is no
-- longer in the block (presumably manually deleted).
--
-- TrackNums are assigned increasing from the previous track that was present,
-- or at the end of the block if no tracks are present.  This way new control
-- tracks should be added adjacent to their sisters, and the first integrate
-- will append the generated tracks to the end of the block.
pair_tracks :: [Maybe TrackId] -- ^ tracks in the block, in tracknum order
    -> Convert.Tracks -> [Block.TrackDestination] -> [[TrackPair]]
pair_tracks track_ids tracks dests = map (filter is_valid) $
    snd $ List.mapAccumL resolve1 (length track_ids) $ map pairs_of $
        Seq.padded_zip tracks dests
    where
    -- Pair up the tracks.
    pairs_of (Seq.First (note, controls)) = map Seq.First (note : controls)
    pairs_of (Seq.Second (Block.TrackDestination note controls)) =
        map Seq.Second (note : Map.elems controls)
    pairs_of (Seq.Both (note, controls)
            (Block.TrackDestination note_dest control_dests)) =
        Seq.Both note note_dest : pair_controls controls control_dests
    pair_controls tracks dests =
        map snd $ Seq.pair_sorted keyed_tracks (Map.toAscList dests)
        where
        -- TODO pair_sorted only works if tracks is sorted, it's easier to
        -- prove that if it's a Map instead of [Track]
        keyed_tracks = Seq.sort_on fst (Seq.key_on Convert.track_title tracks)

    resolve1 next_tracknum pairs = List.mapAccumL resolve next_tracknum pairs
    -- Figure out tracknums.
    resolve next_tracknum (Seq.First track) =
        (next_tracknum + 1, (Just track, Left next_tracknum))
    resolve next_tracknum (Seq.Second dest) = case tracknum_of (fst dest) of
        -- Track deleted and the integrate no longer wants it.
        -- Ugly, but (Nothing, Left) can be code for "ignore me".
        Nothing -> (next_tracknum, (Nothing, Left 0))
        Just tracknum -> (tracknum + 1, (Nothing, Right dest))
    resolve next_tracknum (Seq.Both track dest) = case tracknum_of (fst dest) of
        Nothing -> (next_tracknum + 1, (Just track, Left next_tracknum))
        Just tracknum -> (tracknum + 1, (Just track, Right dest))
    tracknum_of track_id = List.findIndex (== Just track_id) track_ids
    is_valid (Nothing, Left _) = False
    is_valid _ = True

type TrackPair = (Maybe Convert.Track, Either TrackNum Dest)
type Dest = (TrackId, Block.EventIndex)

clear_generated_events :: (State.M m) => TrackId -> m ()
clear_generated_events track_id = State.modify_events track_id $
    Events.from_list . filter (Maybe.isNothing . Event.stack) . Events.ascending

merge_track :: (State.M m) => Convert.Track -> Dest -> m ()
merge_track (Convert.Track _ integrated_events) (track_id, index) = do
    old_events <- Events.ascending . Track.track_events <$>
        State.get_track track_id
    let (deletes, edits) = diff_events index old_events
        new_events = apply deletes edits integrated_events
    State.modify_some_events track_id (const new_events)

set_skeleton :: (State.M m) => BlockId -> [Block.TrackDestination] -> m ()
set_skeleton block_id dests = do
    track_ids <- all_block_tracks block_id
    skel <- State.require "integrate somehow created a cyclic skeleton"
        =<< Skeleton.add_edges (track_edges track_ids dests) <$>
            State.get_skeleton block_id
    State.set_skeleton block_id skel

track_edges :: [Maybe TrackId] -> [Block.TrackDestination]
    -> [(TrackNum, TrackNum)]
track_edges track_ids = concatMap edges
    where
    edges (Block.TrackDestination (track_id, _) controls) =
        case tracknum_of track_id of
            Nothing -> []
            Just tracknum ->
                let control_nums = mapMaybe (tracknum_of . fst)
                        (Map.elems controls)
                in zip (tracknum : control_nums) control_nums
    tracknum_of track_id = List.findIndex (== Just track_id) track_ids

-- * reintegrate

-- | Create an index from integrated tracks.  Since they are integrated, they
-- should all have stacks, so events without stacks are discarded.
make_index :: [Event.Event] -> Block.EventIndex
make_index events = Map.fromList
    [(key, event) | (Just key, event) <- Seq.key_on index_key events]

-- ** diff

-- | Find out how to merge new integrated output with user edits by diffing it
-- against the old integrated output.
diff_events :: Block.EventIndex -- ^ results of last integrate
    -> [Event.Event]
    -- ^ current events, which is last integrate plus user edits
    -> (Set.Set Event.IndexKey, [Edit])
    -- ^ set of deleted events, and edited events
diff_events index events = (deletes, edits)
    where
    deletes = Set.difference (Map.keysSet index) $
        Set.fromList (mapMaybe index_key events)
    edits = map (diff index) events

diff :: Block.EventIndex -> Event.Event -> Edit
diff index new = case index_key new of
    Nothing -> Add new
    Just key -> case Map.lookup key index of
        -- Events with a stack but not in the index shouldn't happen, they
        -- indicate that the index is out of sync with the last
        -- integration.  To be safe, they're counted as an add, and the
        -- stack is deleted.  TODO could this multiply events endlessly?
        -- TODO This could be a symptom of tracks not lining up anymore.
        -- I should emit a warning.
        Nothing -> Add (Event.strip_stack new)
        Just old -> Edit key (diff_event old new)

index_key :: Event.Event -> Maybe Event.IndexKey
index_key = fmap Event.stack_key . Event.stack

diff_event :: Event.Event -> Event.Event -> [Modify]
diff_event old new = concat
    [ cmp (Event.start old) (Event.start new) (Position (Event.start new))
    , cmp (Event.duration old) (Event.duration new)
        (Duration (Event.duration new))
    , diff_text (Event.event_bytestring old) (Event.event_bytestring new)
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

data Edit = Add !Event.Event | Edit !Event.IndexKey ![Modify]
    deriving (Eq, Show)

data Modify = Position !ScoreTime | Duration !ScoreTime
    | Set !B.ByteString | Prefix !B.ByteString
    deriving (Eq, Show)

instance Pretty.Pretty Edit where
    format (Add event) =
        Pretty.constructor "Add" [Pretty.format event]
    format (Edit key mods) = Pretty.constructor "Edit"
        [Pretty.format key, Pretty.format mods]

instance Pretty.Pretty Modify where pretty = show

is_modified :: Edit -> Bool
is_modified (Edit _ mods) = not (null mods)
is_modified _ = True

-- ** apply

apply :: Set.Set Event.IndexKey -- ^ events that were deteleted
    -> [Edit] -> [Event.Event] -- ^ results of current integrate
    -> Events.Events
apply deletes adds_edits = make . mapMaybe edit
    where
    -- Adds go afterwards so they can replace coincident events.
    make events = Events.from_list (events ++ adds)
    edit event
        | Event.start event `Set.member` deletes = Nothing
        | Just mods <- Map.lookup (Event.start event) edit_map = if null mods
            then Just (unmodified event)
            else Just (apply_modifications mods event)
        -- A new event from the integrate.
        | otherwise = Just (unmodified event)
    edit_map = Map.fromList edits
    (adds, edits) = Seq.partition_either (map to_either adds_edits)
    to_either (Add event) = Left event
    to_either (Edit key mods) = Right (key, mods)

-- | Unmodified events get a special style to indicate such.
unmodified :: Event.Event -> Event.Event
unmodified = Event.modify_style Config.unmodified_style

apply_modifications :: [Modify] -> Event.Event -> Event.Event
apply_modifications mods event = List.foldl' go event mods
    where
    go event mod = ($event) $ case mod of
        Position p -> Event.move (const p)
        Duration d -> Event.set_duration d
        Set text -> Event.modify_bytestring (const text)
        Prefix text -> Event.modify_bytestring (text<>)
