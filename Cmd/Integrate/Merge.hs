-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Merge integrated tracks into existing, possibly hand-edited tracks, using
    the index of the previous integration to figure out which edits were made.

    This proceeds in two steps: first the tracks are matched up.  This takes
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
    , MergeTitles(..)
    , merge_block, score_merge_block, merge_tracks
    , score_merge_tracks
    , Edit(..), Modify(..), is_modified
    -- * diff
    , diff_events
#ifdef TESTING
    , make_index
    , diff, diff_event, apply
#endif
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import qualified Data.Tree as Tree

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Trees as Trees

import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Derive.Stack as Stack
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * block

create_block :: Ui.M m => BlockId -> Convert.Tracks
    -> m (BlockId, [Block.NoteDestination])
create_block source_id tracks = do
    ruler_id <- Ui.block_ruler source_id
    dest_id <- Create.block ruler_id
    (,) dest_id <$> merge_block dest_id tracks []

merge_block :: Ui.M m => BlockId -> Convert.Tracks
    -> [Block.NoteDestination] -> m [Block.NoteDestination]
merge_block = merge_tracks KeepTitles

score_merge_block :: Ui.M m => BlockId -> BlockId -> Block.ScoreDestinations
    -> m Block.ScoreDestinations
score_merge_block source_id dest_id dests = do
    tree <- TrackTree.track_tree_of source_id
    score_merge dest_id tree dests

-- * tracks

data MergeTitles =
    KeepTitles -- ^ leave the titles of merged tracks alone
    | ReplaceTitles -- ^ replace titles with the merge source
    deriving (Eq, Show)

-- | Given a set of source 'Convert.Tracks' and a set of previously integrated
-- destination tracks, merge them together and give new destination tracks.
-- A single integrating source track can create multiple Convert.Tracks, and
-- an integrating track can have >=1 destinations, so this is called once per
-- (source, destination) pair.
merge_tracks :: Ui.M m => MergeTitles -> BlockId -> Convert.Tracks
    -> [Block.NoteDestination] -> m [Block.NoteDestination]
merge_tracks merge_titles block_id tracks dests = do
    track_ids <- all_block_tracks block_id
    new_dests <- mapMaybeM (merge_pairs merge_titles block_id) $
        pair_tracks track_ids tracks dests
    -- TODO doesn't this combine with the old skeleton?  Why isn't that
    -- a problem?
    add_derive_skeleton block_id new_dests
    return new_dests

add_derive_skeleton :: Ui.M m => BlockId -> [Block.NoteDestination] -> m ()
add_derive_skeleton block_id dests = do
    track_ids <- all_block_tracks block_id
    skel <- Ui.require "integrate somehow created a cyclic skeleton"
        =<< Skeleton.add_edges (track_edges track_ids dests) <$>
            Ui.get_skeleton block_id
    Ui.set_skeleton block_id skel

track_edges :: [Maybe TrackId] -> [Block.NoteDestination]
    -> [(TrackNum, TrackNum)]
track_edges track_ids = concatMap edges
    where
    edges (Block.NoteDestination _ (track_id, _) controls) =
        case tracknum_of track_id of
            Nothing -> []
            Just tracknum ->
                let control_nums = mapMaybe (tracknum_of . fst)
                        (Map.elems controls)
                in zip (tracknum : control_nums) control_nums
    tracknum_of track_id = List.elemIndex (Just track_id) track_ids

-- | Tracks in tracknum order.  Nothing for non-event tracks, like rulers.
all_block_tracks :: Ui.M m => BlockId -> m [Maybe TrackId]
all_block_tracks block_id =
    map Block.track_id . Block.block_tracks <$> Ui.get_block block_id

-- ** score

-- | Update the given ScoreDestinations from the source block and track.
score_merge_tracks :: Ui.M m => BlockId -> TrackId
    -> Block.ScoreDestinations -> m Block.ScoreDestinations
score_merge_tracks block_id source_id dests = do
    tree <- TrackTree.track_tree_of block_id
    children <- Ui.require ("source track not found: " <> showt source_id) $
        Trees.find ((==source_id) . Ui.track_id) tree
    score_merge block_id [children] dests

score_merge :: Ui.M m => BlockId -> TrackTree.TrackTree
    -> Block.ScoreDestinations -> m Block.ScoreDestinations
score_merge block_id tree dests = do
    remove <- destination_edges block_id (map (fst . snd) dests)
    Ui.modify_skeleton block_id (Skeleton.remove_edges remove)
    track_ids <- all_block_tracks block_id
    tracks <- get_children tree
    dests <- mapMaybeM (score_merge_pair block_id) $
        score_pair_tracks track_ids tracks dests
    add_skeleton block_id =<< source_to_dest block_id dests
        (map (fmap Ui.track_id) tree)
    return dests

-- | Track pairs of the children of the given tree, sorted by tracknum.
get_children :: Ui.M m => TrackTree.TrackTree -> m [(TrackId, Track.Track)]
get_children =
    fmap (map snd . Seq.sort_on fst) . mapM resolve . concatMap Tree.flatten
    where
    resolve tinfo = do
        track <- Ui.get_track (Ui.track_id tinfo)
        return (Ui.track_tracknum tinfo, (Ui.track_id tinfo, track))

source_to_dest :: Ui.M m => BlockId -> Block.ScoreDestinations
    -> [Tree.Tree TrackId] -> m [Tree.Tree TrackNum]
source_to_dest block_id dests = mapM $ Traversable.mapM $ \track_id -> do
    dest_id <- maybe (Ui.throw $ "no destination for " <> showt track_id)
        (return . fst) (lookup track_id dests)
    dest_tracknum block_id dest_id

-- | Get the edges that are part of the destination track structure.  This
-- is so I can clear out the old skeleton before replacing it with the new one.
-- Otherwise, adding a new track gets a mangled skeleton since the old edge
-- remains.  This only returns edges where both ends are in the destination
-- tracks, so if you manually add a non-integrated parent or child it should
-- remain that way.
destination_edges :: Ui.M m => BlockId -> [TrackId] -> m [Skeleton.Edge]
destination_edges block_id track_ids = do
    tracknums <- mapM (dest_tracknum block_id) track_ids
    edges <- Skeleton.flatten <$> Ui.get_skeleton block_id
    return $ filter (\(p, c) -> p `elem` tracknums && c `elem` tracknums) edges

dest_tracknum :: Ui.M m => BlockId -> TrackId -> m TrackNum
dest_tracknum block_id track_id = Ui.require
    ("integrated track " <> showt track_id <> " not in " <> showt block_id)
    =<< Ui.tracknum_of block_id track_id

add_skeleton :: Ui.M m => BlockId -> [Tree.Tree TrackNum] -> m ()
add_skeleton block_id tree = do
    skel <- Ui.require "score integrated somehow created a cyclic skeleton"
        =<< Skeleton.add_edges (Trees.edges tree) <$> Ui.get_skeleton block_id
    Ui.set_skeleton block_id skel

-- ** merge

-- | Merge together TrackPairs, modifying the underlying tracks, and return
-- a NoteDestination.  The head of the TrackPairs is assumed to be the note
-- track, and the rest are its controls.
--
-- Control and pitch tracks are matched or created by title, but the note track
-- title is ignored.
merge_pairs :: Ui.M m => MergeTitles -> BlockId -> [TrackPair]
    -> m (Maybe Block.NoteDestination)
merge_pairs merge_titles block_id pairs = do
    triples <- mapMaybeM (merge_pair block_id) pairs
    case triples of
        [] -> return Nothing
        (source_title, note_id, note_index) : controls -> do
            -- TODO I could merge them, but I need the previous integrated
            -- title
            -- What about the control track titles?  I use those as keys, so I
            -- can't change them without breaking the link.
            when (merge_titles == ReplaceTitles) $ do
                Ui.set_track_title note_id source_title
            return $ Just $ Block.NoteDestination key (note_id, note_index) $
                Map.fromList
                    [ (title, (track_id, index))
                    | (title, track_id, index) <- controls
                    ]
    where
    -- TODO once I use track keys I have to propagate this from the pairing.
    key = ""

merge_pair :: Ui.M m => BlockId -> TrackPair
    -> m (Maybe (Text, TrackId, Block.EventIndex))
merge_pair block_id pair = case pair of
    (Nothing, Left _) -> return Nothing -- not reached
    (Just (Convert.Track title events), Left tracknum) -> do
        -- Track was deleted or it doesn't exist yet, so create it.
        track_id <- Create.track block_id tracknum title
            (Events.from_list (map Event.unmodified events))
        return $ Just (title, track_id, make_index events)
    (Nothing, Right (track_id, _)) -> do
        -- Integrate no longer wants the track.  Don't delete the track in case
        -- there are manually created events on it.
        clear_generated_events track_id
        return Nothing
    (Just (Convert.Track title events), Right dest) -> do
        merge_track events dest
        return $ Just (title, fst dest, make_index events)

score_merge_pair :: Ui.M m => BlockId -> ScoreTrackPair
    -> m (Maybe (TrackId, (TrackId, Block.EventIndex)))
score_merge_pair block_id pair = case pair of
    (Nothing, Left _) -> return Nothing -- not reached
    (Just (source_id, events), Left tracknum) -> do
        -- Track was deleted or never existed.
        let stacked = add_event_stacks block_id source_id events
        title <- Ui.get_track_title source_id
        track_id <- Create.track block_id tracknum
            title (Events.from_list (map Event.unmodified stacked))
        return $ Just (source_id, (track_id, make_index stacked))
    (Nothing, Right (track_id, _)) -> do
        -- Integrate no longer wants the track.  Don't delete the track in case
        -- there are manually created events on it.
        clear_generated_events track_id
        return Nothing
    (Just (source_id, events), Right dest) -> do
        let stacked = add_event_stacks block_id source_id events
        merge_track stacked dest
        return $ Just (source_id, (fst dest, make_index stacked))

clear_generated_events :: Ui.M m => TrackId -> m ()
clear_generated_events track_id = Ui.modify_events track_id $
    Events.from_list . filter (Maybe.isNothing . Event.stack) . Events.ascending

-- | This implements a 3-way merge.  First, diff the recorded index (which
-- is a pristine copy of the previous integrate) against the current contents
-- of the track.  This gives the edits that have been applied manually against
-- the integrate output.  Then those edits are replayed against the new
-- integrate output.
merge_track :: Ui.M m => [Event.Event] -> Dest -> m ()
merge_track source_events (track_id, index) = do
    old_events <- Events.ascending <$> Ui.get_events track_id
    let (deletes, edits) = diff_events index old_events
        new_events = apply deletes edits source_events
    Ui.modify_some_events track_id (const new_events)

-- | Create an index from integrated tracks.  Since they are integrated, they
-- should all have stacks, so events without stacks are discarded.
make_index :: [Event.Event] -> Block.EventIndex
make_index events = Map.fromList
    [(key, event) | (Just key, event) <- Seq.key_on index_key events]

-- | Unlike derive integration, the events are copied directly from the
-- source, and hence don't have stacks.
add_event_stacks :: BlockId -> TrackId -> Events.Events -> [Event.Event]
add_event_stacks block_id track_id = map add_stack . Events.ascending
    where
    add_stack event = Event.stack_ #= Just (make_stack event) $ event
    make_stack event = Event.Stack
        { Event.stack_stack =
            Stack.from_innermost [Stack.Track track_id, Stack.Block block_id]
        , Event.stack_key = Event.start event
        }

-- ** pair

{- | If the Convert.Track is present, then that is the track being integrated
    in from the source.  If it's not present, then this track is no longer
    present in the integration source.  If there is a TrackNum, then this track
    isn't present in the destination, and should be created.  Otherwise, it
    should be merged with the given Dest.

    (Nothing, Left 0) means the track is gone from both source and destination,
    so this TrackPair can be ignored.
-}
type TrackPair = (Maybe Convert.Track, Either TrackNum Dest)
-- | Score integrate copies tracks 1:1, so the destination tracks always have
-- a TrackId, and I can match them up by TrackId.
type ScoreTrackPair = (Maybe (TrackId, Events.Events), Either TrackNum Dest)
type Dest = (TrackId, Block.EventIndex)

{- | Match up new tracks and integrated tracks so I know who to diff against
    whom.  This is called once for each integrate source block.

    Note tracks are simply zipped up, so if a note track is added at the
    beginning it will look like everything changed and the diff won't work
    correctly.  But control tracks are matched based on name, so they should be
    robust against controls appearing or disappearing.

    Also figure out TrackNums for index tracks that don't exist.  An index
    track can not exist because it was never there, or because it was index but
    is no longer in the block (presumably manually deleted).

    TrackNums are assigned increasing from the previous track that was present,
    or at the end of the block if no tracks are present.  This way new control
    tracks should be added adjacent to their sisters, and the first integrate
    will append the generated tracks to the end of the block.
-}
pair_tracks :: [Maybe TrackId] -- ^ Tracks in the block, in tracknum order.
    -- Nothing for non-event tracks like rulers.
    -> Convert.Tracks -> [Block.NoteDestination] -> [[TrackPair]]
    -- ^ Each [TrackPair] is (note : controls).
pair_tracks track_ids tracks dests = map (filter is_valid) $
    snd $ List.mapAccumL resolve1 (length track_ids) $ map pairs_of $
        Seq.zip_padded tracks dests
    where
    -- Pair up the tracks.
    pairs_of (Seq.First (note, controls)) = map Seq.First (note : controls)
    pairs_of (Seq.Second (Block.NoteDestination key note controls)) =
        map Seq.Second (note : Map.elems controls)
    pairs_of (Seq.Both track dest) = pair_destination track dest

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
    tracknum_of track_id = List.elemIndex (Just track_id) track_ids
    is_valid (Nothing, Left _) = False
    is_valid _ = True

-- | Pair up the controls based on the track title, which should be the control
-- name.
pair_destination :: (Convert.Track, [Convert.Track]) -> Block.NoteDestination
    -> [Seq.Paired Convert.Track (TrackId, Block.EventIndex)]
pair_destination (note, controls)
        (Block.NoteDestination key note_dest control_dests) =
    Seq.Both note note_dest : pair_controls controls control_dests
    where
    pair_controls tracks dests =
        map snd $ Seq.pair_sorted (Seq.sort_on fst keyed) (Map.toAscList dests)
        where keyed = Seq.key_on Convert.track_title tracks

-- | Pair up tracks in an analogous way to 'pair_tracks'.  The difference is
-- that ScoreDestinations are matched up by TrackId, so I don't have to do any
-- sketchy zipping heuristics.  I still have to guess about the output tracknum
-- for new tracks though.
score_pair_tracks :: [Maybe TrackId] -> [(TrackId, Track.Track)]
    -> Block.ScoreDestinations -> [ScoreTrackPair]
score_pair_tracks track_ids sources dests =
    snd (List.mapAccumL pair_in_order (length track_ids) sources)
        ++ deleted
    where
    pair_in_order next_tracknum source@(source_id, _) =
        case lookup source_id dests of
            -- make new track
            Nothing ->
                (next_tracknum + 1, (make_source source, Left next_tracknum))
            -- merge
            Just dest -> (tracknum + 1, (make_source source, Right dest))
                where tracknum = tracknum_of next_tracknum dest
    deleted = mapMaybe deleted_track dests
    deleted_track (source_id, dest) = case lookup source_id sources of
        Nothing -> Just (Nothing, Right dest)
        Just _ -> Nothing
    make_source (source_id, source) =
        Just (source_id, Track.track_events source)
    tracknums = Map.fromList [(track_id, tracknum) |
        (tracknum, Just track_id) <- zip [0..] track_ids]
    tracknum_of deflt (source_id, _) =
        Map.findWithDefault deflt source_id tracknums


-- ** diff

-- | Find out how to merge new integrated output with user edits by diffing it
-- against the old integrated output.
diff_events :: Block.EventIndex -- ^ results of last integrate
    -> [Event.Event]
    -- ^ current events, which is last integrate plus user edits
    -> (Set Event.IndexKey, [Edit])
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
        Nothing -> Add (Event.stack_ #= Nothing $ new)
        Just old -> Edit key (diff_event old new)

index_key :: Event.Event -> Maybe Event.IndexKey
index_key = fmap Event.stack_key . Event.stack

diff_event :: Event.Event -> Event.Event -> [Modify]
diff_event old new = concat
    [ cmp (Event.start old) (Event.start new) (Position (Event.start new))
    , cmp (Event.duration old) (Event.duration new)
        (Duration (Event.duration new))
    , diff_text (Event.text old) (Event.text new)
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
diff_text :: Text -> Text -> [Modify]
diff_text old new
    | old == new = []
    | old `Text.isSuffixOf` new && ends_with_pipe prefix = [Prefix prefix]
    | otherwise = [Set new]
    where
    prefix = Text.take (Text.length new - Text.length old) new
    ends_with_pipe text = "|" `Text.isSuffixOf` pre && Text.all (==' ') post
        where (pre, post) = Text.breakOnEnd "|" text

data Edit =
    -- | This event was added, and will be copied to the output.
    Add !Event.Event
    -- | This matched an existing event, which has possibly been modified, so
    -- I have to merge the new event while applying any modifications.
    | Edit !Event.IndexKey ![Modify]
    deriving (Eq, Show)

data Modify = Position !ScoreTime | Duration !ScoreTime
    | Set !Text | Prefix !Text
    deriving (Eq, Show)

instance Pretty Edit where
    format (Add event) =
        Pretty.constructor "Add" [Pretty.format event]
    format (Edit key mods) = Pretty.constructor "Edit"
        [Pretty.format key, Pretty.format mods]

instance Pretty Modify where pretty = showt

is_modified :: Edit -> Bool
is_modified (Edit _ mods) = not (null mods)
is_modified _ = True

-- ** apply

apply :: Set Event.IndexKey -- ^ events that were deleted
    -> [Edit] -> [Event.Event] -- ^ results of current integrate
    -> Events.Events
apply deletes adds_edits = make . mapMaybe edit
    where
    -- Adds go afterwards so they can replace coincident events.
    make events = Events.from_list (events ++ adds)
    edit event
        | Event.start event `Set.member` deletes = Nothing
        | Just mods@(_:_) <- Map.lookup (Event.start event) edit_map =
            Just $ apply_modifications mods event
        -- A new event from the integrate.
        | otherwise = Just $ Event.unmodified event
    edit_map = Map.fromList $ filter (not . null . snd) edits
    (adds, edits) = Either.partitionEithers (map to_either adds_edits)
    to_either (Add event) = Left event
    to_either (Edit key mods) = Right (key, mods)

apply_modifications :: [Modify] -> Event.Event -> Event.Event
apply_modifications mods event = foldl' go event mods
    where
    go event mod = ($event) $ case mod of
        Position p -> Event.start_ #= p
        Duration d -> Event.duration_ #= d
        Set text -> Event.text_ #= text
        Prefix text -> Event.text_ %= (text<>)
