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
    createBlock
    -- * merge
    , MergeTitles(..)
    , mergeBlock, scoreMergeBlock, mergeTracks
    , scoreMergeTracks
    , Edit(..), Modify(..), isModified
    -- * diff
    , diffEvents
#ifdef TESTING
    , makeIndex
    , diff, diffEvent, apply
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
import qualified Util.Lists as Lists
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

createBlock :: Ui.M m => BlockId -> Convert.Tracks
    -> m (BlockId, [Block.NoteDestination])
createBlock sourceId tracks = do
    rulerId <- Ui.blockRuler sourceId
    destId <- Create.block rulerId
    (,) destId <$> mergeBlock destId tracks []

mergeBlock :: Ui.M m => BlockId -> Convert.Tracks
    -> [Block.NoteDestination] -> m [Block.NoteDestination]
mergeBlock = mergeTracks KeepTitles

scoreMergeBlock :: Ui.M m => BlockId -> BlockId -> Block.ScoreDestinations
    -> m Block.ScoreDestinations
scoreMergeBlock sourceId destId dests = do
    tree <- TrackTree.trackTreeOf sourceId
    scoreMerge destId tree dests

-- * tracks

data MergeTitles
    = KeepTitles -- ^ leave the titles of merged tracks alone
    | ReplaceTitles -- ^ replace titles with the merge source
    deriving (Eq, Show)

-- | Given a set of source 'Convert.Tracks' and a set of previously integrated
-- destination tracks, merge them together and give new destination tracks.
-- A single integrating source track can create multiple Convert.Tracks, and
-- an integrating track can have >=1 destinations, so this is called once per
-- (source, destination) pair.
mergeTracks :: Ui.M m => MergeTitles -> BlockId -> Convert.Tracks
    -> [Block.NoteDestination] -> m [Block.NoteDestination]
mergeTracks mergeTitles blockId tracks dests = do
    trackIds <- allBlockTracks blockId
    newDests <- mapMaybeM (mergePairs mergeTitles blockId) $
        pairTracks trackIds tracks dests
    -- TODO doesn't this combine with the old skeleton?  Why isn't that
    -- a problem?
    addDeriveSkeleton blockId newDests
    pure newDests

addDeriveSkeleton :: Ui.M m => BlockId -> [Block.NoteDestination] -> m ()
addDeriveSkeleton blockId dests =
    whenM (Ui.hasExplicitSkeleton blockId) $ do
        trackIds <- allBlockTracks blockId
        skel <- Ui.require "integrate somehow created a cyclic skeleton"
            =<< Skeleton.addEdges (trackEdges trackIds dests) <$>
                Ui.getSkeleton blockId
        Ui.setSkeleton blockId skel

trackEdges :: [Maybe TrackId] -> [Block.NoteDestination]
    -> [(TrackNum, TrackNum)]
trackEdges trackIds = concatMap edges
    where
    edges (Block.NoteDestination _ (trackId, _) controls) =
        case tracknumOf trackId of
            Nothing -> []
            Just tracknum ->
                let controlNums = mapMaybe (tracknumOf . fst)
                        (Map.elems controls)
                in zip (tracknum : controlNums) controlNums
    tracknumOf trackId = List.elemIndex (Just trackId) trackIds

-- | Tracks in tracknum order.  Nothing for non-event tracks, like rulers.
allBlockTracks :: Ui.M m => BlockId -> m [Maybe TrackId]
allBlockTracks blockId =
    map Block.trackId . Block.blockTracks <$> Ui.getBlock blockId

-- ** score

-- | Update the given ScoreDestinations from the source block and track.
scoreMergeTracks :: Ui.M m => BlockId -> TrackId
    -> Block.ScoreDestinations -> m Block.ScoreDestinations
scoreMergeTracks blockId sourceId dests = do
    tree <- TrackTree.trackTreeOf blockId
    children <- Ui.require ("source track not found: " <> showt sourceId) $
        Trees.find ((==sourceId) . Ui.trackId) tree
    scoreMerge blockId [children] dests

scoreMerge :: Ui.M m => BlockId -> TrackTree.TrackTree
    -> Block.ScoreDestinations -> m Block.ScoreDestinations
scoreMerge blockId tree dests = do
    whenM (Ui.hasExplicitSkeleton blockId) $ do
        remove <- destinationEdges blockId (map (fst . snd) dests)
        Ui.modifySkeleton blockId (Skeleton.removeEdges remove)
    trackIds <- allBlockTracks blockId
    tracks <- getChildren tree
    dests <- mapMaybeM (scoreMergePair blockId) $
        scorePairTracks trackIds tracks dests
    whenM (Ui.hasExplicitSkeleton blockId) $
        addSkeleton blockId =<< sourceToDest blockId dests
            (map (fmap Ui.trackId) tree)
    pure dests

-- | Track pairs of the children of the given tree, sorted by tracknum.
getChildren :: Ui.M m => TrackTree.TrackTree -> m [(TrackId, Track.Track)]
getChildren =
    fmap (map snd . Lists.sortOn fst) . mapM resolve . concatMap Tree.flatten
    where
    resolve tinfo = do
        track <- Ui.getTrack (Ui.trackId tinfo)
        pure (Ui.trackTracknum tinfo, (Ui.trackId tinfo, track))

sourceToDest :: Ui.M m => BlockId -> Block.ScoreDestinations
    -> [Tree.Tree TrackId] -> m [Tree.Tree TrackNum]
sourceToDest blockId dests = mapM $ Traversable.mapM $ \trackId -> do
    destId <- maybe (Ui.throw $ "no destination for " <> showt trackId)
        (pure . fst) (lookup trackId dests)
    destTracknum blockId destId

-- | Get the edges that are part of the destination track structure.  This
-- is so I can clear out the old skeleton before replacing it with the new one.
-- Otherwise, adding a new track gets a mangled skeleton since the old edge
-- remains.  This only returns edges where both ends are in the destination
-- tracks, so if you manually add a non-integrated parent or child it should
-- remain that way.
destinationEdges :: Ui.M m => BlockId -> [TrackId] -> m [Skeleton.Edge]
destinationEdges blockId trackIds = do
    tracknums <- mapM (destTracknum blockId) trackIds
    edges <- Skeleton.flatten <$> Ui.getSkeleton blockId
    pure $ filter (\(p, c) -> p `elem` tracknums && c `elem` tracknums) edges

destTracknum :: Ui.M m => BlockId -> TrackId -> m TrackNum
destTracknum blockId trackId = Ui.require
    ("integrated track " <> showt trackId <> " not in " <> showt blockId)
    =<< Ui.tracknumOf blockId trackId

addSkeleton :: Ui.M m => BlockId -> [Tree.Tree TrackNum] -> m ()
addSkeleton blockId tree = do
    skel <- Ui.require "score integrated somehow created a cyclic skeleton"
        =<< Skeleton.addEdges (Trees.edges tree) <$> Ui.getSkeleton blockId
    Ui.setSkeleton blockId skel

-- ** merge

-- | Merge together TrackPairs, modifying the underlying tracks, and return
-- a NoteDestination.  The head of the TrackPairs is assumed to be the note
-- track, and the rest are its controls.
--
-- Control and pitch tracks are matched or created by title, but the note track
-- title is ignored.
mergePairs :: Ui.M m => MergeTitles -> BlockId -> [TrackPair]
    -> m (Maybe Block.NoteDestination)
mergePairs mergeTitles blockId pairs = do
    triples <- mapMaybeM (mergePair blockId) pairs
    case triples of
        [] -> pure Nothing
        (sourceTitle, noteId, noteIndex) : controls -> do
            -- TODO I could merge them, but I need the previous integrated
            -- title
            -- What about the control track titles?  I use those as keys, so I
            -- can't change them without breaking the link.
            when (mergeTitles == ReplaceTitles) $ do
                Ui.setTrackTitle noteId sourceTitle
            pure $ Just $ Block.NoteDestination key (noteId, noteIndex) $
                Map.fromList
                    [ (title, (trackId, index))
                    | (title, trackId, index) <- controls
                    ]
    where
    -- TODO once I use track keys I have to propagate this from the pairing.
    key = ""

mergePair :: Ui.M m => BlockId -> TrackPair
    -> m (Maybe (Text, TrackId, Block.EventIndex))
mergePair blockId pair = case pair of
    (Nothing, Left _) -> pure Nothing -- not reached
    (Just (Convert.Track title events), Left tracknum) -> do
        -- Track was deleted or it doesn't exist yet, so create it.
        trackId <- Create.track blockId tracknum title
            (Events.fromList (map Event.unmodified events))
        pure $ Just (title, trackId, makeIndex events)
    (Nothing, Right (trackId, _)) -> do
        -- Integrate no longer wants the track.  Don't delete the track in case
        -- there are manually created events on it.
        clearGeneratedEvents trackId
        pure Nothing
    (Just (Convert.Track title events), Right dest) -> do
        mergeTrack events dest
        pure $ Just (title, fst dest, makeIndex events)

scoreMergePair :: Ui.M m => BlockId -> ScoreTrackPair
    -> m (Maybe (TrackId, (TrackId, Block.EventIndex)))
scoreMergePair blockId pair = case pair of
    (Nothing, Left _) -> pure Nothing -- not reached
    (Just (sourceId, events), Left tracknum) -> do
        -- Track was deleted or never existed.
        let stacked = addEventStacks blockId sourceId events
        title <- Ui.getTrackTitle sourceId
        trackId <- Create.track blockId tracknum
            title (Events.fromList (map Event.unmodified stacked))
        pure $ Just (sourceId, (trackId, makeIndex stacked))
    (Nothing, Right (trackId, _)) -> do
        -- Integrate no longer wants the track.  Don't delete the track in case
        -- there are manually created events on it.
        clearGeneratedEvents trackId
        pure Nothing
    (Just (sourceId, events), Right dest) -> do
        let stacked = addEventStacks blockId sourceId events
        mergeTrack stacked dest
        pure $ Just (sourceId, (fst dest, makeIndex stacked))

clearGeneratedEvents :: Ui.M m => TrackId -> m ()
clearGeneratedEvents trackId = Ui.modifyEvents trackId $
    Events.fromList . filter (Maybe.isNothing . Event.stack) . Events.ascending

-- | This implements a 3-way merge.  First, diff the recorded index (which
-- is a pristine copy of the previous integrate) against the current contents
-- of the track.  This gives the edits that have been applied manually against
-- the integrate output.  Then those edits are replayed against the new
-- integrate output.
mergeTrack :: Ui.M m => [Event.Event] -> Dest -> m ()
mergeTrack sourceEvents (trackId, index) = do
    oldEvents <- Events.ascending <$> Ui.getEvents trackId
    let (deletes, edits) = diffEvents index oldEvents
        newEvents = apply deletes edits sourceEvents
    Ui.modifySomeEvents trackId (const newEvents)

-- | Create an index from integrated tracks.  Since they are integrated, they
-- should all have stacks, so events without stacks are discarded.
makeIndex :: [Event.Event] -> Block.EventIndex
makeIndex events = Map.fromList
    [(key, event) | (Just key, event) <- Lists.keyOn indexKey events]

-- | Unlike derive integration, the events are copied directly from the
-- source, and hence don't have stacks.
addEventStacks :: BlockId -> TrackId -> Events.Events -> [Event.Event]
addEventStacks blockId trackId = map addStack . Events.ascending
    where
    addStack event = Event.stack_ #= Just (makeStack event) $ event
    makeStack event = Event.Stack
        { Event.stackStack =
            Stack.fromInnermost [Stack.Track trackId, Stack.Block blockId]
        , Event.stackKey = Event.start event
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
pairTracks :: [Maybe TrackId] -- ^ Tracks in the block, in tracknum order.
    -- Nothing for non-event tracks like rulers.
    -> Convert.Tracks -> [Block.NoteDestination] -> [[TrackPair]]
    -- ^ Each [TrackPair] is (note : controls).
pairTracks trackIds tracks dests = map (filter isValid) $
    snd $ List.mapAccumL resolve1 (length trackIds) $ map pairsOf $
        Lists.zipPadded tracks dests
    where
    -- Pair up the tracks.
    pairsOf (Lists.First (note, controls)) = map Lists.First (note : controls)
    pairsOf (Lists.Second (Block.NoteDestination key note controls)) =
        map Lists.Second (note : Map.elems controls)
    pairsOf (Lists.Both track dest) = pairDestination track dest

    resolve1 nextTracknum pairs = List.mapAccumL resolve nextTracknum pairs
    -- Figure out tracknums.
    resolve nextTracknum (Lists.First track) =
        (nextTracknum + 1, (Just track, Left nextTracknum))
    resolve nextTracknum (Lists.Second dest) = case tracknumOf (fst dest) of
        -- Track deleted and the integrate no longer wants it.
        -- Ugly, but (Nothing, Left) can be code for "ignore me".
        Nothing -> (nextTracknum, (Nothing, Left 0))
        Just tracknum -> (tracknum + 1, (Nothing, Right dest))
    resolve nextTracknum (Lists.Both track dest) = case tracknumOf (fst dest) of
        Nothing -> (nextTracknum + 1, (Just track, Left nextTracknum))
        Just tracknum -> (tracknum + 1, (Just track, Right dest))
    tracknumOf trackId = List.elemIndex (Just trackId) trackIds
    isValid (Nothing, Left _) = False
    isValid _ = True

-- | Pair up the controls based on the track title, which should be the control
-- name.
pairDestination :: (Convert.Track, [Convert.Track]) -> Block.NoteDestination
    -> [Lists.Paired Convert.Track (TrackId, Block.EventIndex)]
pairDestination (note, controls)
        (Block.NoteDestination key noteDest controlDests) =
    Lists.Both note noteDest : pairControls controls controlDests
    where
    pairControls tracks dests =
        map snd $ Lists.pairSorted (Lists.sortOn fst keyed) $
        Map.toAscList dests
        where keyed = Lists.keyOn Convert.trackTitle tracks

-- | Pair up tracks in an analogous way to 'pair_tracks'.  The difference is
-- that ScoreDestinations are matched up by TrackId, so I don't have to do any
-- sketchy zipping heuristics.  I still have to guess about the output tracknum
-- for new tracks though.
scorePairTracks :: [Maybe TrackId] -> [(TrackId, Track.Track)]
    -> Block.ScoreDestinations -> [ScoreTrackPair]
scorePairTracks trackIds sources dests =
    snd (List.mapAccumL pairInOrder (length trackIds) sources)
        ++ deleted
    where
    pairInOrder nextTracknum source@(sourceId, _) =
        case lookup sourceId dests of
            -- make new track
            Nothing ->
                (nextTracknum + 1, (makeSource source, Left nextTracknum))
            -- merge
            Just dest -> (tracknum + 1, (makeSource source, Right dest))
                where tracknum = tracknumOf nextTracknum dest
    deleted = mapMaybe deletedTrack dests
    deletedTrack (sourceId, dest) = case lookup sourceId sources of
        Nothing -> Just (Nothing, Right dest)
        Just _ -> Nothing
    makeSource (sourceId, source) =
        Just (sourceId, Track.trackEvents source)
    tracknums = Map.fromList [(trackId, tracknum) |
        (tracknum, Just trackId) <- zip [0..] trackIds]
    tracknumOf deflt (sourceId, _) =
        Map.findWithDefault deflt sourceId tracknums


-- ** diff

-- | Find out how to merge new integrated output with user edits by diffing it
-- against the old integrated output.
diffEvents :: Block.EventIndex -- ^ results of last integrate
    -> [Event.Event]
    -- ^ current events, which is last integrate plus user edits
    -> (Set Event.IndexKey, [Edit])
    -- ^ set of deleted events, and edited events
diffEvents index events = (deletes, edits)
    where
    deletes = Set.difference (Map.keysSet index) $
        Set.fromList (mapMaybe indexKey events)
    edits = map (diff index) events

diff :: Block.EventIndex -> Event.Event -> Edit
diff index new = case indexKey new of
    Nothing -> Add new
    Just key -> case Map.lookup key index of
        -- Events with a stack but not in the index shouldn't happen, they
        -- indicate that the index is out of sync with the last
        -- integration.  To be safe, they're counted as an add, and the
        -- stack is deleted.  TODO could this multiply events endlessly?
        -- TODO This could be a symptom of tracks not lining up anymore.
        -- I should emit a warning.
        Nothing -> Add (Event.stack_ #= Nothing $ new)
        Just old -> Edit key (diffEvent old new)

indexKey :: Event.Event -> Maybe Event.IndexKey
indexKey = fmap Event.stackKey . Event.stack

diffEvent :: Event.Event -> Event.Event -> [Modify]
diffEvent old new = concat
    [ cmp (Event.start old) (Event.start new) (Position (Event.start new))
    , cmp (Event.duration old) (Event.duration new)
        (Duration (Event.duration new))
    , diffText (Event.text old) (Event.text new)
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
diffText :: Text -> Text -> [Modify]
diffText old new
    | old == new = []
    | old `Text.isSuffixOf` new && endsWithPipe prefix = [Prefix prefix]
    | otherwise = [Set new]
    where
    prefix = Text.take (Text.length new - Text.length old) new
    endsWithPipe text = "|" `Text.isSuffixOf` pre && Text.all (==' ') post
        where (pre, post) = Text.breakOnEnd "|" text

data Edit
    -- | This event was added, and will be copied to the output.
    = Add !Event.Event
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

isModified :: Edit -> Bool
isModified (Edit _ mods) = not (null mods)
isModified _ = True

-- ** apply

apply :: Set Event.IndexKey -- ^ events that were deleted
    -> [Edit] -> [Event.Event] -- ^ results of current integrate
    -> Events.Events
apply deletes addsEdits = make . mapMaybe edit
    where
    -- Adds go afterwards so they can replace coincident events.
    make events = Events.fromList (events ++ adds)
    edit event
        | Event.start event `Set.member` deletes = Nothing
        | Just mods@(_:_) <- Map.lookup (Event.start event) editMap =
            Just $ applyModifications mods event
        -- A new event from the integrate.
        | otherwise = Just $ Event.unmodified event
    editMap = Map.fromList $ filter (not . null . snd) edits
    (adds, edits) = Either.partitionEithers (map toEither addsEdits)
    toEither (Add event) = Left event
    toEither (Edit key mods) = Right (key, mods)

applyModifications :: [Modify] -> Event.Event -> Event.Event
applyModifications mods event = foldl' go event mods
    where
    go event mod = ($event) $ case mod of
        Position p -> Event.start_ #= p
        Duration d -> Event.duration_ #= d
        Set text -> Event.text_ #= text
        Prefix text -> Event.text_ %= (text<>)
