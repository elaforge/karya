-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.TrackTree where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import qualified Data.Tree as Tree

import qualified Util.Pretty as Pretty
import qualified Util.Tree as Tree
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | A TrackTree is the Skeleton resolved to the tracks it references.
type TrackTree = [Tree.Tree Ui.TrackInfo]

tracks_of :: Ui.M m => BlockId -> m [Ui.TrackInfo]
tracks_of block_id = do
    block <- Ui.get_block block_id
    state <- Ui.get
    return $ track_info block (Ui.state_tracks state)
    where
    track_info block tracks = do
        (i, btrack@(Block.Track { Block.tracklike_id = Block.TId tid _}))
            <- zip [0..] (Block.block_tracks block)
        track <- maybe [] (:[]) (Map.lookup tid tracks)
        return $ Ui.TrackInfo
            { track_title = Track.track_title track
            , track_id = tid
            , track_tracknum = i
            , track_block = btrack
            }

-- | Return @(parents, self : children)@.
parents_children_of :: Ui.M m => BlockId -> TrackId
    -> m (Maybe ([Ui.TrackInfo], [Ui.TrackInfo]))
parents_children_of block_id track_id = do
    tree <- track_tree_of block_id
    case List.find (\(t, _, _) -> Ui.track_id t == track_id)
            (Tree.flat_paths tree) of
        Nothing -> return Nothing
        Just (track, parents, children) ->
            return $ Just (parents, track : children)

-- | This is like 'parents_children_of', but only the children, and it doesn't
-- include the given TrackId.
get_children_of :: Ui.M m => BlockId -> TrackId -> m [Ui.TrackInfo]
get_children_of block_id track_id =
    parents_children_of block_id track_id >>= \case
        Just (_, _ : children) -> return children
        _ -> Ui.throw $ "no children of " <> pretty (block_id, track_id)

is_child_of :: Ui.M m => BlockId -> TrackNum -> TrackNum -> m Bool
is_child_of block_id parent child = do
    children <- get_children_of block_id
        =<< Ui.get_event_track_at block_id parent
    return $ child `elem` map Ui.track_tracknum children

-- | Combine the skeleton with the tracks to create a TrackTree.
--
-- TODO this is pretty complicated.  If I stored the tracks as a tree in the
-- first place and generated the skeleton from that then this would all go
-- away.  But that would mean redoing all the "Ui.Skeleton" operations for
-- trees.  And the reason I didn't do it in the first place was the hassle of
-- graph operations on a Data.Tree.
track_tree_of :: Ui.M m => BlockId -> m TrackTree
track_tree_of block_id = do
    skel <- Ui.get_skeleton block_id
    tracks <- tracks_of block_id
    ntracks <- fmap (length . Block.block_tracklike_ids) (Ui.get_block block_id)
    let by_tracknum = Map.fromList $
            zip (map Ui.track_tracknum tracks) tracks
    let (resolved, missing) = resolve_track_tree by_tracknum
            (Skeleton.to_forest ntracks skel)
    -- Rulers and dividers should show up as missing.  They're ok as long as
    -- they have no edges.
    let really_missing = filter (not . Skeleton.lonely_vertex skel) missing
    unless (null really_missing) $
        Ui.throw $ "skeleton of " <> showt block_id
            <> " names missing tracknums: " <> showt really_missing
    return resolved

-- | Resolve the TrackNum indices in a tree into whatever values as given by
-- a map.
resolve_track_tree :: Map TrackNum a -> [Tree.Tree TrackNum]
    -> ([Tree.Tree a], [TrackNum]) -- ^ resolved tree, and missing TrackNums
resolve_track_tree tracknums = foldr (cat_tree . go) ([], [])
    where
    go (Tree.Node tracknum subs) = case Map.lookup tracknum tracknums of
        Nothing -> (Nothing, [tracknum])
        Just track_info ->
            let (subforest, missing) = resolve_track_tree tracknums subs
            in (Just (Tree.Node track_info subforest), missing)
    cat_tree (maybe_tree, missing) (forest, all_missing) = case maybe_tree of
        Nothing -> (forest, missing ++ all_missing)
        Just tree -> (tree : forest, missing ++ all_missing)

strip_disabled_tracks :: Ui.M m => BlockId -> TrackTree -> m TrackTree
strip_disabled_tracks block_id = concatMapM strip
    where
    strip (Tree.Node track subs) = ifM (disabled track)
        (concatMapM strip subs)
        ((:[]) . Tree.Node track <$> concatMapM strip subs)
    disabled = fmap (Block.Disable `Set.member`)
        . Ui.track_flags block_id . Ui.track_tracknum

type EventsTree = [EventsNode]
type EventsNode = Tree.Tree Track

data Track = Track {
    track_title :: !Text
    -- | Events on this track.  These are shifted by
    -- 'Derive.Slice.slice_notes', so they are in ScoreTime, not TrackTime.
    , track_events :: !Events.Events
    -- | This goes into the stack when the track is evaluated.  Inverted tracks
    -- will carry the TrackId of the track they were inverted from, so they'll
    -- show up in the stack twice.  This means they can record their environ
    -- as it actually is when the notes are evaluated, rather than its
    -- pre-invert value, which is likely to not have the right scale.
    , track_id :: !(Maybe TrackId)
    -- | The block these events came from.  A track can appear in multiple
    -- blocks, but can only appear once in each block.
    , track_block_id :: !(Maybe BlockId)

    -- | The relative start and end of this slice of track.  Like
    -- 'track_events', this is in ScoreTime, not TrackTime.
    , track_start :: !ScoreTime
    , track_end :: !ScoreTime

    -- | True if this is a sliced track.  That means it's a fragment of
    -- a track and certain track-level things should be skipped.
    , track_sliced :: !Sliced
    -- | These events are not evaluated, but go in
    -- 'Derive.Derive.ctx_prev_events' and ctx_next_events.  This is so that
    -- sliced calls (such as inverting calls) can see previous and following
    -- events.  Shifted along with 'track_events'.
    , track_around :: !([Event.Event], [Event.Event])

    -- | If the events have been shifted from their original positions on the
    -- track, add this to them to put them back in TrackTime.
    , track_shifted :: !TrackTime
    -- | This is the track's track voice, as defined in 'Environ.track_voices'.
    -- Originally I tried to keep it all within "Derive.Call.BlockUtil", but
    -- it gets complicated with child tracks and slicing.  Putting it in
    -- 'Track' ensures it can't get lost.
    , track_voice :: !(Maybe Int)
    } deriving (Show)

track_range :: Track -> (TrackTime, TrackTime)
track_range track = (track_shifted track, track_shifted track + track_end track)

instance Pretty Track where
    format (Track title events track_id block_id start end sliced
            around shifted voice) =
        Pretty.record "Track"
            [ ("title", Pretty.format title)
            , ("events", Pretty.format events)
            , ("track_id", Pretty.format track_id)
            , ("block_id", Pretty.format block_id)
            , ("start", Pretty.format start)
            , ("end", Pretty.format end)
            , ("sliced", Pretty.format sliced)
            , ("around", Pretty.format around)
            , ("shifted", Pretty.format shifted)
            , ("voice", Pretty.format voice)
            ]

make_track :: Text -> Events.Events -> ScoreTime -> Track
make_track title events end = Track
    { track_title = title
    , track_events = events
    , track_id = Nothing
    , track_block_id = Nothing
    , track_start = 0
    , track_end = end
    , track_sliced = NotSliced
    , track_around = ([], [])
    , track_shifted = 0
    , track_voice = Nothing
    }

data Sliced =
    -- | An intact track, unchanged from the score.
    --
    -- It's confusing to say track_sliced track == NotSliced, and I could pick
    -- something like Intact, but there's no precedent for that terminology.
    NotSliced
    -- | A "Derive.Slice"d fragment, and certain track-level things should be
    -- skipped.
    | Sliced !Types.Orientation
    -- | Set on the fake track created by inversion.
    | Inversion
    deriving (Eq, Show)

instance Pretty Sliced where pretty = showt

block_track_id :: Track -> Maybe (BlockId, TrackId)
block_track_id track = do
    bid <- track_block_id track
    tid <- track_id track
    return (bid, tid)

events_tree_of :: Ui.M m => BlockId -> m EventsTree
events_tree_of block_id = do
    info_tree <- track_tree_of block_id
    end <- Ui.block_ruler_end block_id
    events_tree block_id end info_tree

events_tree :: Ui.M m => BlockId -> ScoreTime -> [Tree.Tree Ui.TrackInfo]
    -> m EventsTree
events_tree block_id end = mapM resolve . track_voices
    where
    resolve (Tree.Node (Ui.TrackInfo title track_id _ _, voice) subs) =
        Tree.Node <$> make title track_id voice <*> mapM resolve subs
    make title track_id voice = do
        events <- Ui.get_events track_id
        return $ (make_track title events end)
            { track_id = Just track_id
            , track_block_id = Just block_id
            , track_voice = voice
            }

-- | Get the EventsTree of a block.  Strip disabled tracks.
block_events_tree :: Ui.M m => BlockId -> m EventsTree
block_events_tree block_id = do
    info_tree <- strip_disabled_tracks block_id =<< track_tree_of block_id
    -- This is the end of the last event or ruler, not
    -- Ui.block_logical_range.  The reason is that functions that look at
    -- track_end are expecting the physical end, e.g.
    -- Control.derive_control uses it to put the last sample on the tempo
    -- track.
    end <- Ui.block_end block_id
    events_tree block_id end info_tree

-- | All the children of this EventsNode with TrackIds.
track_children :: EventsNode -> Set TrackId
track_children = foldl' (flip Set.insert) Set.empty
    . mapMaybe track_id . Tree.flatten

-- | Each note track with an instrument gets a count and maximum count, so they
-- can go in 'Environ.track_voice' and 'Environ.track_voices'.
track_voices :: [Tree.Tree Ui.TrackInfo]
    -> [Tree.Tree (Ui.TrackInfo, Maybe Int)]
track_voices tracks = map (fmap only_inst) $ count_occurrences inst_of tracks
    where
    inst_of = not_empty <=< ParseTitle.title_to_instrument . Ui.track_title
        where
        not_empty inst = if inst == ScoreT.empty_instrument
            then Nothing else Just inst
    only_inst (track, voice)
        | Just _ <- inst_of track = (track, Just voice)
        | otherwise = (track, Nothing)

-- | For each element, give its index amount its equals, and the total number
-- of elements equal to it.
count_occurrences :: (Traversable f, Traversable g, Ord k) =>
    (a -> k) -> f (g a) -> f (g (a, Int))
count_occurrences key =
    snd . (Traversable.mapAccumL . Traversable.mapAccumL) go mempty
    where
    go counts x = (Map.insert (key x) (n+1) counts, (x, n))
        where n = Map.findWithDefault 0 (key x) counts
