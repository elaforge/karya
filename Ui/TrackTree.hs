-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.TrackTree where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Tree as Tree

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track

import Types


-- | A TrackTree is the Skeleton resolved to the tracks it references.
type TrackTree = [Tree.Tree State.TrackInfo]

tracks_of :: State.M m => BlockId -> m [State.TrackInfo]
tracks_of block_id = do
    block <- State.get_block block_id
    state <- State.get
    return [State.TrackInfo (Track.track_title track) tid i
        | (i, tid, track) <- track_info block (State.state_tracks state)]
    where
    track_info block tracks = do
        (i, Block.TId tid _) <- Seq.enumerate (Block.block_tracklike_ids block)
        track <- maybe mzero (:[]) (Map.lookup tid tracks)
        return (i, tid, track)

-- | Return @(parents, self : children)@.
parents_children_of :: State.M m => BlockId -> TrackId
    -> m (Maybe ([State.TrackInfo], [State.TrackInfo]))
parents_children_of block_id track_id = do
    tree <- track_tree_of block_id
    case List.find (\(t, _, _) -> State.track_id t == track_id)
            (Tree.flat_paths tree) of
        Nothing -> return Nothing
        Just (track, parents, children) ->
            return $ Just (parents, track : children)

-- | This is like 'parents_children_of', but only the children, and it doesn't
-- include the given TrackId.
children_of :: State.M m => BlockId -> TrackId -> m (Maybe [State.TrackInfo])
children_of block_id track_id =
    parents_children_of block_id track_id >>= \x -> return $ case x of
        Just (_, _ : children) -> Just children
        _ -> Nothing

-- | Combine the skeleton with the tracks to create a TrackTree.
--
-- TODO this is pretty complicated.  If I stored the tracks as a tree in the
-- first place and generated the skeleton from that then this would all go
-- away.  But that would mean redoing all the "Ui.Skeleton" operations for
-- trees, which would be a huge pain.  And the reason I didn't do it in the
-- first place was the hassle of graph operations on a Data.Tree.
track_tree_of :: State.M m => BlockId -> m TrackTree
track_tree_of block_id = do
    skel <- State.get_skeleton block_id
    tracks <- tracks_of block_id
    ntracks <- fmap (length . Block.block_tracklike_ids)
        (State.get_block block_id)
    let by_tracknum = Map.fromList $
            zip (map State.track_tracknum tracks) tracks
    let (resolved, missing) = resolve_track_tree by_tracknum
            (Skeleton.to_forest ntracks skel)
    -- Rulers and dividers should show up as missing.  They're ok as long as
    -- they have no edges.
    let really_missing = filter (not . Skeleton.lonely_vertex skel) missing
    unless (null really_missing) $
        State.throw $ "skeleton of " ++ show block_id
            ++ " names missing tracknums: " ++ show really_missing
    return resolved

-- | Resolve the TrackNum indices in a tree into whatever values as given by
-- a map.
resolve_track_tree :: Map.Map TrackNum a -> [Tree.Tree TrackNum]
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

strip_disabled_tracks :: State.M m => BlockId -> TrackTree -> m TrackTree
strip_disabled_tracks block_id = concatMapM strip
    where
    strip (Tree.Node track subs) = ifM (disabled track)
        (concatMapM strip subs)
        ((:[]) . Tree.Node track <$> concatMapM strip subs)
    disabled = fmap (Block.Disable `Set.member`)
        . State.track_flags block_id . State.track_tracknum

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

    -- | The relative end of this slice of track.  Like 'track_events', this is
    -- in ScoreTime, not TrackTime.
    , track_end :: !ScoreTime

    -- | True if this is a sliced track.  That means it's a fragment of
    -- a track and certain track-level things should be skipped.
    , track_sliced :: !Bool
    -- | True if this was created as a result of inversion.  It's just here
    -- to hand off to 'Derive.info_inverted'.  If this is True,
    -- 'track_sliced' will also be True.  TODO so why not a 3 state type?
    , track_inverted :: !Bool
    -- | These events are not evaluated, but go in
    -- 'Derive.Derive.info_prev_events' and info_next_events.  This is so that
    -- sliced calls (such as inverting calls) can see previous and following
    -- events.  Shifted along with 'track_events'.
    , track_around :: !([Event.Event], [Event.Event])

    -- | If the events have been shifted from their original positions on the
    -- track, add this to them to put them back in TrackTime.
    , track_shifted :: !TrackTime
    } deriving (Show)

track_range :: Track -> (TrackTime, TrackTime)
track_range track = (track_shifted track, track_shifted track + track_end track)

instance Pretty.Pretty Track where
    format (Track title events track_id block_id end sliced inverted around
            shifted) =
        Pretty.record_title "Track"
            [ ("title", Pretty.format title)
            , ("events", Pretty.format events)
            , ("track_id", Pretty.format track_id)
            , ("block_id", Pretty.format block_id)
            , ("end", Pretty.format end)
            , ("sliced", Pretty.format sliced)
            , ("inverted", Pretty.format inverted)
            , ("around", Pretty.format around)
            , ("shifted", Pretty.format shifted)
            ]

make_track :: Text -> Events.Events -> ScoreTime -> Track
make_track title events end = Track
    { track_title = title
    , track_events = events
    , track_id = Nothing
    , track_block_id = Nothing
    , track_end = end
    , track_sliced = False
    , track_inverted = False
    , track_around = ([], [])
    , track_shifted = 0
    }

track_block_track_id :: Track -> Maybe (BlockId, TrackId)
track_block_track_id track = do
    bid <- track_block_id track
    tid <- track_id track
    return (bid, tid)

events_tree_of :: State.M m => BlockId -> m EventsTree
events_tree_of block_id = do
    info_tree <- track_tree_of block_id
    end <- State.block_ruler_end block_id
    events_tree block_id end info_tree

events_tree :: State.M m => BlockId -> ScoreTime -> TrackTree -> m EventsTree
events_tree block_id end = mapM resolve
    where
    resolve (Tree.Node (State.TrackInfo title track_id _) subs) =
        Tree.Node <$> make title track_id <*> mapM resolve subs
    make title track_id = do
        track <- State.get_track track_id
        return $ (make_track title (Track.track_events track) end)
            { track_id = Just track_id
            , track_block_id = Just block_id
            }

-- | All the children of this EventsNode with TrackIds.
track_children :: EventsNode -> Set.Set TrackId
track_children = List.foldl' (flip Set.insert) Set.empty
    . mapMaybe track_id . Tree.flatten
