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

tracks_of :: (State.M m) => BlockId -> m [State.TrackInfo]
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
parents_children_of :: (State.M m) => BlockId -> TrackId
    -> m (Maybe ([State.TrackInfo], [State.TrackInfo]))
parents_children_of block_id track_id = do
    tree <- get_track_tree block_id
    case List.find (\(t, _, _) -> State.track_id t == track_id)
            (Tree.flat_paths tree) of
        Nothing -> return Nothing
        Just (track, parents, children) ->
            return $ Just (parents, track : children)

-- | This is like 'parents_children_of', but only the children, and it doesn't
-- include the given TrackId.
children_of :: (State.M m) => BlockId -> TrackId -> m (Maybe [State.TrackInfo])
children_of block_id track_id =
    parents_children_of block_id track_id >>= \x -> return $ case x of
        Just (_, _ : children) -> Just children
        _ -> Nothing

get_track_tree :: (State.M m) => BlockId -> m TrackTree
get_track_tree block_id = do
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

strip_disabled_tracks :: (State.M m) => BlockId -> TrackTree -> m TrackTree
strip_disabled_tracks block_id = concatMapM strip
    where
    strip (Tree.Node track subs) = ifM (disabled track)
        (concatMapM strip subs)
        ((:[]) . Tree.Node track <$> concatMapM strip subs)
    disabled = fmap (Block.Disable `Set.member`)
        . State.track_flags block_id . State.track_tracknum

type EventsTree = [EventsNode]
type EventsNode = Tree.Tree TrackEvents

data TrackEvents = TrackEvents {
    tevents_title :: !String
    , tevents_events :: !Events.Events
    -- | This goes into the stack when the track is evaluated.  Inverted tracks
    -- will carry the TrackId of the track they were inverted from, so they'll
    -- show up in the stack twice.  This means they can record their environ
    -- as it actually is when the notes are evaluated, rather than its
    -- pre-invert value, which is likely to not have the right scale.
    , tevents_track_id :: !(Maybe TrackId)

    -- | The relative end of this slice of track.  This is different from @snd
    -- . tevents_range@ because it gets shifted after slicing to be relative to
    -- the sliced note, while tevents_range always reflects the absolute track
    -- range.  So the difference between the two is how much this slice has
    -- been shifted.
    , tevents_end :: !ScoreTime

    -- | Range of the track.  This may be past the end of the last event since
    -- it's the range of the block as a whole.
    --
    -- Used by "Derive.Cache": due to inverting calls, a control track may be
    -- sliced to a shorter range.  In that case, I shouldn't bother with
    -- damage outside of its range.
    --
    -- This is a (start, end) range, not (start, dur).
    , tevents_range :: !(ScoreTime, ScoreTime)
    -- | True if this is a sliced track.  That means it's a fragment of
    -- a track and so certain track-level things, like recording a track
    -- signal, should be skipped.
    , tevents_sliced :: !Bool
    -- | These events are not evaluated, but go in the
    -- 'Derive.Derive.info_prev_events' and info_next_events.  This is so that
    -- sliced calls (such as inverting calls) can see previous and following
    -- events.
    , tevents_around :: !([Event.Event], [Event.Event])

    -- | If the events have been shifted from their original positions on the
    -- track, this can be added to them to put them back in track time.  This
    -- is for the stack, which should always be in track time.  It's probably
    -- the same as @fst . tevents_range@, but only applies if the events have
    -- been shifted, which you can't tell from just looking at
    -- @tevents_range@.
    , tevents_shifted :: !ScoreTime
    } deriving (Show)

instance Pretty.Pretty TrackEvents where
    format track = Pretty.record
        (Pretty.text "TrackEvents"
            Pretty.<+> Pretty.format (tevents_title track)
            Pretty.<+> Pretty.format (tevents_track_id track))
        [ ("end", Pretty.format (tevents_end track))
        , ("range", Pretty.format (tevents_range track))
        , ("sliced", Pretty.format (tevents_sliced track))
        , ("shifted", Pretty.format (tevents_shifted track))
        , ("events", Pretty.format (tevents_events track))
        ]

track_events :: String -> Events.Events -> ScoreTime -> TrackEvents
track_events title events end = TrackEvents
    { tevents_title = title
    , tevents_events = events
    , tevents_track_id = Nothing
    , tevents_end = end
    , tevents_range = (0, end)
    , tevents_sliced = False
    , tevents_around = ([], [])
    , tevents_shifted = 0
    }

events_tree_of :: (State.M m) => BlockId -> m EventsTree
events_tree_of block_id = do
    info_tree <- get_track_tree block_id
    end <- State.block_ruler_end block_id
    events_tree end info_tree

events_tree :: (State.M m) => ScoreTime -> TrackTree -> m EventsTree
events_tree end = mapM resolve
    where
    resolve (Tree.Node (State.TrackInfo title track_id _) subs) =
        Tree.Node <$> make title track_id <*> mapM resolve subs
    make title track_id = do
        track <- State.get_track track_id
        return $ (track_events title (Track.track_events track) end)
            { tevents_track_id = Just track_id }
