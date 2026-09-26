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
import qualified Util.Trees as Trees
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

tracksOf :: Ui.M m => BlockId -> m [Ui.TrackInfo]
tracksOf blockId = do
    block <- Ui.getBlock blockId
    state <- Ui.get
    pure $ trackInfo block (Ui.stateTracks state)
    where
    trackInfo block tracks = do
        (i, btrack@(Block.Track { Block.tracklikeId = Block.TId tid _}))
            <- zip [0..] (Block.blockTracks block)
        track <- maybe [] (:[]) (Map.lookup tid tracks)
        pure $ Ui.TrackInfo
            { trackTitle = Track.trackTitle track
            , trackId = tid
            , trackTracknum = i
            , trackBlock = btrack
            }

-- | Return @(parents, self : children)@.
parentsChildrenOf :: Ui.M m => BlockId -> TrackId
    -> m (Maybe ([Ui.TrackInfo], [Ui.TrackInfo]))
parentsChildrenOf blockId trackId = do
    tree <- trackTreeOf blockId
    case List.find (\(t, _, _) -> Ui.trackId t == trackId)
            (Trees.flatPaths tree) of
        Nothing -> pure Nothing
        Just (track, parents, children) ->
            pure $ Just (parents, track : children)

-- | This is like 'parents_children_of', but only the children, and it doesn't
-- include the given TrackId.
getChildrenOf :: Ui.M m => BlockId -> TrackId -> m [Ui.TrackInfo]
getChildrenOf blockId trackId =
    parentsChildrenOf blockId trackId >>= \case
        Just (_, _ : children) -> pure children
        _ -> Ui.throw $ "no children of " <> pretty (blockId, trackId)

isChildOf :: Ui.M m => BlockId -> TrackNum -> TrackNum -> m Bool
isChildOf blockId parent child = do
    children <- getChildrenOf blockId
        =<< Ui.getEventTrackAt blockId parent
    pure $ child `elem` map Ui.trackTracknum children

-- | Combine the skeleton with the tracks to create a TrackTree.
--
-- TODO this is pretty complicated.  If I stored the tracks as a tree in the
-- first place and generated the skeleton from that then this would all go
-- away.  But that would mean redoing all the "Ui.Skeleton" operations for
-- trees.  And the reason I didn't do it in the first place was the hassle of
-- graph operations on a Data.Tree.
trackTreeOf :: Ui.M m => BlockId -> m TrackTree
trackTreeOf blockId = do
    skel <- Ui.getSkeleton blockId
    tracks <- tracksOf blockId
    ntracks <- fmap (length . Block.blockTracklikeIds) (Ui.getBlock blockId)
    let byTracknum = Map.fromList $
            zip (map Ui.trackTracknum tracks) tracks
    let (resolved, missing) = resolveTrackTree byTracknum
            (Skeleton.toForest ntracks skel)
    -- Rulers and dividers should show up as missing.  They're ok as long as
    -- they have no edges.
    let reallyMissing = filter (not . Skeleton.lonelyVertex skel) missing
    unless (null reallyMissing) $
        Ui.throw $ "skeleton of " <> showt blockId
            <> " names missing tracknums: " <> showt reallyMissing
    pure resolved

-- | Resolve the TrackNum indices in a tree into whatever values as given by
-- a map.
resolveTrackTree :: Map TrackNum a -> [Tree.Tree TrackNum]
    -> ([Tree.Tree a], [TrackNum]) -- ^ resolved tree, and missing TrackNums
resolveTrackTree tracknums = foldr (catTree . go) ([], [])
    where
    go (Tree.Node tracknum subs) = case Map.lookup tracknum tracknums of
        Nothing -> (Nothing, [tracknum])
        Just trackInfo ->
            let (subforest, missing) = resolveTrackTree tracknums subs
            in (Just (Tree.Node trackInfo subforest), missing)
    catTree (maybeTree, missing) (forest, allMissing) = case maybeTree of
        Nothing -> (forest, missing ++ allMissing)
        Just tree -> (tree : forest, missing ++ allMissing)

stripDisabledTracks :: Ui.M m => BlockId -> TrackTree -> m TrackTree
stripDisabledTracks blockId = concatMapM strip
    where
    strip (Tree.Node track subs) = ifM (disabled track)
        (concatMapM strip subs)
        ((:[]) . Tree.Node track <$> concatMapM strip subs)
    disabled = fmap (Block.Disable `Set.member`)
        . Ui.trackFlags blockId . Ui.trackTracknum

type EventsTree = [EventsNode]
type EventsNode = Tree.Tree Track

data Track = Track
    { trackTitle :: !Text
    -- | Events on this track.  These are shifted by
    -- 'Derive.Slice.slice_notes', so they are in ScoreTime, not TrackTime.
    , trackEvents :: !Events.Events
    -- | This goes into the stack when the track is evaluated.  Inverted tracks
    -- will carry the TrackId of the track they were inverted from, so they'll
    -- show up in the stack twice.  This means they can record their environ
    -- as it actually is when the notes are evaluated, rather than its
    -- pre-invert value, which is likely to not have the right scale.
    , trackId :: !(Maybe TrackId)
    -- | The block these events came from.  A track can appear in multiple
    -- blocks, but can only appear once in each block.
    , trackBlockId :: !(Maybe BlockId)

    -- | The relative start and end of this slice of track.  Like
    -- 'track_events', this is in ScoreTime, not TrackTime.
    , trackStart :: !ScoreTime
    , trackEnd :: !ScoreTime

    -- | True if this is a sliced track.  That means it's a fragment of
    -- a track and certain track-level things should be skipped.
    , trackSliced :: !Sliced
    -- | These events are not evaluated, but go in
    -- 'Derive.Derive.ctx_prev_events' and ctx_next_events.  This is so that
    -- sliced calls (such as inverting calls) can see previous and following
    -- events.  Shifted along with 'track_events'.
    , trackAround :: !([Event.Event], [Event.Event])

    -- | If the events have been shifted from their original positions on the
    -- track, add this to them to put them back in TrackTime.
    , trackShifted :: !TrackTime
    -- | This is the track's track voice, as defined in 'Environ.track_voices'.
    -- Originally I tried to keep it all within "Derive.Call.BlockUtil", but
    -- it gets complicated with child tracks and slicing.  Putting it in
    -- 'Track' ensures it can't get lost.
    , trackVoice :: !(Maybe Int)
    } deriving (Show)

trackRange :: Track -> (TrackTime, TrackTime)
trackRange track = (trackShifted track, trackShifted track + trackEnd track)

instance Pretty Track where
    format (Track title events trackId blockId start end sliced
            around shifted voice) =
        Pretty.record "Track"
            [ ("title", Pretty.format title)
            , ("events", Pretty.format events)
            , ("track_id", Pretty.format trackId)
            , ("block_id", Pretty.format blockId)
            , ("start", Pretty.format start)
            , ("end", Pretty.format end)
            , ("sliced", Pretty.format sliced)
            , ("around", Pretty.format around)
            , ("shifted", Pretty.format shifted)
            , ("voice", Pretty.format voice)
            ]

makeTrack :: Text -> Events.Events -> ScoreTime -> Track
makeTrack title events end = Track
    { trackTitle = title
    , trackEvents = events
    , trackId = Nothing
    , trackBlockId = Nothing
    , trackStart = 0
    , trackEnd = end
    , trackSliced = NotSliced
    , trackAround = ([], [])
    , trackShifted = 0
    , trackVoice = Nothing
    }

data Sliced
    -- | An intact track, unchanged from the score.
    --
    -- It's confusing to say track_sliced track == NotSliced, and I could pick
    -- something like Intact, but there's no precedent for that terminology.
    = NotSliced
    -- | A "Derive.Slice"d fragment, and certain track-level things should be
    -- skipped.
    | Sliced !Types.Orientation
    -- | Set on the fake track created by inversion.
    | Inversion
    deriving (Eq, Show)

instance Pretty Sliced where pretty = showt

blockTrackId :: Track -> Maybe (BlockId, TrackId)
blockTrackId track = do
    bid <- trackBlockId track
    tid <- trackId track
    pure (bid, tid)

eventsTreeOf :: Ui.M m => BlockId -> m EventsTree
eventsTreeOf blockId = do
    infoTree <- trackTreeOf blockId
    end <- Ui.blockRulerEnd blockId
    eventsTree blockId end infoTree

eventsTree :: Ui.M m => BlockId -> ScoreTime -> [Tree.Tree Ui.TrackInfo]
    -> m EventsTree
eventsTree blockId end = mapM resolve . trackVoices
    where
    resolve (Tree.Node (Ui.TrackInfo title trackId _ _, voice) subs) =
        Tree.Node <$> make title trackId voice <*> mapM resolve subs
    make title trackId voice = do
        events <- Ui.getEvents trackId
        pure $ (makeTrack title events end)
            { trackId = Just trackId
            , trackBlockId = Just blockId
            , trackVoice = voice
            }

-- | Get the EventsTree of a block.  Strip disabled tracks.
blockEventsTree :: Ui.M m => BlockId -> m EventsTree
blockEventsTree blockId = do
    infoTree <- stripDisabledTracks blockId =<< trackTreeOf blockId
    -- This is the end of the last event or ruler, not
    -- Ui.block_logical_range.  The reason is that functions that look at
    -- track_end are expecting the physical end, e.g.
    -- Control.derive_control uses it to put the last sample on the tempo
    -- track.
    end <- Ui.blockEnd blockId
    eventsTree blockId end infoTree

-- | All the children of this EventsNode with TrackIds.
trackChildren :: EventsNode -> Set TrackId
trackChildren = foldl' (flip Set.insert) Set.empty
    . mapMaybe trackId . Tree.flatten

-- | Each note track with an instrument gets a count and maximum count, so they
-- can go in 'Environ.track_voice' and 'Environ.track_voices'.
trackVoices :: [Tree.Tree Ui.TrackInfo]
    -> [Tree.Tree (Ui.TrackInfo, Maybe Int)]
trackVoices tracks = map (fmap onlyInst) $ countOccurrences instOf tracks
    where
    instOf = notEmpty <=< ParseTitle.titleToInstrument . Ui.trackTitle
        where
        notEmpty inst = if inst == ScoreT.emptyInstrument
            then Nothing else Just inst
    onlyInst (track, voice)
        | Just _ <- instOf track = (track, Just voice)
        | otherwise = (track, Nothing)

-- | For each element, give its index amount its equals, and the total number
-- of elements equal to it.
countOccurrences :: (Traversable f, Traversable g, Ord k) =>
    (a -> k) -> f (g a) -> f (g (a, Int))
countOccurrences key =
    snd . (Traversable.mapAccumL . Traversable.mapAccumL) go mempty
    where
    go counts x = (Map.insert (key x) (n+1) counts, (x, n))
        where n = Map.findWithDefault 0 (key x) counts
