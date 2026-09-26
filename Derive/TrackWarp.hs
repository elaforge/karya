-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | TrackWarps are collected throughout derivation each time there is a new
    warp context.  By the end, they represent a complete mapping from ScoreTime
    to RealTime and back again, and can be used to create a TempoFunction and
    InverseTempoFunction, among other things.
-}
module Derive.TrackWarp (
    Track(..), WarpMap, TrackWarp(..)
    , collectTrackWarps
    , getTrackTrees
    -- * functions on TrackWarp
    , tempoFunc, closestWarp, inverseTempoFunc
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Derive.Stack as Stack
import qualified Derive.Warp as Warp
import qualified Perform.Transport as Transport
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

import Global
import Types


-- | Collected warp for a single track.
-- start end warp block_id (tempo track if there is one)
data Track = Track !RealTime !RealTime !Warp.Warp !BlockId !(Maybe TrackId)
    deriving (Show)

instance Pretty Track where
    format (Track start end warp blockId trackId) =
        Pretty.format (start, end, warp, blockId, trackId)

instance DeepSeq.NFData Track where
    rnf (Track _ _ _ _ trackId) = DeepSeq.rnf trackId

-- | Each 'Track' is collected at the Stack of the track it represents.
-- A Track is only saved when the warp changes, which is likely a tempo
-- track.  'collect_warps' then fills in the rest of the tracks.
type WarpMap = Map Stack.Stack Track

-- | Each track warp is a warp indexed by the block and tracks it covers.
-- These are used by the play monitor to figure out where the play position
-- indicator is at a given point in real time.
data TrackWarp = TrackWarp
    -- | The range over which this warp's 'tw_warp' can be used.
    { twStart :: !RealTime
    , twEnd :: !RealTime
    , twBlock :: !BlockId
    , twTracks :: !(Set TrackId)
    , twWarp :: !Warp.Warp
    } deriving (Show)

instance Pretty TrackWarp where
    format (TrackWarp start end block tracks warp) = Pretty.record "TrackWarp"
        [ ("start", Pretty.format start)
        , ("end", Pretty.format end)
        , ("block", Pretty.format block)
        , ("tracks", Pretty.format tracks)
        , ("warp", Pretty.format warp)
        ]

instance DeepSeq.NFData TrackWarp where
    rnf tw = DeepSeq.rnf (twTracks tw) `seq` DeepSeq.rnf (twWarp tw)

convert :: (Track, [TrackId]) -> TrackWarp
convert (Track start end warp blockId maybeTrackId, tracks) =
    TrackWarp
        { twStart = start
        , twEnd = end
        , twBlock = blockId
        , twTracks = Set.fromList trackIds
        , twWarp = warp
        }
    where trackIds = maybe tracks (:tracks) maybeTrackId

-- | Collect 'Track's together into TrackWarps'.
collectTrackWarps :: [(BlockId, [Tree.Tree TrackId])] -> WarpMap
    -> [TrackWarp]
collectTrackWarps blocks =
    filter (not . Set.null . twTracks) . map convert . collectWarps blocks
    -- There will be a TrackWarp with a null 'tw_tracks' if there are multiple
    -- tempo tracks at the top level.

getTrackTrees :: Ui.M m => m [(BlockId, [Tree.Tree TrackId])]
getTrackTrees = do
    blockIds <- Ui.allBlockIds
    zip blockIds . fmap (fmap (fmap Ui.trackId)) <$>
        mapM TrackTree.trackTreeOf blockIds

{- | The WarpMap only has TrackWarps for tempo tracks.  But I want to have
    playback cursors on all tracks, and be able to start play from any track.
    So this will extend a Track of a block or a track to all of its
    children.  This assumes that no one else is fiddling with the Warp.

    Previously I would collect TrackWarps on every track, which is more
    technically correct.  However, due to note inversion, that wounds up
    collecting a Track for every single note, and just sorting all of the
    stacks was at the top of the profile output.
-}
collectWarps :: [(BlockId, [Tree.Tree TrackId])] -> WarpMap
    -> [(Track, [TrackId])]
collectWarps blocks wmap =
    [(tw, getChildren stack) | (stack, tw) <- Map.toList wmap]
    where
    getChildren stack = maybe [] childTracks $ case getBlockTrack stack of
        Just (blockId, Nothing) -> Map.lookup blockId blockChildren
        Just (blockId, Just trackId) ->
            Map.lookup (blockId, trackId) trackChildren
        Nothing -> Nothing
    getBlockTrack stack = case Stack.toUiInnermost stack of
        (Just blockId, trackId, _) : _ -> Just (blockId, trackId)
        _ -> Nothing
    -- If a block doesn't have a toplevel tempo track, it gets an implicit
    -- one, which of course won't have its own TrackId.
    blockChildren = Map.fromList
        [ (blockId, tracks)
        | (blockId, tracks) <- blocks
        ]
    trackChildren = Map.fromList
        [ ((blockId, trackId), children)
        | (blockId, tracks) <- blocks
        , Tree.Node trackId children <- tracks
        ]
    -- Get all child TrackIds, but stop as soon as I hit another tempo track.
    childTracks = concatMap $ \(Tree.Node trackId children) ->
        if trackId `Set.member` tempoTracks then []
            else trackId : childTracks children
    tempoTracks = Set.fromList $
        mapMaybe (maybe Nothing snd . getBlockTrack) $ Map.keys wmap

-- * functions on TrackWarp

tempoFunc :: [TrackWarp] -> Transport.TempoFunction
tempoFunc trackWarps blockId trackId pos = map (flip Warp.warp pos) warps
    where
    warps = [twWarp tw | tw <- trackWarps, twBlock tw == blockId,
        Set.member trackId (twTracks tw)]

-- | If a block is called in multiple places, a score time on it may occur at
-- multiple real times.  Find the Warp which is closest to a given RealTime, or
-- the ID warp if there are none.
--
-- Pick the real time from the given selection which is
-- closest to the real time of the selection on the root block.
--
-- Return the first real time if there's no root or it doesn't have
-- a selection.
--
-- This can't use Transport.TempoFunction because I need to pick the
-- appropriate Warp and then look up multiple ScoreTimes in it.
closestWarp :: [TrackWarp] -> Transport.ClosestWarpFunction
closestWarp trackWarps blockId trackId pos =
    maybe Warp.identity (twWarp . snd) $
        Lists.minimumOn (abs . subtract pos . fst) annotated
    where
    annotated = zip (map twStart warps) warps
    warps = [tw | tw <- trackWarps, twBlock tw == blockId,
        Set.member trackId (twTracks tw)]

-- | Take RealTime back to the TrackTimes on the various blocks that it
-- corresponds to.
inverseTempoFunc :: [TrackWarp] -> Transport.InverseTempoFunction
inverseTempoFunc trackWarps stop realtime = do
    (blockId, trackIds, pos) <- trackPos
    pure (blockId, [(trackId, pos) | trackId <- Set.toList trackIds])
    where
    -- Ornaments and leading keyswitches can result in starting at a negative
    -- time.  But if this function returns [] the play monitor thread will take
    -- that to mean the performance is over.
    ts = max 0 realtime
    -- ts <= tw_end means that you can get the ScoreTime for the end of
    -- a block.  This is useful because then "Cmd.StepPlay" can step to the
    -- very end.
    trackPos =
        [ (twBlock tw, twTracks tw, Warp.unwarp (twWarp tw) ts)
        | tw <- trackWarps
        , twStart tw <= ts && (stop == Transport.NoStop || ts <= twEnd tw)
        ]
