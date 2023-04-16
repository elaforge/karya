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
    , collect_track_warps
    , get_track_trees
    -- * functions on TrackWarp
    , tempo_func, closest_warp, inverse_tempo_func
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
    format (Track start end warp block_id track_id) =
        Pretty.format (start, end, warp, block_id, track_id)

instance DeepSeq.NFData Track where
    rnf (Track _ _ _ _ track_id) = DeepSeq.rnf track_id

-- | Each 'Track' is collected at the Stack of the track it represents.
-- A Track is only saved when the warp changes, which is likely a tempo
-- track.  'collect_warps' then fills in the rest of the tracks.
type WarpMap = Map Stack.Stack Track

-- | Each track warp is a warp indexed by the block and tracks it covers.
-- These are used by the play monitor to figure out where the play position
-- indicator is at a given point in real time.
data TrackWarp = TrackWarp {
    -- | The range over which this warp's 'tw_warp' can be used.
    tw_start :: !RealTime
    , tw_end :: !RealTime
    , tw_block :: !BlockId
    , tw_tracks :: !(Set TrackId)
    , tw_warp :: !Warp.Warp
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
    rnf tw = DeepSeq.rnf (tw_tracks tw) `seq` DeepSeq.rnf (tw_warp tw)

convert :: (Track, [TrackId]) -> TrackWarp
convert (Track start end warp block_id maybe_track_id, tracks) =
    TrackWarp
        { tw_start = start
        , tw_end = end
        , tw_block = block_id
        , tw_tracks = Set.fromList track_ids
        , tw_warp = warp
        }
    where track_ids = maybe tracks (:tracks) maybe_track_id

-- | Collect 'Track's together into TrackWarps'.
collect_track_warps :: [(BlockId, [Tree.Tree TrackId])] -> WarpMap
    -> [TrackWarp]
collect_track_warps blocks =
    filter (not . Set.null . tw_tracks) . map convert . collect_warps blocks
    -- There will be a TrackWarp with a null 'tw_tracks' if there are multiple
    -- tempo tracks at the top level.

get_track_trees :: Ui.M m => m [(BlockId, [Tree.Tree TrackId])]
get_track_trees = do
    block_ids <- Ui.all_block_ids
    zip block_ids . fmap (fmap (fmap Ui.track_id)) <$>
        mapM TrackTree.track_tree_of block_ids

{- | The WarpMap only has TrackWarps for tempo tracks.  But I want to have
    playback cursors on all tracks, and be able to start play from any track.
    So this will extend a Track of a block or a track to all of its
    children.  This assumes that no one else is fiddling with the Warp.

    Previously I would collect TrackWarps on every track, which is more
    technically correct.  However, due to note inversion, that wounds up
    collecting a Track for every single note, and just sorting all of the
    stacks was at the top of the profile output.
-}
collect_warps :: [(BlockId, [Tree.Tree TrackId])] -> WarpMap
    -> [(Track, [TrackId])]
collect_warps blocks wmap =
    [(tw, get_children stack) | (stack, tw) <- Map.toList wmap]
    where
    get_children stack = maybe [] child_tracks $ case get_block_track stack of
        Just (block_id, Nothing) -> Map.lookup block_id block_children
        Just (block_id, Just track_id) ->
            Map.lookup (block_id, track_id) track_children
        Nothing -> Nothing
    get_block_track stack = case Stack.to_ui_innermost stack of
        (Just block_id, track_id, _) : _ -> Just (block_id, track_id)
        _ -> Nothing
    -- If a block doesn't have a toplevel tempo track, it gets an implicit
    -- one, which of course won't have its own TrackId.
    block_children = Map.fromList
        [ (block_id, tracks)
        | (block_id, tracks) <- blocks
        ]
    track_children = Map.fromList
        [ ((block_id, track_id), children)
        | (block_id, tracks) <- blocks
        , Tree.Node track_id children <- tracks
        ]
    -- Get all child TrackIds, but stop as soon as I hit another tempo track.
    child_tracks = concatMap $ \(Tree.Node track_id children) ->
        if track_id `Set.member` tempo_tracks then []
            else track_id : child_tracks children
    tempo_tracks = Set.fromList $
        mapMaybe (maybe Nothing snd . get_block_track) $ Map.keys wmap

-- * functions on TrackWarp

tempo_func :: [TrackWarp] -> Transport.TempoFunction
tempo_func track_warps block_id track_id pos = map (flip Warp.warp pos) warps
    where
    warps = [tw_warp tw | tw <- track_warps, tw_block tw == block_id,
        Set.member track_id (tw_tracks tw)]

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
closest_warp :: [TrackWarp] -> Transport.ClosestWarpFunction
closest_warp track_warps block_id track_id pos =
    maybe Warp.identity (tw_warp . snd) $
        Lists.minimumOn (abs . subtract pos . fst) annotated
    where
    annotated = zip (map tw_start warps) warps
    warps = [tw | tw <- track_warps, tw_block tw == block_id,
        Set.member track_id (tw_tracks tw)]

-- | Take RealTime back to the TrackTimes on the various blocks that it
-- corresponds to.
inverse_tempo_func :: [TrackWarp] -> Transport.InverseTempoFunction
inverse_tempo_func track_warps stop realtime = do
    (block_id, track_ids, pos) <- track_pos
    return (block_id, [(track_id, pos) | track_id <- Set.toList track_ids])
    where
    -- Ornaments and leading keyswitches can result in starting at a negative
    -- time.  But if this function returns [] the play monitor thread will take
    -- that to mean the performance is over.
    ts = max 0 realtime
    -- ts <= tw_end means that you can get the ScoreTime for the end of
    -- a block.  This is useful because then "Cmd.StepPlay" can step to the
    -- very end.
    track_pos =
        [ (tw_block tw, tw_tracks tw, Warp.unwarp (tw_warp tw) ts)
        | tw <- track_warps
        , tw_start tw <= ts && (stop == Transport.NoStop || ts <= tw_end tw)
        ]
