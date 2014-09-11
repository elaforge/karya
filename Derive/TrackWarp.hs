-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | TrackWarps are collected throughout derivation each time there is a new
    warp context.  By the end, they represent a complete mapping from ScoreTime
    to RealTime and back again, and can be used to create a TempoFunction and
    InverseTempoFunction, among other things.
-}
module Derive.TrackWarp (
    TrackWarp(..), WarpMap, Collection(..)
    , collections
    -- * functions on Collection
    , tempo_func, closest_warp, inverse_tempo_func
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Perform.Transport as Transport
import Types


newtype TrackWarp =
    -- | (start, end, warp, block_id, track of tempo track if there is one)
    TrackWarp (RealTime, RealTime, Score.Warp, BlockId, Maybe TrackId)
    deriving (Eq, Show, Pretty.Pretty, DeepSeq.NFData)

-- | Each TrackWarp is collected at the Stack of the track it represents.
-- A Left is a new TrackWarp and a Right is a track that uses the warp in
-- the environment provided by its callers.  Later they will all be collected
-- into a Collection.
--
-- The reason I don't simply save the warps at every track is that many tracks
-- share the same warp, and it's more efficient to consolidate them, yet I
-- don't want to directly compare warp signals because they may be large or
-- lazy.
type WarpMap = Map.Map Stack.Stack (Either TrackWarp TrackId)

-- | Each track warp is a warp indexed by the block and tracks it covers.
-- These are used by the play monitor to figure out where the play position
-- indicator is at a given point in real time.
data Collection = Collection {
    tw_start :: !RealTime
    , tw_end :: !RealTime
    , tw_block :: !BlockId
    , tw_tracks :: !(Set.Set TrackId)
    , tw_warp :: !Score.Warp
    } deriving (Eq, Show)

instance Pretty.Pretty Collection where
    format (Collection start end block tracks warp) = Pretty.record "Collection"
        [ ("start", Pretty.format start)
        , ("end", Pretty.format end)
        , ("block", Pretty.format block)
        , ("tracks", Pretty.format tracks)
        , ("warp", Pretty.format warp)
        ]

instance DeepSeq.NFData Collection where
    rnf tw = DeepSeq.rnf (tw_tracks tw) `seq` DeepSeq.rnf (tw_warp tw)

collections :: WarpMap -> [Collection]
collections = map convert . collect_warps

convert :: (TrackWarp, [TrackId]) -> Collection
convert (TrackWarp (start, end, warp, block_id, maybe_track_id), tracks) =
    Collection
        { tw_start = start
        , tw_end = end
        , tw_block = block_id
        , tw_tracks = Set.fromList track_ids
        , tw_warp = warp
        }
    where track_ids = maybe tracks (:tracks) maybe_track_id

collect_warps :: WarpMap -> [(TrackWarp, [TrackId])]
    -- TODO The first result will be the dummy TrackWarp with parentless
    -- tracks as children, which shouldn't happen.  Warn about them anyway?
collect_warps wmap = drop 1 $ map drop_stack $ collect [] dummy_tw assocs
    where
    assocs = Seq.sort_on fst $ map (first Stack.outermost) $ Map.assocs wmap
    drop_stack (_, tw, tracks) = (tw, tracks)
    dummy_tw = TrackWarp (0, 0, Score.id_warp, no_block, Nothing)
    no_block = Id.BlockId (Id.global "-dummy-trackwarp-")

type Frames = [Stack.Frame]

-- | Group a list of stacks into a @(stack, parent, children)@ triples.
-- A Left is a parent, and will collect the Rights prefixed by its stack.
collect :: Frames -> a -> [(Frames, Either a b)] -> [(Frames, a, [b])]
collect prefix a stacks = (prefix, a, bs)
    : concat [collect pref sub_a substacks | (sub_a, pref, substacks) <- subs]
    where
    (subs, bs) = Seq.partition_either (split stacks)
    split [] = []
    split ((stack, Left a) : rest) = Left (a, stack, substacks) : split rest2
        where (substacks, rest2) = span ((stack `List.isPrefixOf`) . fst) rest
    split ((_, Right b) : rest) = Right b : split rest


-- * functions on Collection

tempo_func :: [Collection] -> Transport.TempoFunction
tempo_func track_warps block_id track_id pos =
    map (flip Score.warp_pos pos) warps
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
closest_warp :: [Collection] -> Transport.ClosestWarpFunction
closest_warp track_warps block_id track_id pos =
    maybe Score.id_warp (tw_warp . snd) $
        Seq.minimum_on (abs . subtract pos . fst) annotated
    where
    annotated = zip (map tw_start warps) warps
    warps = [tw | tw <- track_warps, tw_block tw == block_id,
        Set.member track_id (tw_tracks tw)]

inverse_tempo_func :: [Collection] -> Transport.InverseTempoFunction
inverse_tempo_func track_warps time = do
    (block_id, track_ids, Just pos) <- track_pos
    return (block_id, [(track_id, pos) | track_id <- Set.toList track_ids])
    where
    -- Ornaments and leading keyswitches can result in starting at a negative
    -- time.  But if this function returns [] the play monitor thread will take
    -- that to mean the performance is over.
    ts = max 0 time
    -- ts <= tw_end means that you can get the ScoreTime for the end of
    -- a block.  This is useful because then "Cmd.StepPlay" can step to the
    -- very end.
    track_pos = [(tw_block tw, tw_tracks tw, Score.unwarp_pos (tw_warp tw) ts)
        | tw <- track_warps, tw_start tw <= ts && ts <= tw_end tw]

    -- TODO Comment is obsolete, but I may want to add it back in if I go
    -- back to using 'Perf.find_play_pos' for the step playback.
    --
    -- The guard is for @ts <= tw_end tw@ rather than @<@ which would be
    -- consistent with the rest of the half-open ranges.  However, it's
    -- convenient to have the RealTime corresponding to the end of the last
    -- msg produce a ScoreTime.  Specifically, "Cmd.StepPlay" wants to do
    -- this.
