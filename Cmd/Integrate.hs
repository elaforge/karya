module Cmd.Integrate where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Debug as Debug
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.TrackInfo as TrackInfo
import Types


cmd_integrate :: (Cmd.M m) => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf)) = do
    when_just (Cmd.perf_integrated perf) (integrate block_id)
    return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

-- | Look for blocks derived from this one and replace their contents and mark
-- them as damaged, or create a new block.
integrate :: (Cmd.M m) => BlockId -> [Derive.Track] -> m ()
integrate block_id tracks = do
    blocks <- State.gets State.state_blocks
    let integrated = map fst $
            filter ((== Just block_id) .  Block.block_integrated . snd)
                (Map.toList blocks)
    Log.warn $ "integrated " ++ show block_id ++ ": " ++ show integrated
    if null integrated then void $ create block_id tracks
        else mapM_ (merge block_id tracks) integrated

-- ** create

create :: (State.M m) => BlockId -> [Derive.Track] -> m BlockId
create source_block_id tracks = do
    ruler_id <- State.get_block_ruler source_block_id
    block_id <- Create.block ruler_id
    fill_block source_block_id block_id tracks
    return block_id

fill_block :: (State.M m) => BlockId -> BlockId -> [Derive.Track] -> m ()
fill_block source_block_id block_id tracks = do
    mapM_ (create_track block_id) (zip [1..] tracks)
    State.set_skeleton block_id $ Skeleton.make $
        -- +1 to account for the ruler track.
        make_edges (length tracks + 1) note_tracks
    State.modify_block block_id $ \block ->
        block { Block.block_integrated = Just source_block_id }
    where
    create_track block_id (tracknum, (Derive.Track title events)) = do
        Create.track block_id tracknum title (Events.from_asc_list events)
    note_tracks =
        [tracknum | (tracknum, Derive.Track title _) <- zip [1..] tracks,
            TrackInfo.is_note_track title]

-- | 6 [1, 4] -> [(1, 2), (2, 3), (4, 5)]
make_edges :: TrackNum -> [TrackNum] -> [(TrackNum, TrackNum)]
make_edges track_count = concatMap interpolate . Seq.zip_next
    where
    interpolate (t1, maybe_t2) = zip ts (drop 1 ts)
        where ts = [t1 .. Maybe.fromMaybe track_count maybe_t2 - 1]

-- ** merge

merge :: (Cmd.M m) => BlockId -> [Derive.Track] -> BlockId -> m ()
merge source_block_id tracks block_id = do
    -- TODO merge for real
    ntracks <- State.track_count block_id
    mapM_ (State.remove_track block_id) [ntracks, ntracks-1 .. 1]
    void $ fill_block source_block_id block_id tracks
