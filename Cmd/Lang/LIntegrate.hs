module Cmd.Lang.LIntegrate where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Perf as Perf

import qualified Derive.Call.Integrate as Call.Integrate
import Types


block :: (Cmd.M m) => BlockId -> m ViewId
block block_id = do
    perf <- Cmd.get_performance block_id
    events <- Call.Integrate.unwarp block_id (Cmd.perf_events perf)
    key <- Perf.get_key block_id Nothing
    tracks <- Convert.convert events key
    (new_block_id, dests) <- Merge.create_block block_id tracks
    State.modify_block new_block_id $ \block -> block
        { Block.block_integrated = Just (block_id, dests) }
    Cmd.derive_immediately [new_block_id]
    Create.view new_block_id

edits :: (Cmd.M m) => BlockId -> TrackId -> m ([Event.Stack], [Merge.Edit])
edits block_id track_id = do
    block <- State.get_block block_id
    index <- Cmd.require_msg "track is not integrated from anywhere" $
        lookup track_id $ indices_of (Block.block_integrated block)
            (Block.block_integrated_tracks block)
    events <- State.get_all_events track_id
    let (deleted, edits) = Merge.diff_events index events
    return (Set.toList deleted, filter Merge.is_modified edits)

indices_of :: Maybe (BlockId, [Block.TrackDestination])
    -> [(TrackId, [Block.TrackDestination])] -> [(TrackId, Block.EventIndex)]
indices_of integrated integrated_tracks =
    block_indices integrated ++ concatMap track_indices integrated_tracks
    where
    block_indices = maybe [] (concatMap dest_indices . snd)
    track_indices = concatMap dest_indices . snd
    dest_indices (Block.TrackDestination note controls) =
        note : Map.elems controls
