module Cmd.Lang.LIntegrate where
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
import qualified Derive.Derive as Derive
import Types


block :: (Cmd.M m) => BlockId -> m ViewId
block block_id = do
    perf <- Cmd.get_performance block_id
    events <- Call.Integrate.unwarp block_id (Cmd.perf_events perf)
    key <- Perf.get_key block_id Nothing
    tracks <- Convert.convert (Derive.Integrated (Left block_id) [] events key)
    new_block <- Merge.create block_id tracks
    Create.view new_block

edits :: (Cmd.M m) => BlockId -> m ([Event.Stack], [Merge.Edit])
edits block_id = do
    block <- State.get_block block_id
    integrated <- Cmd.require_msg
        "block is not integrated from anywhere" (Block.block_integrated block)
    events <- Merge.get_block_events block_id
    let (deleted, edits) =
            Merge.diff_events (Block.integrated_index integrated) events
    return (Set.toList deleted, filter Merge.is_modified edits)
