module Cmd.Integrate (cmd_integrate) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import Types


cmd_integrate :: (Cmd.M m) => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf)) = do
    integrate_block block_id (Cmd.perf_integrated perf)
    return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

-- | Look for blocks derived from this one and replace their contents and mark
-- them as damaged, or create a new block.
integrate_block :: (Cmd.M m) => BlockId -> [Derive.Integrated] -> m ()
integrate_block _ [] = return ()
integrate_block block_id integrated = do
    tracks <- concat <$> mapM Convert.convert integrated
    blocks <- State.gets State.state_blocks
    let integrated = integrated_from block_id blocks
    integrated_blocks <- if null integrated
        then (:[]) <$> Merge.create block_id tracks
        else do
            mapM_ (Merge.merge tracks) integrated
            return $ map fst integrated
    Log.notice $ "integrated " ++ show block_id ++ " to: "
        ++ show integrated_blocks
    let index = Merge.make_index (concatMap Convert.track_events tracks)
    forM_ integrated_blocks $ \integrated_block_id ->
        State.modify_block integrated_block_id $ \block -> block
            { Block.block_integrated = Just (Block.Integrated block_id index) }
    -- Hack to avoid rederive, see 'Cmd.state_suppress_rederive'.
    Cmd.modify $ \state -> state { Cmd.state_suppress_rederive =
        block_id : Cmd.state_suppress_rederive state }
    where
    integrated_from source_block_id block_map =
        [ (block_id, Block.integrated_index integrated)
        | (block_id, Just integrated) <-
            zip (map fst blocks) (map (Block.block_integrated . snd) blocks)
        , Block.integrated_block integrated == source_block_id
        ]
        where blocks = Map.toList block_map
