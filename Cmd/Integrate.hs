module Cmd.Integrate (cmd_integrate) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import Types


cmd_integrate :: (Cmd.M m) => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf))
    | null (Cmd.perf_integrated perf) = return Cmd.Continue
    | otherwise = do
        integrated <- concatMapM (integrate block_id) (Cmd.perf_integrated perf)
        State.modify_block block_id $ \block -> block
            { Block.block_integrated_tracks = integrated }
        return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

integrate :: (Cmd.M m) => BlockId -> Derive.Integrated
    -> m [(TrackId, [Block.TrackDestination])]
integrate block_id integrated = do
    tracks <- Convert.convert (Derive.integrated_events integrated)
        (Derive.integrated_key integrated)
    case Derive.integrated_source integrated of
        Left block_id -> do
            integrate_block block_id tracks
            return []
        Right track_id -> integrate_tracks block_id track_id tracks

integrate_tracks :: (Cmd.M m) => BlockId -> TrackId -> Convert.Tracks
    -> m [(TrackId, [Block.TrackDestination])]
integrate_tracks block_id track_id tracks = do
    itracks <- Block.block_integrated_tracks <$> State.get_block block_id
    new_dests <- case filter ((==track_id) . fst) itracks of
        [] -> (:[]) <$> Merge.merge_tracks block_id tracks []
        dests -> mapM (Merge.merge_tracks block_id tracks . snd) dests
    Log.notice $ "integrated " ++ show track_id ++ " to: "
        ++ Pretty.pretty new_dests
    Cmd.derive_immediately [block_id]
    return $ map ((,) track_id) new_dests

-- | Look for blocks derived from this one and replace their contents, or
-- create a new block.
integrate_block :: (Cmd.M m) => BlockId -> Convert.Tracks -> m ()
integrate_block block_id tracks = do
    blocks <- State.gets State.state_blocks
    new_blocks <- case integrated_from block_id blocks of
        [] -> do
            (block_id, dests) <- Merge.create_block block_id tracks
            Create.view block_id
            return [(block_id, dests)]
        integrated -> forM integrated $ \(block_id, track_dests) ->
            ((,) block_id) <$> Merge.merge_block block_id tracks track_dests
    Log.notice $ "integrated " ++ show block_id ++ " to: "
        ++ Pretty.pretty (map fst new_blocks)
    forM_ new_blocks $ \(new_block_id, track_dests) ->
        State.modify_block new_block_id $ \block -> block
            { Block.block_integrated = Just (block_id, track_dests) }
    Cmd.derive_immediately (map fst new_blocks)
    where
    integrated_from source_block_id block_map =
        [ (block_id, dests)
        | (block_id, Just (source_block, dests)) <-
            zip (map fst blocks) (map (Block.block_integrated . snd) blocks)
        , source_block == source_block_id
        ]
        where blocks = Map.toList block_map
