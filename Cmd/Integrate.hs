module Cmd.Integrate (cmd_integrate) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import Types


cmd_integrate :: (Cmd.M m) => Msg.Msg -> m Cmd.Status
cmd_integrate (Msg.DeriveStatus block_id (Msg.DeriveComplete perf))
    | null (Cmd.perf_integrated perf) = return Cmd.Continue
    | otherwise = do
        tracks <- mapM (integrate block_id) (Cmd.perf_integrated perf)
        State.modify_block block_id $ \block -> block
            { Block.block_integrated_tracks = concat tracks }
        return Cmd.Continue
cmd_integrate _ = return Cmd.Continue

integrate :: (Cmd.M m) => BlockId -> Derive.Integrated
    -> m [Block.IntegratedTrack]
integrate block_id integrated = do
    tracks <- Convert.convert (Derive.integrated_events integrated)
        (Derive.integrated_key integrated)
    case Derive.integrated_source integrated of
        Left block_id -> do
            integrate_block block_id tracks
            return []
        Right track_id -> integrate_track block_id track_id tracks

integrate_track :: (Cmd.M m) => BlockId -> TrackId -> Convert.Tracks
    -> m [Block.IntegratedTrack]
integrate_track block_id track_id tracks = do
    -- clear_score_damage source_tracks
    itracks <- Block.block_integrated_tracks <$>
        State.get_block block_id
    -- 'news' is a list of the new groups of TrackIds.
    news <- case filter ((==track_id) . Block.integrated_source) itracks of
        [] -> do
            tracknum <- State.track_count block_id
            (:[]) <$> Merge.create_tracks block_id tracknum tracks
        integrateds -> do
            forM_ integrateds $ \i ->
                Merge.merge_tracks tracks block_id
                    (Block.integrated_destinations i)
                    (Block.integrated_track_index i)
            return $ map Block.integrated_destinations integrateds
    Log.notice $ "integrated " ++ show track_id ++ " to: "
        ++ Seq.join "; " (map show news)
    let index = make_index tracks
    return [Block.IntegratedTrack track_id new index | new <- news]

make_index :: Convert.Tracks -> Block.EventIndex
make_index =
    Merge.make_index . concatMap Convert.track_events . concatMap flatten
    where flatten (note, controls) = note : controls

-- | Look for blocks derived from this one and replace their contents and mark
-- them as damaged, or create a new block.
integrate_block :: (Cmd.M m) => BlockId -> Convert.Tracks -> m ()
integrate_block block_id tracks = do
    blocks <- State.gets State.state_blocks
    let integrated = integrated_from block_id blocks
    new_blocks <- if null integrated
        then (:[]) <$> Merge.create_block block_id tracks
        else do
            sequence_ [Merge.merge_block tracks block_id index
                | (block_id, index) <- integrated]
            return $ map fst integrated
    Log.notice $ "integrated " ++ show block_id ++ " to: " ++ show new_blocks
    let index = make_index tracks
    forM_ new_blocks $ \new_block_id ->
        State.modify_block new_block_id $ \block -> block
            { Block.block_integrated = Just (Block.Integrated block_id index) }
    where
    integrated_from source_block_id block_map =
        [ (block_id, Block.integrated_index integrated)
        | (block_id, Just integrated) <-
            zip (map fst blocks) (map (Block.block_integrated . snd) blocks)
        , Block.integrated_block integrated == source_block_id
        ]
        where blocks = Map.toList block_map
