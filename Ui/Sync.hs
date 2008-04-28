{- | The C++ level and BlockC have no notion of "blocks" which may be shared
between block views.  The haskell State does have this notion, so it's this
module's job to distribute an operation on a block to all of the C++ block
views that are displaying that block.

So if this module has a bug, two views of one block could get out of sync and
display different data.  Hopefully that won't happen.

-}
module Ui.Sync where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Initialize as Initialize

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Track as Track
import qualified Ui.State as State
import qualified Ui.Update as Update

-- | Sync with the ui by applying the given updates to it.
sync :: State.State -> [Update.Update] -> IO (Maybe State.StateError)
sync state updates = do
    -- TODO: TrackUpdates can overlap.  Merge them together here.
    result <- State.run state (mapM_ run_update updates)
    return $ case result of
        Left err -> Just err
        -- I reuse State.StateT for convenience, but run_update should
        -- not modify the State and hence shouldn't produce any updates.
        -- TODO Try to split StateT into ReadStateT and ReadWriteStateT to
        -- express this in the type?
        Right _ -> Nothing

send = Trans.liftIO . Initialize.send_action

track_title (Block.T track_id _) =
    fmap Track.track_title (State.get_track track_id)
track_title _ = return ""

-- | Apply the update to the UI.
-- CreateView Updates will modify the State to add the ViewPtr
run_update :: Update.Update -> State.StateT IO ()
run_update (Update.ViewUpdate view_id Update.CreateView) = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    ruler_track <- tracklike_to_ctracklike (Block.block_ruler_track block)
    ctracks <- mapM (tracklike_to_ctracklike . fst) (Block.block_tracks block)
    titles <- mapM (track_title . fst) (Block.block_tracks block)
    let sels = Block.view_selections view
    csels <- mapM (\(selnum, sel) -> to_csel view_id selnum (Just sel))
        (Map.assocs sels)
    -- I manually sync the new empty view with its state.  It might reduce
    -- repetition to let Diff.diff do that by diffing against a state with an
    -- empty view, but this way seems less complicated.
    -- Sync: title, tracks, selection
    send $ do
        BlockC.create_view view_id (Block.view_rect view)
            (Block.view_config view) (Block.block_config block) ruler_track
        let widths = map Block.track_view_width (Block.view_tracks view)
        forM_ (List.zip4 [0..] ctracks widths titles) $
            \(tracknum, ctrack, width, title) -> do
                BlockC.insert_track view_id tracknum ctrack width
                when (not (null title)) $
                    BlockC.set_track_title view_id tracknum title

        when (not (null (Block.block_title block))) $
            BlockC.set_title view_id (Block.block_title block)
        forM_ (zip (Map.keys sels) csels) $ \(selnum, csel) ->
            BlockC.set_selection view_id selnum csel

run_update (Update.ViewUpdate view_id update) = do
    case update of
        -- The previous equation matches CreateView, but ghc warning doesn't
        -- figure that out.
        Update.CreateView -> error "run_update: notreached"
        Update.DestroyView -> send (BlockC.destroy_view view_id)
        Update.ViewSize rect -> send (BlockC.set_size view_id rect)
        Update.ViewConfig config -> send (BlockC.set_view_config view_id config)
        Update.TrackWidth tracknum width -> send $
            BlockC.set_track_width view_id tracknum width
        Update.Selection selnum maybe_sel -> do
            csel <- to_csel view_id selnum maybe_sel
            send $ BlockC.set_selection view_id selnum csel

-- Block ops apply to every view with that block.
run_update (Update.BlockUpdate block_id update) = do
    view_ids <- State.get_view_ids_of block_id
    case update of
        Update.BlockTitle title ->
            mapM_ (send . flip BlockC.set_title title) view_ids
        Update.BlockConfig config ->
            mapM_ (send . flip BlockC.set_model_config config) view_ids
        Update.RemoveTrack tracknum ->
            mapM_ (send . flip BlockC.remove_track tracknum) view_ids
        Update.InsertTrack tracknum track width -> do
            ctrack <- tracklike_to_ctracklike track
            send $ forM_ view_ids $ \view_id -> do
                BlockC.insert_track view_id tracknum ctrack width
                case ctrack of
                    BlockC.T t _ -> when (not (null (Track.track_title t))) $
                        -- Sync the title.  See the CreateView comment.
                        BlockC.set_track_title view_id tracknum
                            (Track.track_title t)
                    _ -> return ()

run_update (Update.TrackUpdate track_id update) = do
    blocks <- blocks_with_track track_id
    forM_ blocks $ \(block_id, tracknum, tracklike, _width) -> do
        view_ids <- State.get_view_ids_of block_id
        forM_ view_ids $ \view_id -> case update of
            Update.TrackEvents low high -> do
                ctrack <- tracklike_to_ctracklike tracklike
                send $ BlockC.update_track view_id tracknum ctrack low high
            Update.TrackTitle title ->
                send $ BlockC.set_track_title view_id tracknum title
            Update.TrackBg -> do
                ctrack <- tracklike_to_ctracklike tracklike
                -- update_track also updates the bg color
                send $ BlockC.update_track view_id tracknum ctrack
                    (TrackPos 0) (TrackPos 0)

to_csel :: Block.ViewId -> Block.SelNum -> Maybe (Block.Selection)
    -> State.StateT IO (Maybe BlockC.CSelection)
to_csel view_id selnum maybe_sel = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    let color = Seq.at_err "selection colors"
            (Block.config_selection_colors (Block.block_config block))
            selnum
    return $ fmap (BlockC.CSelection color) maybe_sel

-- | Find 'track_id' in all the blocks it exists in, and return the relevant
-- info.
blocks_with_track :: (Monad m) => Track.TrackId -> State.StateT m
    [(Block.BlockId, Block.TrackNum, Block.Tracklike, Block.Width)]
blocks_with_track track_id = do
    st <- State.get
    return [(block_id, i, tracklike, width) |
            (block_id, block) <- Map.assocs (State.state_blocks st),
            (i, (tracklike@(Block.T block_tid _block_rid), width))
                <- Seq.enumerate (Block.block_tracks block),
            track_id == block_tid]

tracklike_to_ctracklike track = case track of
    Block.T track_id ruler_id ->
        liftM2 BlockC.T (State.get_track track_id) (State.get_ruler ruler_id)
    Block.R ruler_id ->
        liftM BlockC.R (State.get_ruler ruler_id)
    Block.D divider -> return (BlockC.D divider)
