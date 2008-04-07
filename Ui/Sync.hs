module Ui.Sync where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Ui.Initialize as Initialize

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.State as State
import qualified Ui.Update as Update

{-
create and delete views: create_view, insert/remove track
redraw in areas where events where added / removed
although for the moment redrawing everywhere is fine
-}

-- | Sync with the ui by applying the given updates to it.
sync :: State.State -> [Update.Update]
    -> IO (Either State.StateError State.State)
sync state updates = do
    State.run state (mapM_ run_update updates)

send act = Trans.liftIO (Initialize.send_action act)

-- | Apply the update to the UI.
-- CreateView Updates will modify the State to add the ViewPtr
run_update :: Update.Update -> State.StateT IO ()
run_update (Update.ViewUpdate view_id Update.CreateView) = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    ruler_track <- tracklike_to_ctracklike (Block.block_ruler_track block)
    ctracks <- mapM (tracklike_to_ctracklike . fst) (Block.block_tracks block)
    viewp <- send $ do
        viewp <- BlockC.create_view (Block.view_rect view)
            (Block.view_config view) (Block.block_config block) ruler_track
        -- add tracks and title
        forM_ (zip3 [0..] ctracks (map snd (Block.block_tracks block))) $
            \(i, ctrack, width) -> BlockC.insert_track viewp i ctrack width
        when (not (null (Block.block_title block))) $
            BlockC.set_title viewp (Block.block_title block)
        return viewp
    State.add_view_ptr view_id viewp

run_update (Update.ViewUpdate view_id update) = do
    viewp <- State.get_view_ptr view_id
    case update of
        Update.DestroyView -> send (BlockC.destroy_view viewp)
        Update.ViewSize rect -> send (BlockC.set_size viewp rect)
        Update.ViewConfig config -> send (BlockC.set_view_config viewp config)

-- Block ops apply to every view with that block.
run_update (Update.BlockUpdate block_id update) = do
    viewps <- State.get_view_ptrs_of block_id
    case update of
        Update.BlockTitle title ->
            mapM_ (send . flip BlockC.set_title title) viewps
        Update.BlockConfig config ->
            mapM_ (send . flip BlockC.set_model_config config) viewps
        Update.RemoveTrack tracknum ->
            mapM_ (send . flip BlockC.remove_track tracknum) viewps
        Update.InsertTrack tracknum track width -> do
            ctrack <- tracklike_to_ctracklike track
            send $ mapM_
                (\viewp -> BlockC.insert_track viewp tracknum ctrack width)
                viewps

run_update _ = undefined

tracklike_to_ctracklike track = case track of
    Block.T track_id ruler_id ->
        liftM2 BlockC.T (State.get_track track_id) (State.get_ruler ruler_id)
    Block.R ruler_id ->
        liftM BlockC.R (State.get_ruler ruler_id)
    Block.D divider -> return (BlockC.D divider)
