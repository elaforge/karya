{- |

The C++ level and BlockC have no notion of "blocks" which may be shared between
block views.  The haskell State does have this notion, so it's this module's
job to distribute an operation on a block to all of the C++ block views that
are displaying that block.  Thus if this module has a bug, two views of one
block could get out of sync and display different data.  Hopefully that won't
happen.

-}
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

send = Trans.liftIO . Initialize.send_action

-- | Apply the update to the UI.
-- CreateView Updates will modify the State to add the ViewPtr
run_update :: Update.Update -> State.StateT IO ()
run_update (Update.ViewUpdate view_id Update.CreateView) = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    ruler_track <- tracklike_to_ctracklike (Block.block_ruler_track block)
    ctracks <- mapM (tracklike_to_ctracklike . fst) (Block.block_tracks block)
    send $ do
        BlockC.create_view view_id (Block.view_rect view)
            (Block.view_config view) (Block.block_config block) ruler_track
        -- add tracks and title
        forM_ (zip3 [0..] ctracks (map snd (Block.block_tracks block))) $
            \(i, ctrack, width) -> BlockC.insert_track view_id i ctrack width
        when (not (null (Block.block_title block))) $
            BlockC.set_title view_id (Block.block_title block)

run_update (Update.ViewUpdate view_id update) = do
    case update of
        Update.DestroyView -> send (BlockC.destroy_view view_id)
        Update.ViewSize rect -> send (BlockC.set_size view_id rect)
        Update.ViewConfig config -> send (BlockC.set_view_config view_id config)
        -- Previous equation should have gotten this, but ghc warning doesn't
        -- know that.
        Update.CreateView -> error "run_update: notreached"

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
            send $ mapM_
                (\view_id -> BlockC.insert_track view_id tracknum ctrack width)
                view_ids

run_update _ = undefined

tracklike_to_ctracklike track = case track of
    Block.T track_id ruler_id ->
        liftM2 BlockC.T (State.get_track track_id) (State.get_ruler ruler_id)
    Block.R ruler_id ->
        liftM BlockC.R (State.get_ruler ruler_id)
    Block.D divider -> return (BlockC.D divider)
