{- | The C++ level and BlockC have no notion of "blocks" which may be shared
between block views.  The haskell State does have this notion, so it's this
module's job to distribute an operation on a block to all of the C++ block
views that are displaying that block.

So if this module has a bug, two views of one block could get out of sync and
display different data.  Hopefully that won't happen.

-}
module Ui.Sync (BlockSamples, sync, set_play_position) where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import qualified App.Config as Config

import Ui.Types
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Track as Track
import qualified Ui.State as State
import qualified Ui.Update as Update


type BlockSamples = [(Block.BlockId, Track.TrackSamples)]

-- | Sync with the ui by applying the given updates to it.
sync :: State.State -> [Update.Update] -> BlockSamples ->
    IO (Maybe State.StateError)
sync state updates block_samples = do
    -- TODO: TrackUpdates can overlap.  Merge them together here.
    -- Technically I can also cancel out all TrackUpdates that only apply to
    -- newly created views, but this optimization is probably not worth it.
    result <- State.run state $
        do_updates block_samples (Update.sort updates)
    return $ case result of
        Left err -> Just err
        -- I reuse State.StateT for convenience, but run_update should
        -- not modify the State and hence shouldn't produce any updates.
        -- TODO Try to split StateT into ReadStateT and ReadWriteStateT to
        -- express this in the type?
        Right _ -> Nothing

do_updates :: BlockSamples -> [Update.Update] -> State.StateT IO ()
do_updates block_samples updates = do
    actions <- mapM (run_update block_samples) updates
    -- Trans.liftIO $ putStrLn ("run updates: " ++ show updates)
    Trans.liftIO (Ui.send_action (sequence_ actions))

-- | The play position selection bypasses all the usual State -> Diff -> Sync
-- stuff for a direct write to the UI.
--
-- This is because it happens asynchronously and would be noisy and inefficient
-- to work into the responder loop, and isn't part of the usual state that
-- should be saved anyway.
set_play_position :: Block.ViewId -> Maybe TrackPos -> IO ()
set_play_position view_id maybe_pos = do
    let csel = case maybe_pos of
            Nothing -> Nothing
            Just pos -> Just $ BlockC.CSelection Config.play_position_color
                (Block.Selection 0 pos 99 (TrackPos 0))
    Ui.send_action $
        BlockC.set_selection view_id Config.play_position_selnum csel

track_title (Block.TId track_id _) =
    fmap Track.track_title (State.get_track track_id)
track_title _ = return ""

block_window_title :: Block.ViewId -> Block.BlockId -> String
block_window_title view_id block_id =
    Id.show_ident view_id ++ " -- " ++ Id.show_ident block_id

get_samples :: Maybe Track.TrackSamples -> Block.TracklikeId -> Track.Samples
get_samples maybe_track_samples track = maybe Track.no_samples id $ do
    track_samples <- maybe_track_samples
    track_id <- Block.track_id_of track
    lookup track_id track_samples

-- | Apply the update to the UI.
-- CreateView Updates will modify the State to add the ViewPtr
run_update :: BlockSamples -> Update.Update -> State.StateT IO (IO ())
run_update block_samples (Update.ViewUpdate view_id Update.CreateView) = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    let maybe_track_samples = lookup (Block.view_block view) block_samples

    let tracks = Block.block_tracks block
    ctracks <- mapM State.get_tracklike tracks
    let widths = map Block.track_view_width (Block.view_tracks view)
    titles <- mapM track_title tracks

    let sels = Block.view_selections view
    csels <- mapM (\(selnum, sel) -> to_csel view_id selnum (Just sel))
        (Map.assocs sels)
    -- I manually sync the new empty view with its state.  It might reduce
    -- repetition to let Diff.diff do that by diffing against a state with an
    -- empty view, but this way seems less complicated.
    -- Sync: title, tracks, selection
    return $ do
        let title = block_window_title view_id (Block.view_block view)
        BlockC.create_view view_id title (Block.view_rect view)
            (Block.view_config view) (Block.block_config block)

        let track_info = List.zip5 [0..] tracks ctracks widths titles
        forM_ track_info $ \(tracknum, track, ctrack, width, title) -> do
            -- The 'get_samples' may imply some work evaluating 'block_samples'
            -- which will be serialized in the UI thread.  Should be ok though.
            BlockC.insert_track view_id tracknum ctrack
                (get_samples maybe_track_samples track) width
            unless (null title) $
                BlockC.set_track_title view_id tracknum title

        unless (null (Block.block_title block)) $
            BlockC.set_title view_id (Block.block_title block)
        forM_ (zip (Map.keys sels) csels) $ \(selnum, csel) ->
            BlockC.set_selection view_id selnum csel
        BlockC.set_status view_id (Block.show_status view)
        BlockC.set_zoom view_id (Block.view_zoom view)
        BlockC.set_track_scroll view_id (Block.view_track_scroll view)

run_update _ (Update.ViewUpdate view_id update) = do
    case update of
        -- The previous equation matches CreateView, but ghc warning doesn't
        -- figure that out.
        Update.CreateView -> error "run_update: notreached"
        Update.DestroyView -> return (BlockC.destroy_view view_id)
        Update.ViewSize rect -> return (BlockC.set_size view_id rect)
        Update.ViewConfig config -> return
            (BlockC.set_view_config view_id config)
        Update.Status status -> return (BlockC.set_status view_id status)
        Update.TrackScroll offset ->
            return (BlockC.set_track_scroll view_id offset)
        Update.Zoom zoom -> return (BlockC.set_zoom view_id zoom)
        Update.TrackWidth tracknum width -> return $
            BlockC.set_track_width view_id tracknum width
        Update.Selection selnum maybe_sel -> do
            csel <- to_csel view_id selnum maybe_sel
            return $ BlockC.set_selection view_id selnum csel

-- Block ops apply to every view with that block.
run_update block_samples (Update.BlockUpdate block_id update) = do
    view_ids <- fmap Map.keys (State.get_views_of block_id)
    let maybe_track_samples = lookup block_id block_samples
    case update of
        Update.BlockTitle title -> return $
            mapM_ (flip BlockC.set_title title) view_ids
        Update.BlockConfig config -> return $
            mapM_ (flip BlockC.set_model_config config) view_ids
        Update.RemoveTrack tracknum -> return $
            mapM_ (flip BlockC.remove_track tracknum) view_ids
        Update.InsertTrack tracknum tracklike_id width -> do
            ctrack <- State.get_tracklike tracklike_id
            return $ forM_ view_ids $ \view_id -> do
                BlockC.insert_track view_id tracknum ctrack
                    (get_samples maybe_track_samples tracklike_id) width
                case ctrack of
                    Block.T t _ -> unless (null (Track.track_title t)) $
                        -- Sync the title.  See the CreateView comment.
                        BlockC.set_track_title view_id tracknum
                            (Track.track_title t)
                    _ -> return ()

run_update block_samples (Update.TrackUpdate track_id update) = do
    blocks <- State.blocks_with_track track_id
    fmap sequence_ $ forM blocks $ \(block_id, tracknum, tracklike_id) -> do
        view_ids <- fmap Map.keys (State.get_views_of block_id)
        tracklike <- State.get_tracklike tracklike_id
        let maybe_track_samples = lookup block_id block_samples
            samples = get_samples maybe_track_samples tracklike_id
        fmap sequence_ $ forM view_ids $ \view_id -> case update of
            Update.TrackEvents low high ->
                return $ BlockC.update_track view_id tracknum tracklike
                    samples low high
            Update.TrackAllEvents ->
                return $ BlockC.update_entire_track view_id tracknum tracklike
                    samples
            Update.TrackTitle title ->
                return $ BlockC.set_track_title view_id tracknum title
            Update.TrackBg ->
                -- update_track also updates the bg color
                return $ BlockC.update_track view_id tracknum tracklike
                    samples (TrackPos 0) (TrackPos 0)
            Update.TrackRender ->
                return $ BlockC.update_entire_track view_id tracknum tracklike
                    samples

run_update _ (Update.RulerUpdate ruler_id) = do
    blocks <- State.blocks_with_ruler ruler_id
    fmap sequence_ $ forM blocks $ \(block_id, tracknum, tracklike_id) -> do
        view_ids <- fmap Map.keys (State.get_views_of block_id)
        tracklike <- State.get_tracklike tracklike_id
        -- A ruler track doesn't have samples so don't bother to look for them.
        let samples = get_samples Nothing tracklike_id
        fmap sequence_ $ forM view_ids $ \view_id -> return $
            BlockC.update_entire_track view_id tracknum tracklike samples

to_csel :: Block.ViewId -> Block.SelNum -> Maybe (Block.Selection)
    -> State.StateT IO (Maybe BlockC.CSelection)
to_csel view_id selnum maybe_sel = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    let color = Seq.at_err "selection colors"
            (Block.config_selection_colors (Block.block_config block))
            selnum
    return $ fmap (BlockC.CSelection color) maybe_sel
