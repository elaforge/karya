-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection
import qualified Cmd.Track as Track
import qualified Cmd.ViewConfig as ViewConfig


-- * block

cmd_toggle_edge :: (Cmd.M m) => Msg.Msg -> m ()
cmd_toggle_edge msg = do
    (block_id, sel_track_num, _, _) <- Selection.get_insert
    clicked_track_num <- get_clicked_track msg
    let edge = (clicked_track_num, sel_track_num)
    success <- State.toggle_skeleton_edge block_id edge
    unless success $
        Log.warn $ "refused to add cycle-creating edge: " ++ show edge
    -- TODO: set selection so you can chain these

get_clicked_track :: (Cmd.M m) => Msg.Msg -> m TrackNum
get_clicked_track msg = case (Msg.mouse_down msg, Msg.context_track_pos msg) of
    (True, Just (tracknum, _)) -> return tracknum
    _ -> Cmd.abort

-- | Merge all adjacent note/pitch pairs.  If they're already all merged,
-- unmerge them all.
toggle_merge_all :: (State.M m) => BlockId -> m ()
toggle_merge_all block_id = do
    tree <- State.get_track_tree block_id
    tracks <- State.tracks block_id
    let collapse = Maybe.mapMaybe (collapsable tree) [0..tracks-1]
    let merged b (tracknum, _) = (b &&) <$> track_merged block_id tracknum
    ifM (foldM merged True collapse)
        (mapM_ (State.unmerge_track block_id) (map fst collapse))
        (mapM_ (uncurry (State.merge_track block_id)) collapse)
    where
    collapsable tree tracknum = case Track.get_track_type tree tracknum of
        Just (Track.NoteTrack (NoteTrack.ExistingTrack pitch_tracknum _)) ->
            Just (tracknum, pitch_tracknum)
        _ -> Nothing

track_merged :: (State.M m) => BlockId -> TrackNum -> m Bool
track_merged block_id tracknum =
    not . null . Block.track_merged <$> State.get_block_track block_id tracknum

cmd_open_block :: (Cmd.M m) => m ()
cmd_open_block = do
    ns <- State.get_namespace
    let call_of = NoteTrack.block_call ns
    sel <- Selection.events
    forM_ sel $ \(_, _, events) -> forM_ events $ \(_, event) ->
        when_just (call_of (Event.event_string event)) $ \block_id -> do
            views <- State.get_views_of block_id
            maybe (Create.fitted_view block_id >> return ())
                ViewConfig.bring_to_front (Seq.head (Map.keys views))

-- * track

cmd_toggle_flag :: (Cmd.M m) => Block.TrackFlag -> m ()
cmd_toggle_flag flag = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    State.toggle_track_flag block_id tracknum flag
