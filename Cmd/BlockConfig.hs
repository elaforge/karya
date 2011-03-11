-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig where
import Control.Monad
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Tree as Tree

import Ui
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.ViewConfig as ViewConfig
import qualified Cmd.NoteTrack as NoteTrack


-- * block

cmd_toggle_edge :: (Cmd.M m) => Msg.Msg -> m ()
cmd_toggle_edge msg = do
    (block_id, sel_track_num, _, _) <- Selection.get_insert
    clicked_track_num <- get_clicked_track msg
    let edge = (clicked_track_num, sel_track_num)
    success <- State.toggle_skeleton_edge block_id edge
    when (not success) $
        Log.warn $ "refused to add cycle-creating edge: " ++ show edge
    -- TODO: set selection so you can chain these

get_clicked_track :: (Cmd.M m) => Msg.Msg -> m TrackNum
get_clicked_track msg = case Cmd.msg_to_mod msg of
    Just (True, Cmd.MouseMod _ (Just (tracknum, _))) -> return tracknum
    _ -> Cmd.abort

-- | Merge all adjacent note/pitch pairs.
merge_all :: (Cmd.M m) => BlockId -> m ()
merge_all block_id = do
    tree <- State.get_track_tree block_id
    let collapse = Seq.map_maybe collapsable (Tree.paths tree)
    mapM_ (uncurry (State.merge_track block_id)) collapse
    where
    collapsable (track, parent : _, [])
        | num parent == num track + 1 = Just (num track, num parent)
    collapsable _ = Nothing
    num = State.track_tracknum

cmd_open_block :: (Cmd.M m) => m ()
cmd_open_block = do
    ns <- State.get_project
    let call_of = NoteTrack.block_call ns
    sel <- Selection.events
    forM_ sel $ \(_, _, events) -> forM_ events $ \(_, event) ->
        when_just (call_of (Event.event_string event)) $ \block_id -> do
            views <- State.get_views_of block_id
            maybe (Create.view block_id >> return ()) ViewConfig.bring_to_front
                (Seq.head (Map.keys views))

-- * track

cmd_toggle_flag :: (Cmd.M m) => Block.TrackFlag -> m ()
cmd_toggle_flag flag = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    State.toggle_track_flag block_id tracknum flag
