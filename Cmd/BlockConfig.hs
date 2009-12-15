-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig where
import Control.Monad

import qualified Util.Log as Log

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection


-- * block

cmd_toggle_edge :: (Monad m) => Msg.Msg -> Cmd.CmdT m ()
cmd_toggle_edge msg = do
    (block_id, sel_track_num, _, _) <- Selection.get_insert
    clicked_track_num <- get_clicked_track msg
    let edge = (clicked_track_num, sel_track_num)
    success <- State.toggle_skeleton_edge block_id edge
    when (not success) $
        Log.warn $ "refused to add cycle-creating edge: " ++ show edge
    -- TODO: set selection so you can chain these

get_clicked_track :: (Monad m) => Msg.Msg -> Cmd.CmdT m TrackNum
get_clicked_track msg = case Cmd.msg_to_mod msg of
    Just (True, Cmd.MouseMod _ (Just (tracknum, _))) -> return tracknum
    _ -> Cmd.abort


-- * track

cmd_toggle_flag :: (Monad m) => Block.TrackFlag -> Cmd.CmdT m ()
cmd_toggle_flag flag = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    State.toggle_track_flag block_id tracknum flag
