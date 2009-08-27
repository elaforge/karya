-- | Cmds that affect global block config but don't fit into any of the
-- more specefic modules.
module Cmd.BlockConfig where
import Control.Monad

import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection


cmd_toggle_edge :: (Monad m) => Msg.Msg -> Cmd.CmdT m Cmd.Status
cmd_toggle_edge msg = do
    (_, sel_track_num, _) <- Selection.get_insert_track
    clicked_track_num <- get_clicked_track msg
    block_id <- Cmd.get_focused_block
    let edge = (clicked_track_num, sel_track_num)
    failed <- State.toggle_skeleton_edge block_id edge
    when failed $
        Log.warn $ "refused to add cycle-creating edge: " ++ show edge
    -- TODO: set selection so you can chain these
    return Cmd.Done

get_clicked_track :: (Monad m) => Msg.Msg
    -> Cmd.CmdT m Block.TrackNum
get_clicked_track msg = case Cmd.msg_to_mod msg of
    Just (True, Cmd.MouseMod _ (Just (tracknum, _))) -> return tracknum
    _ -> Cmd.abort
