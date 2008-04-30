module Cmd.Edit where
import Control.Monad

import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd

import qualified App.Config as Config


newtype PitchClass = PitchClass Int
    deriving (Show)

cmd_insert_pitch :: PitchClass -> Cmd.CmdM
cmd_insert_pitch pitch = do
    edit <- fmap Cmd.state_edit_mode Cmd.get_state
    when (not edit) Cmd.abort
    Log.debug $ "insert pitch " ++ show pitch
    return Cmd.Done

cmd_toggle_edit :: Cmd.CmdM
cmd_toggle_edit = do
    view_id <- Cmd.get_active_view
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    edit <- fmap Cmd.state_edit_mode Cmd.get_state

    Cmd.modify_state $ \st -> st { Cmd.state_edit_mode = not edit }
    State.set_block_config (Block.view_block view) $ (Block.block_config block)
        { Block.config_track_box_color = edit_color (not edit) }
    return Cmd.Done


edit_color True = Config.edit_color
edit_color False = Config.box_color
