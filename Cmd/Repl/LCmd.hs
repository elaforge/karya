-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to modify cmd state.
module Cmd.Repl.LCmd where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.TimeStep as TimeStep
import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime

import           Global
import           Types


-- * config


-- | Temporarily set 'Cmd.config_im_play_direct', see
-- 'App.StaticConfig.im_play_direct' to set it permanently.
im_play_direct :: Cmd.M m => m Bool
im_play_direct = do
    Cmd.modify $ \st -> st
        { Cmd.state_config = (Cmd.state_config st)
            { Cmd.config_im_play_direct =
                not (Cmd.config_im_play_direct (Cmd.state_config st))
            }
        }
    Cmd.gets $ Cmd.config_im_play_direct . Cmd.state_config

-- * edit

get_step :: Cmd.CmdL Text
get_step = TimeStep.show_time_step <$> get_time_step

set_step :: Text -> Cmd.CmdL ()
set_step = set_time_step <=< Cmd.require_right id . TimeStep.parse_time_step

get_time_step :: Cmd.CmdL TimeStep.TimeStep
get_time_step = Cmd.gets (Cmd.state_time_step . Cmd.state_edit)

set_time_step :: TimeStep.TimeStep -> Cmd.CmdL ()
set_time_step step = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_time_step = step }

set_note_duration :: TimeStep.TimeStep -> Cmd.CmdL ()
set_note_duration step = Cmd.modify_edit_state $ \st ->
    st { Cmd.state_note_duration = step }

-- | Set the note duration to the time step.
dur_to_step :: Cmd.CmdL ()
dur_to_step = set_note_duration =<< get_time_step

dur_to_end :: Cmd.CmdL ()
dur_to_end = set_note_duration $ TimeStep.time_step TimeStep.BlockEdge

-- | Set play step to current step.
set_play_step :: Cmd.CmdL ()
set_play_step = do
    step <- Cmd.get_current_step
    Cmd.modify_play_state $ \st -> st { Cmd.state_play_step = step }

-- * play

set_play_multiplier :: Double -> Cmd.CmdL ()
set_play_multiplier d = Cmd.modify_play_state $ \st ->
    st { Cmd.state_play_multiplier = RealTime.seconds d }

-- * mmc

set_sync :: Text -> Bool -> Cmd.CmdL ()
set_sync dev mtc = Cmd.modify_play_state $ \st -> st
    { Cmd.state_sync = Just $ Cmd.SyncConfig
        { Cmd.sync_device = Midi.write_device dev
        , Cmd.sync_device_id = 0x7f -- all devices
        , Cmd.sync_mtc = mtc
        , Cmd.sync_frame_rate = Midi.Frame30
        }
    }

unset_sync :: Cmd.CmdL ()
unset_sync = Cmd.modify_play_state $ \st -> st { Cmd.state_sync = Nothing }

-- * hooks

add_selection_hook :: ([(ViewId, Maybe Cmd.TrackSelection)] -> Cmd.CmdId ())
    -> Cmd.CmdL ()
add_selection_hook hook = Cmd.modify $ \st -> st
    { Cmd.state_hooks = (Cmd.state_hooks st)
        { Cmd.hooks_selection = hook : Cmd.hooks_selection (Cmd.state_hooks st)
        }
    }

-- * undo

get_history :: Cmd.CmdL Cmd.History
get_history = Cmd.gets Cmd.state_history
