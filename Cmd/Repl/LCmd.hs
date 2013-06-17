-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Cmds to modify cmd state.
module Cmd.Repl.LCmd where
import Util.Control
import qualified Cmd.Cmd as Cmd
import qualified Cmd.TimeStep as TimeStep
import qualified Perform.RealTime as RealTime


-- * edit

get_step :: Cmd.CmdL Text
get_step = TimeStep.show_time_step <$> get_time_step

set_step :: Text -> Cmd.CmdL ()
set_step = set_time_step <=< Cmd.require_right id . TimeStep.parse_time_step

get_time_step :: Cmd.CmdL TimeStep.TimeStep
get_time_step = Cmd.gets (Cmd.state_time_step . Cmd.state_edit)

set_time_step :: TimeStep.TimeStep -> Cmd.CmdL ()
set_time_step step = Cmd.modify_edit_state $
    \st -> st { Cmd.state_time_step = step }

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

-- * undo

get_history :: Cmd.CmdL Cmd.History
get_history = Cmd.gets Cmd.state_history
