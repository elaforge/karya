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
imPlayDirect :: Cmd.M m => m Bool
imPlayDirect = do
    Cmd.modify $ \st -> st
        { Cmd.stateConfig = (Cmd.stateConfig st)
            { Cmd.configImPlayDirect =
                not (Cmd.configImPlayDirect (Cmd.stateConfig st))
            }
        }
    Cmd.gets $ Cmd.configImPlayDirect . Cmd.stateConfig

-- * edit

getStep :: Cmd.CmdL Text
getStep = TimeStep.showTimeStep <$> getTimeStep

setStep :: Text -> Cmd.CmdL ()
setStep = setTimeStep <=< Cmd.requireRight id . TimeStep.parseTimeStep

getTimeStep :: Cmd.CmdL TimeStep.TimeStep
getTimeStep = Cmd.gets (Cmd.stateTimeStep . Cmd.stateEdit)

setTimeStep :: TimeStep.TimeStep -> Cmd.CmdL ()
setTimeStep step = Cmd.modifyEditState $ \st ->
    st { Cmd.stateTimeStep = step }

setNoteDuration :: TimeStep.TimeStep -> Cmd.CmdL ()
setNoteDuration step = Cmd.modifyEditState $ \st ->
    st { Cmd.stateNoteDuration = step }

-- | Set the note duration to the time step.
durToStep :: Cmd.CmdL ()
durToStep = setNoteDuration =<< getTimeStep

durToEnd :: Cmd.CmdL ()
durToEnd = setNoteDuration $ TimeStep.timeStep TimeStep.BlockEdge

-- | Set play step to current step.
setPlayStep :: Cmd.CmdL ()
setPlayStep = do
    step <- Cmd.getCurrentStep
    Cmd.modifyPlayState $ \st -> st { Cmd.statePlayStep = step }

-- * play

setPlayMultiplier :: Double -> Cmd.CmdL ()
setPlayMultiplier d = Cmd.modifyPlayState $ \st ->
    st { Cmd.statePlayMultiplier = RealTime.seconds d }

-- * mmc

setSync :: Text -> Bool -> Cmd.CmdL ()
setSync dev mtc = Cmd.modifyPlayState $ \st -> st
    { Cmd.stateSync = Just $ Cmd.SyncConfig
        { Cmd.syncDevice = Midi.writeDevice dev
        , Cmd.syncDeviceId = 0x7f -- all devices
        , Cmd.syncMtc = mtc
        , Cmd.syncFrameRate = Midi.Frame30
        }
    }

unsetSync :: Cmd.CmdL ()
unsetSync = Cmd.modifyPlayState $ \st -> st { Cmd.stateSync = Nothing }

-- * hooks

addSelectionHook :: ([(ViewId, Maybe Cmd.TrackSelection)] -> Cmd.CmdId ())
    -> Cmd.CmdL ()
addSelectionHook hook = Cmd.modify $ \st -> st
    { Cmd.stateHooks = (Cmd.stateHooks st)
        { Cmd.hooksSelection = hook : Cmd.hooksSelection (Cmd.stateHooks st)
        }
    }

-- * undo

getHistory :: Cmd.CmdL Cmd.History
getHistory = Cmd.gets Cmd.stateHistory
