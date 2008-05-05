{- | A module with minimal dependencies that has player oriented types and
utilities.
-}
module Derive.Player where
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef

import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Midi.Midi as Midi

-- TODO: PlayerM IO: State, Log, ...

-- | These go back to the responder loop from the render thread to notify it
-- about the player's state.
data Status = Status Block.BlockId PlayerStatus deriving (Eq, Show)
data PlayerStatus = Playing | Stopped String | Completed
    | Died Exception.Exception
    -- TODO later have play status so it can move the selection
    deriving (Eq, Show)
type Chan = STM.TChan Status

-- | Data needed by the player thread.  This is created during app setup and
-- passed directly to the play cmds by the responder loop.
data Info = Info {
    -- | Channel to communicate back to the responder loop.
    player_chan :: Chan
    , midi_writer :: Midi.WriteMessage -> IO ()
    }

-- * control

-- | Send msgs from the responder loop to the player thread.
newtype Control = Control (IORef.IORef ControlMsg)
instance Show Control where
    show control = "<PlayerControl>"
data ControlMsg = Play | Stop | Pause | Resume deriving (Eq, Show)

send (Control control) msg = IORef.writeIORef control msg


-- * state

data State = State {
    -- | Communicate out of the Player.
    state_player_chan :: Chan
    -- | Communicate into the Player.
    , state_control :: Control
    , state_midi_writer :: Midi.WriteMessage -> IO ()
    , state_block_id :: Block.BlockId
    }
state (Info chan writer) control block_id = State chan control writer block_id

check_control (State { state_control = Control c }) = IORef.readIORef c

write_status state status = do
    Log.notice $ "player status: " ++ show status
    STM.atomically $ STM.writeTChan (state_player_chan state)
        (Status (state_block_id state) status)
