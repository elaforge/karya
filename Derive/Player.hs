{- | A module with minimal dependencies that has player oriented types and
utilities.
-}
module Derive.Player where
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef

import qualified Util.Log as Log
import Ui.Types
import qualified Ui.Block as Block
import qualified Midi.Midi as Midi
import qualified Derive.Timestamp as Timestamp

-- TODO: PlayerM IO: State, Log, ...

-- | These go back to the responder loop from the render thread to notify it
-- about the player's state.
data Status = Status Block.BlockId PlayerStatus deriving (Eq, Show)
data PlayerStatus = Playing | Stopped | Died Exception.Exception
    -- TODO later have play status so it can move the selection
    deriving (Eq, Show)
type Chan = STM.TChan Status

-- | Data needed by the player thread.  This is created during app setup and
-- passed directly to the play cmds by the responder loop.  When the play is
-- started, it's incorporated into the play 'State'.
data Info = Info {
    -- | Channel to communicate back to the responder loop.
    info_player_chan :: Chan
    , info_midi_writer :: Midi.WriteMessage -> IO ()
    -- | Get current timestamp according to timing system.
    , info_get_current_timestamp :: IO Timestamp.Timestamp
    }

-- * Transport control

-- | Send msgs from the responder loop to the player thread.
newtype Transport = Transport (IORef.IORef TransportMsg)
instance Show Transport where
    show trans = "<Transport_Control>"

-- | Msgs to control the player thread.
--
-- Pause is tricky because I will have to halt streaming, flush buffers, and
-- remember where I stopped, and wait on a resume.  Then when its received,
-- find the stopping time and resume playing there.
data TransportMsg = Play | Stop | Pause | Resume deriving (Eq, Show)

send_transport (Transport trans) msg = IORef.writeIORef trans msg
check_transport (Transport trans) = IORef.readIORef trans


-- * play timing

-- | Return the TrackPos play position is at on the given block at the given
-- time.  A play position display thread should poll this at a given resolution
-- for all displayed blocks and update the play selection accordingly.  If the
-- given Timestamp is beyond the end of the block, return Nothing.
--
-- This is generated as the inverse of the tempo, i.e. at the "bottom" block
-- it's 1:1, and at above blocks it's warped according to the inverse of the
-- tempo warped from there.
type TempoMap = Block.BlockId -> Timestamp.Timestamp -> Maybe TrackPos


-- * state

-- | Access to info that's needed by a particular run of the player.
-- This is read-only, and shouldn't need to be modified.
data State = State {
    -- | Communicate out of the Player.
    state_player_chan :: Chan
    -- | Communicate into the Player.
    , state_transport :: Transport
    , state_midi_writer :: Midi.WriteMessage -> IO ()
    , state_block_id :: Block.BlockId

    -- | When play started.  Timestamps relative to the block start should be
    -- added to this to get absolute Timestamps.
    , state_timestamp_offset :: Timestamp.Timestamp
    }
state (Info chan writer get_ts) trans block_id = do
    ts <- get_ts
    return (State chan trans writer block_id ts)

write_status :: Chan -> PlayerStatus -> Block.BlockId -> IO ()
write_status chan status block_id = 
    STM.atomically $ STM.writeTChan chan (Status block_id status)
