{- | The transport is the communication mechanism between the app and the
    performer.  Extensive description is in the Cmd.Play docstring.
-}
module Perform.Transport where
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Midi.Midi as Midi
import qualified Perform.Timestamp as Timestamp


-- | These go back to the responder loop from the render thread to notify it
-- about the transport's state.
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
    info_responder_chan :: Chan
    , info_midi_writer :: Midi.WriteMessage -> IO ()
    -- | Get current timestamp according to timing system.
    , info_get_current_timestamp :: IO Timestamp.Timestamp
    }

-- * Transport control

-- | Send msgs from the responder loop to the player thread.
newtype Transport = Transport (IORef.IORef TransportMsg)
instance Show Transport where
    show _trans = "<Transport_Control>"

-- | Msgs to control the player thread.
--
-- Pause is tricky because I will have to halt streaming, flush buffers, and
-- remember where I stopped, and wait on a resume.  Then when its received,
-- find the stopping time and resume playing there.
data TransportMsg = Play | Stop | Pause | Resume deriving (Eq, Show)

send_transport (Transport trans) msg = IORef.writeIORef trans msg
check_transport (Transport trans) = IORef.readIORef trans


-- * play timing

-- | Given a pos on a certain track in a certain block, give the real time
-- that it corresponds to.  Nothing if I don't know for that block and track.
type TempoFunction = Block.BlockId -> Track.TrackId -> TrackPos
    -> Maybe Timestamp.Timestamp

-- | Return the TrackPos play position in the various playing blocks at the
-- given physical time.  If the Timestamp is past the end of all playing
-- blocks, return [].  The updater thread polls this at a given resolution for
-- all displayed blocks and updates the play selection accordingly.
--
-- Since a given block may be playing in multiple places at the same time (e.g.
-- for a block that is played like an instrument, if the notes overlap), the
-- same BlockId may occur more than once in the output list.
type InverseTempoFunction = Timestamp.Timestamp
    -> [(Block.BlockId, [(Track.TrackId, TrackPos)])]


-- * state

-- | Access to info that's needed by a particular run of the player.
-- This is read-only, and shouldn't need to be modified.
data State = State {
    -- | Communicate out of the Player.
    state_responder_chan :: Chan
    -- | Communicate into the Player.
    , state_transport :: Transport
    , state_midi_writer :: Midi.WriteMessage -> IO ()
    , state_block_id :: Block.BlockId

    -- | When play started.  Timestamps relative to the block start should be
    -- added to this to get absolute Timestamps.
    , state_timestamp_offset :: Timestamp.Timestamp
    , state_get_current_timestamp :: IO Timestamp.Timestamp
    }
state (Info chan writer get_ts) trans block_id = do
    ts <- get_ts
    return (State chan trans writer block_id ts get_ts)

write_status :: Chan -> PlayerStatus -> Block.BlockId -> IO ()
write_status chan status block_id =
    STM.atomically $ STM.writeTChan chan (Status block_id status)
