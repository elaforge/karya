{-# LANGUAGE ScopedTypeVariables #-}
module Perform.Midi.Play (play) where
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Derive.LEvent as LEvent
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport
import Types


type Messages = [LEvent.LEvent Midi.WriteMessage]

-- | Start a thread to stream a list of WriteMessages, and return
-- a Transport.Control which can be used to stop and restart the player.
play :: Transport.Info -> BlockId -> Messages
    -> IO (Transport.PlayControl, Transport.UpdaterControl)
play transport_info block_id midi_msgs = do
    state <- make_state transport_info block_id
    let ts_offset = state_time_offset state
        -- Catch msgs up to realtime.
        ts_midi_msgs = map (fmap (Midi.add_timestamp ts_offset)) midi_msgs
    Thread.start_logged "render midi" (player_thread state ts_midi_msgs)
    return (state_play_control state, state_updater_control state)

player_thread :: State -> Messages -> IO ()
player_thread state msgs = do
    Log.debug $ "play block " ++ show (state_block_id state)
    play_msgs state msgs
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Transport.info_send_status (state_info state)
                (Transport.Died (show exc))
    Transport.player_stopped (state_updater_control state)
    Log.debug $ "render score " ++ show (state_block_id state) ++ " complete"

-- * implementation

-- | Access to info that's needed by a particular run of the player.
-- This is read-only, and shouldn't need to be modified.
data State = State {
    -- | Communicate into the Player.
    state_play_control :: Transport.PlayControl
    , state_updater_control :: Transport.UpdaterControl
    , state_block_id :: BlockId

    -- | When play started.  Timestamps relative to the block start should be
    -- added to this to get absolute Timestamps.
    , state_time_offset :: RealTime
    , state_info :: Transport.Info
    }

make_state :: Transport.Info -> BlockId -> IO State
make_state info block_id = do
    ts <- Transport.info_get_current_time info
    play_control <- fmap Transport.PlayControl STM.newEmptyTMVarIO
    updater_control <- fmap Transport.UpdaterControl (IORef.newIORef False)
    return $ State play_control updater_control block_id ts info

-- | 'play_msgs' tries to not get too far ahead of now both to avoid flooding
-- the midi driver and so a stop will happen fairly quickly.
write_ahead :: RealTime
write_ahead = RealTime.seconds 1

-- | @devs@ keeps track of devices that have been seen, so I know which devices
-- to reset.
play_msgs :: State -> Messages -> IO ()
play_msgs state msgs = do
    let write_midi = Transport.info_midi_writer (state_info state)
        write_msg = LEvent.either (write_midi . Interface.Midi) Log.write

    -- This should make the buffer always be between write_ahead*2 and
    -- write_ahead ahead of now.
    now <- Transport.info_get_current_time (state_info state)
    let until = now + RealTime.mul write_ahead 2
    let (chunk, rest) =
            span (LEvent.either ((<until) . Midi.wmsg_ts) (const True))  msgs
    -- Log.debug $ "play at " ++ show now ++ " chunk: " ++ show (length chunk)
    mapM_ write_msg chunk

    let timeout = if null rest then RealTime.mul write_ahead 2 else write_ahead
    stop <- Transport.check_for_stop (RealTime.to_seconds timeout)
        (state_play_control state)
    case (stop, rest) of
        (True, _) -> do
            Transport.info_midi_abort (state_info state)
            mapM_ write_midi
                [Interface.AllNotesOff now, Interface.reset_controls now]
        (_, []) -> write_midi $ Interface.reset_pitch now
        _ -> play_msgs state rest
