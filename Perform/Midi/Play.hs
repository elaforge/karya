module Perform.Midi.Play where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.IORef as IORef
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.Block as Block

import qualified Midi.Midi as Midi

import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp


-- | If this is True, send ResetAllControllers before playing to a device.
reset_controllers = False

-- | Start a thread to stream a list of WriteMessages, and return
-- a Transport.Control which can be used to stop and restart the player.
play :: Transport.Info -> Block.BlockId -> Timestamp.Timestamp
    -> [Midi.WriteMessage] -> IO Transport.Transport
play transport_info block_id start_ts midi_msgs = do
    transport <- fmap Transport.Transport (IORef.newIORef Transport.Play)
    state <- Transport.state transport_info transport block_id
    let ts_offset = Transport.state_timestamp_offset state
        ts_midi_msgs = map (add_ts (ts_offset - start_ts)) midi_msgs
    Thread.start_thread "render midi" $
        player_thread state ts_midi_msgs `Exception.catch` \exc -> do
            Transport.write_status (Transport.state_responder_chan state)
                (Transport.Died exc) (Transport.state_block_id state)
            -- The Renderer died so it doesn't need the Stop, but I still need
            -- to stop the updater.
            Transport.send_transport transport Transport.Stop
    return transport

-- Catch this event up to realtime.
add_ts ts wmsg = wmsg { Midi.wmsg_ts = Midi.wmsg_ts wmsg + ts }


player_thread :: Transport.State -> [Midi.WriteMessage] -> IO ()
player_thread state midi_msgs = do
    let name = show (Transport.state_block_id state)
    Log.notice $ "play block " ++ name ++ " starting at "
        ++ show (Transport.state_timestamp_offset state)
    play_msgs state Set.empty midi_msgs
    Log.notice $ "render score " ++ show name ++ " complete"


-- * implementation

-- | 'play_msgs' tries to not get too far ahead of now both to avoid flooding
-- the midi driver and so a stop will happen fairly quickly.  I could flush the
-- devices, but PortMidi insists that you close and reopen the device
-- afterwards.
write_ahead :: Timestamp.Timestamp
write_ahead = Timestamp.seconds 0.5

-- | @devs@ is used to keep track of devices that have received their first
-- message, so some state-resetting msgs can be sent to make sure the synth is
-- in a known state.  This has somewhat questionable value, but I'll keep it as
-- it is for the moment.
play_msgs :: Transport.State -> Set.Set Midi.WriteDevice -> [Midi.WriteMessage]
    -> IO ()
play_msgs state devs msgs = do
    let new_devs = Set.difference (Set.fromList (map Midi.wmsg_dev msgs)) devs
        write = Transport.state_midi_writer state
    -- Force 'devs' so I don't drag 'msgs' on forever.
    -- TODO verify that this is working right
    devs <- let devs' = Set.union devs new_devs
        in Set.size devs' `seq` return devs'
    -- Make sure that I get a consistent play, not affected by previous
    -- controller states.
    when reset_controllers $
        send_all write new_devs Midi.ResetAllControllers

    now <- Transport.state_get_current_timestamp state
    let (chunk, rest) = span ((< (now + write_ahead)) . Midi.wmsg_ts) msgs
    -- Log.debug $ "play at " ++ show now ++ " chunk: " ++ show (length chunk)
    mapM_ write chunk
    tmsg <- Transport.check_transport (Transport.state_transport state)
    case (rest, tmsg) of
        -- I could send AllNotesOff here, but stuck notes probably indicate an
        -- error so it's good to hear them.
        -- I leave other controllers be, but pitch bend is too annoying to
        -- leave.  If the last note is bent it will make a sproing.  Another
        -- solution would be to get rid of Pitch.scale_set_pitch_bend so that
        -- the bend will be fixed the next time you play a note.
        ([], _) -> send_all write devs (Midi.PitchBend 0)
        (_, Transport.Stop) -> send_all write devs Midi.AllNotesOff
            >> send_all write devs (Midi.PitchBend 0)
        _ -> do
            -- block to avoid flooding the midi driver
            Concurrent.threadDelay (fromIntegral
                (Timestamp.to_microseconds write_ahead))
            play_msgs state devs rest

send_all write_midi devs chan_msg = forM_ (Set.elems devs) $ \dev ->
    forM_ [0..15] $ \chan -> write_midi
        (Midi.WriteMessage dev Timestamp.immediately
            (Midi.ChannelMessage chan chan_msg))
