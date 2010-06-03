{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
module Perform.Midi.Play where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Ui
import qualified Midi.Midi as Midi

import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Instrument as Instrument


-- | Start a thread to stream a list of WriteMessages, and return
-- a Transport.Control which can be used to stop and restart the player.
play :: Transport.Info -> BlockId -> [Midi.WriteMessage]
    -> IO (Transport.PlayControl, Transport.UpdaterControl)
play transport_info block_id midi_msgs = do
    state <- Transport.state transport_info block_id
    let ts_offset = Transport.state_timestamp_offset state
        -- Catch msgs up to realtime.
        ts_midi_msgs = map (Midi.add_timestamp ts_offset) midi_msgs
    Thread.start_thread "render midi" (player_thread state ts_midi_msgs)
    return (Transport.state_play_control state,
        Transport.state_updater_control state)

player_thread :: Transport.State -> [Midi.WriteMessage] -> IO ()
player_thread state midi_msgs = do
    let name = show (Transport.state_block_id state)
    Log.notice $ "play block " ++ name ++ " starting at "
        ++ show (Transport.state_timestamp_offset state)
    play_msgs state Set.empty midi_msgs
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Transport.state_send_status state
                (Transport.state_block_id state) (Transport.Died (show exc))
    Transport.player_stopped (Transport.state_updater_control state)
    Log.notice $ "render score " ++ show name ++ " complete"


-- * implementation

-- | 'play_msgs' tries to not get too far ahead of now both to avoid flooding
-- the midi driver and so a stop will happen fairly quickly.
write_ahead :: Timestamp.Timestamp
write_ahead = Timestamp.seconds 1

-- | @devs@ keeps track of devices that have been seen, so I know which devices
-- to reset.
play_msgs :: Transport.State -> Set.Set Instrument.Addr -> [Midi.WriteMessage]
    -> IO ()
play_msgs state addrs_seen msgs = do
    let write_midi = Transport.state_midi_writer state
    -- Make sure that I get a consistent play, not affected by previous
    -- control states.
    -- send_all write_midi new_devs Midi.ResetAllCcontrols

    -- This should make the buffer always be between write_ahead*2 and
    -- write_ahead ahead of now.
    now <- Transport.state_get_current_timestamp state
    let (chunk, rest) = span ((< (now + (write_ahead*2))) . Midi.wmsg_ts) msgs
    -- Log.debug $ "play at " ++ show now ++ " chunk: " ++ show (length chunk)
    mapM_ write_midi chunk
    addrs_seen <- return (update_addrs addrs_seen chunk)

    let timeout = if null rest then write_ahead * 2 else write_ahead
    stop <- Transport.check_for_stop (Timestamp.to_seconds timeout)
        (Transport.state_play_control state)
    case (stop, rest) of
        (True, _) -> do
            Transport.state_midi_abort state
            -- Ok, so there's this weird bug (?) in CoreMIDI, where an abort
            -- will convert deschedued pitchbends to -1 pitchbends.  So abort,
            -- wait for it to send its bogus pitchbend, and then reset it.
            -- send_all write_midi addrs_seen now Midi.AllNotesOff
            Concurrent.threadDelay 150000
            send_all write_midi addrs_seen (now + Timestamp.Timestamp 150)
                (Midi.PitchBend 0)
        (_, []) -> send_all write_midi addrs_seen now (Midi.PitchBend 0)
        _ -> play_msgs state addrs_seen rest

send_all write_midi addrs ts chan_msg =
    forM_ (Set.elems addrs) $ \(dev, chan) -> write_midi
        (Midi.WriteMessage dev ts (Midi.ChannelMessage chan chan_msg))

-- Force 'addrs_seen' so I don't drag on 'wmsgs'.
update_addrs addrs_seen wmsgs = Set.size addrs_seen' `seq` addrs_seen'
    where
    addrs_seen' = Set.union addrs_seen
        (Set.fromList (Seq.map_maybe wmsg_addr wmsgs))

wmsg_addr :: Midi.WriteMessage -> Maybe Instrument.Addr
wmsg_addr (Midi.WriteMessage dev _ (Midi.ChannelMessage chan _)) =
    Just (dev, chan)
wmsg_addr _ = Nothing
