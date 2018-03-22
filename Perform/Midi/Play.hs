-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This module is responsible for actually scheduling MIDI messages with the
-- OS's MIDI driver.
module Perform.Midi.Play (play, cycle_messages) where
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.Mmc as Mmc

import qualified Cmd.Cmd as Cmd
import qualified Derive.LEvent as LEvent
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport
import Global
import Types


type Messages = [LEvent.LEvent Midi.WriteMessage]

-- | Start a thread to stream a list of WriteMessages, and return
-- a Transport.Control which can be used to stop and restart the player.
play :: Transport.Info -> Maybe Cmd.SyncConfig -> Text -> Messages
    -> Maybe RealTime
    -- ^ If given, loop back to the beginning when this time is reached.
    -> IO (Transport.PlayControl, Transport.PlayMonitorControl)
play transport_info sync name msgs repeat_at = do
    state <- make_state transport_info
    now <- Transport.info_get_current_time transport_info
    Thread.startLogged "render midi" $
        -- Don't send MMC if I'm repeating, it'll just confuse the DAW.
        player_thread (if Maybe.isJust repeat_at then Nothing else sync) now
            name state (process now repeat_at msgs)
    return (state_play_control state, state_monitor_control state)
    where
    -- Catch msgs up to realtime and cycle them if I'm repeating.
    process now repeat_at = shift_messages now . cycle_messages repeat_at

player_thread :: Maybe Cmd.SyncConfig -> RealTime -> Text -> State
    -> Messages -> IO ()
player_thread maybe_sync start name state msgs = do
    Log.debug $ "play start: " <> name
    case maybe_sync of
        Just sync | not (Cmd.sync_mtc sync) ->
            state_write_midi state $ make_mmc sync start Mmc.Play
        _ -> return ()
    play_loop state msgs
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Transport.info_send_status (state_info state)
                (Transport.Died (show exc))
    case maybe_sync of
        Just sync | not (Cmd.sync_mtc sync) ->
            state_write_midi state $ make_mmc sync start Mmc.Stop
        _ -> return ()
    Transport.player_stopped (state_monitor_control state)
    Log.debug $ "play complete: " <> name

make_mmc :: Cmd.SyncConfig -> RealTime -> Mmc.Mmc -> Midi.WriteMessage
make_mmc sync start msg = Midi.WriteMessage (Cmd.sync_device sync) start $
    Mmc.encode (Cmd.sync_device_id sync) msg

cycle_messages :: Maybe RealTime -> Messages -> Messages
cycle_messages _ [] = []
cycle_messages Nothing msgs = msgs
cycle_messages (Just repeat_at) msgs =
    go1 $ takeWhile (LEvent.log_or ((<repeat_at) . Midi.wmsg_ts)) msgs
    where
    go1 msgs = msgs ++ go (shift_messages repeat_at (strip msgs))
    -- Don't bother writing logs the second time around.
    go [] = []
    go msgs = msgs ++ go (shift_messages repeat_at msgs)
    strip = map LEvent.Event . LEvent.events_of

shift_messages :: RealTime -> Messages -> Messages
shift_messages ts = map (fmap (Midi.add_timestamp ts))

-- * loop

-- | Access to info that's needed by a particular run of the player.
-- This is read-only, and shouldn't need to be modified.
data State = State {
    -- | Communicate into the Player.
    state_play_control :: !Transport.PlayControl
    , state_monitor_control :: !Transport.PlayMonitorControl
    , state_info :: !Transport.Info
    }

make_state :: Transport.Info -> IO State
make_state info = do
    play_control <- Transport.play_control
    monitor_control <- Transport.play_monitor_control
    return $ State
        { state_play_control = play_control
        , state_monitor_control = monitor_control
        , state_info = info
        }

-- | 'play_loop' tries to not get too far ahead of now both to avoid flooding
-- the midi driver and so a stop will happen fairly quickly.
write_ahead :: RealTime
write_ahead = RealTime.seconds 1

state_write :: State -> Interface.Message -> IO ()
state_write state = Transport.info_midi_writer (state_info state)

state_write_midi :: State -> Midi.WriteMessage -> IO ()
state_write_midi state = state_write state . Interface.Midi

-- | @devs@ keeps track of devices that have been seen, so I know which devices
-- to reset.
play_loop :: State -> Messages -> IO ()
play_loop state msgs = do
    -- This should make the buffer always be between write_ahead*2 and
    -- write_ahead ahead of now.
    now <- Transport.info_get_current_time (state_info state)
    let until = now + RealTime.mul write_ahead 2
    let (chunk, rest) =
            span (LEvent.either ((<until) . Midi.wmsg_ts) (const True)) msgs
    -- Log.debug $ "play at " ++ show now ++ " chunk: " ++ show (length chunk)
    mapM_ (LEvent.either (state_write_midi state) Log.write) chunk

    -- Don't quit until all events have been played.
    let timeout = if null rest
            then maybe now Midi.wmsg_ts (Seq.last (LEvent.events_of chunk))
                - now
            else write_ahead
    stop <- Transport.poll_stop_player (RealTime.to_diff timeout)
        (state_play_control state)
    let reset_midi = state_write state (Interface.AllNotesOff now)
    case (stop, rest) of
        (True, _) -> do
            Transport.info_midi_abort (state_info state)
            reset_midi
        (_, []) -> reset_midi
        _ -> play_loop state rest
