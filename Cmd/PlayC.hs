-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The FFI-using part of "Cmd.Play".  It uses the FFI because the play
    monitor wants to directly call to the UI to update the playback indicator,
    which is much more efficient than going through the whole "Ui.Diff" thing.

    But that means "Cmd.Play" would import FFI-using modules, which causes
    a problem for ghci, and, since I want the Cmd.Play functions to be
    available there, for the REPL as well.  So I engage in a hack: the play cmd
    returns a special 'Cmd.PlayMidi' value which contains the arguments for the
    play monitor thread.  The responder treats it as a Done but will call
    'play' with the given args.
-}
module Cmd.PlayC (cmd_play_msg, play) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Thread as Thread

import qualified Ui.Color as Color
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf

import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Types


-- | Respond to msgs about derivation and playing status.
cmd_play_msg :: Ui.Channel -> Msg.Msg -> Cmd.CmdIO
cmd_play_msg ui_chan msg = do
    case msg of
        Msg.Transport status -> transport_msg status
        Msg.DeriveStatus block_id status -> derive_status_msg block_id status
        _ -> Cmd.abort
    return Cmd.Done
    where
    transport_msg status = case status of
        Transport.Playing -> set_all_play_boxes Config.play_color
        -- Either the performer has declared itself stopped, or the play
        -- monitor has declared it stopped.  In any case, I don't need
        -- a transport to tell it what to do anymore.
        Transport.Stopped -> do
            Cmd.modify_play_state $ \st ->
                st { Cmd.state_play_control = Nothing }
            -- This will cover up derive status info, but that should be ok.
            -- And play normally only comes after derive is completed.
            set_all_play_boxes Config.box_color
        Transport.Died err_msg -> Log.warn ("player died: " ++ err_msg)
    derive_status_msg block_id status = do
        whenJust (derive_status_color status) (State.set_play_box block_id)
        case status of
            Msg.DeriveComplete perf -> do
                Cmd.modify_play_state $ \st -> st
                    { Cmd.state_performance = Map.insert block_id
                         perf (Cmd.state_performance st)
                    }
                    -- Don't mess with state_current_performance because Play
                    -- may have flipped the 'Cmd.perf_logs_written' bit.
                ui_state <- State.get
                liftIO $ Sync.set_track_signals ui_chan block_id ui_state
                    (Cmd.perf_track_signals perf)
                set_event_highlights (Cmd.perf_events perf)
            _ -> return ()
    derive_status_color status = case status of
        Msg.OutOfDate {} -> Just $ Color.brightness 1.5 Config.busy_color
        Msg.Deriving {} -> Just Config.busy_color
        Msg.DeriveComplete {} -> Just Config.box_color

set_all_play_boxes :: State.M m => Color.Color -> m ()
set_all_play_boxes color =
    mapM_ (flip State.set_play_box color) =<< State.all_block_ids

set_event_highlights :: Cmd.M m => Cmd.Events -> m ()
set_event_highlights events = return ()

-- * play

-- | This actually kicks off a MIDI play thread, and if an inverse tempo
-- function is given, a play monitor thread.
play :: Ui.Channel -> State.State -> Transport.Info -> Cmd.PlayMidiArgs
    -> IO Transport.PlayControl
play ui_chan ui_state transport_info
        (Cmd.PlayMidiArgs mmc name msgs maybe_inv_tempo repeat_at) = do
    (play_ctl, monitor_ctl) <-
        Midi.Play.play transport_info mmc name msgs repeat_at
    -- Pass the current state in the MVar.  ResponderSync will keep it up
    -- to date afterwards, but only if blocks are added or removed.
    MVar.modifyMVar_ (Transport.info_state transport_info) $
        const (return ui_state)
    liftIO $ void $ Thread.start $ case maybe_inv_tempo of
        Just inv_tempo -> play_monitor_thread ui_chan transport_info
            monitor_ctl inv_tempo repeat_at
        Nothing -> passive_play_monitor_thread
            (Transport.info_send_status transport_info) monitor_ctl
    return play_ctl

-- | Just coordinate Playing and Stopped, don't update a play position.
passive_play_monitor_thread :: (Transport.Status -> IO ())
    -> Transport.PlayMonitorControl -> IO ()
passive_play_monitor_thread send monitor_ctl =
    Exception.bracket_ (send Transport.Playing) (send Transport.Stopped) $
        Transport.wait_player_stopped monitor_ctl

-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
play_monitor_thread :: Ui.Channel -> Transport.Info
    -> Transport.PlayMonitorControl -> Transport.InverseTempoFunction
    -> Maybe RealTime -> IO ()
play_monitor_thread ui_chan transport_info ctl inv_tempo_func repeat_at = do
    let get_now = Transport.info_get_current_time transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    offset <- get_now
    let state = UpdaterState
            { monitor_ctl = ctl
            , monitor_offset = offset
            , monitor_get_now = get_now
            , monitor_inv_tempo_func = inv_tempo_func
            , monitor_active_sels = Set.empty
            , monitor_ui_state = Transport.info_state transport_info
            , monitor_repeat_at = repeat_at
            , monitor_ui_channel = ui_chan
            }
    ui_state <- MVar.readMVar (monitor_ui_state state)
    mapM_ (Sync.clear_play_position ui_chan) $
        Map.keys $ State.state_views ui_state
    Exception.bracket_
        (Transport.info_send_status transport_info Transport.Playing)
        (Transport.info_send_status transport_info Transport.Stopped)
        (monitor_loop state)

data UpdaterState = UpdaterState {
    monitor_ctl :: Transport.PlayMonitorControl
    -- | When the monitor thread started.
    , monitor_offset :: RealTime
    , monitor_get_now :: IO RealTime
    , monitor_inv_tempo_func :: Transport.InverseTempoFunction
    , monitor_active_sels :: Set.Set (ViewId, [TrackNum])
    , monitor_ui_state :: MVar.MVar State.State
    , monitor_repeat_at :: Maybe RealTime
    , monitor_ui_channel :: Ui.Channel
    }

monitor_loop :: UpdaterState -> IO ()
monitor_loop state = do
    now <- maybe id (flip Num.fmod) (monitor_repeat_at state)
        . subtract (monitor_offset state) <$> monitor_get_now state
    let fail err = Log.error ("state error in play monitor: " ++ show err)
            >> return []
    ui_state <- MVar.readMVar (monitor_ui_state state)
    let block_pos = monitor_inv_tempo_func state now
    play_pos <- either fail return $ State.eval ui_state $
        Perf.block_pos_to_play_pos block_pos
    Sync.set_play_position (monitor_ui_channel state) play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    mapM_ (Sync.clear_play_position (monitor_ui_channel state) . fst) $
        Set.toList (Set.difference (monitor_active_sels state) active_sels)
    state <- return $ state { monitor_active_sels = active_sels }

    stopped <- Transport.poll_player_stopped (monitor_ctl state)
    -- If repeat_at is on, then playback will never stop on its own, even if
    -- I run out of tempo map (which happens if the repeat point is at or past
    -- the end of the score).  When the monitor thread stops, it sends Stopped
    -- on the transport, which clears out the player ctl which will make the
    -- player unstoppable, if it's still going.
    if stopped || (null block_pos && Maybe.isNothing (monitor_repeat_at state))
        then do
            mapM_ (Sync.clear_play_position (monitor_ui_channel state) . fst) $
                Set.toList (monitor_active_sels state)
            unless stopped $ wait_for_stop $
                Transport.poll_player_stopped (monitor_ctl state)
        else Thread.delay 0.05 >> monitor_loop state
    where
    wait_for_stop ctl = do
        Thread.delay 0.1
        stopped <- ctl
        unless stopped $ wait_for_stop ctl
