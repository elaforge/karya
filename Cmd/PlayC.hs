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
import qualified Control.Monad.Trans as Trans

import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.Color as Color
import qualified Ui.State as State
import qualified Ui.Sync as Sync

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf

import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Types


-- | Respond to msgs about derivation and playing status.
cmd_play_msg :: Msg.Msg -> Cmd.CmdIO
cmd_play_msg msg = do
    case msg of
        Msg.Transport status -> transport_msg status
        Msg.DeriveStatus block_id status -> derive_status_msg block_id status
        _ -> Cmd.abort
    return Cmd.Done
    where
    transport_msg status = case status of
        Transport.Playing -> return ()
        -- Either the performer has declared itself stopped, or the play
        -- monitor has declared it stopped.  In any case, I don't need
        -- a transport to tell it what to do anymore.
        Transport.Stopped -> Cmd.modify_play_state $ \st ->
            st { Cmd.state_play_control = Nothing }
        Transport.Died err_msg -> Log.warn ("player died: " ++ err_msg)
    derive_status_msg block_id status = do
        when_just (derive_status_color status) (State.set_play_box block_id)
        case status of
            Msg.OutOfDate perf ->
                Cmd.modify_play_state $ \st ->
                    st { Cmd.state_current_performance = Map.insert block_id
                        perf (Cmd.state_current_performance st) }
            Msg.DeriveComplete perf -> do
                Cmd.modify_play_state $ \st ->
                    st { Cmd.state_performance = Map.insert block_id
                         perf (Cmd.state_performance st) }
                ui_state <- State.get
                Trans.liftIO $ Sync.set_track_signals block_id ui_state
                    (Cmd.perf_track_signals perf)
            _ -> return ()
    derive_status_color status = case status of
        Msg.OutOfDate {} -> Just $ Color.brightness 1.5 Config.busy_color
        Msg.Deriving {} -> Just Config.busy_color
        Msg.DeriveComplete {} -> Just Config.box_color
        Msg.Killed {} -> Just Config.box_color

-- * play

-- | This actually kicks off a MIDI play thread, and if an inverse tempo
-- function is given, a play monitor thread.
play :: State.State -> Transport.Info -> Cmd.PlayMidiArgs
    -> IO Transport.PlayControl
play ui_state transport_info (Cmd.PlayMidiArgs name msgs maybe_inv_tempo) = do
    (play_ctl, monitor_ctl) <- Midi.Play.play transport_info name msgs
    -- Pass the current state in the MVar.  ResponderSync will keep it up
    -- to date afterwards, but only if blocks are added or removed.
    MVar.modifyMVar_ (Transport.info_state transport_info) $
        const (return ui_state)
    Trans.liftIO $ void $ Thread.start $ case maybe_inv_tempo of
        Just inv_tempo ->
            play_monitor_thread transport_info monitor_ctl inv_tempo
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
play_monitor_thread :: Transport.Info
    -> Transport.PlayMonitorControl -> Transport.InverseTempoFunction -> IO ()
play_monitor_thread transport_info ctl inv_tempo_func = do
    let get_now = Transport.info_get_current_time transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    offset <- get_now
    let state = UpdaterState ctl offset get_now
            inv_tempo_func Set.empty (Transport.info_state transport_info)
    Exception.bracket_
        (Transport.info_send_status transport_info Transport.Playing)
        (Transport.info_send_status transport_info Transport.Stopped)
        (monitor_loop state)

data UpdaterState = UpdaterState {
    monitor_ctl :: Transport.PlayMonitorControl
    , monitor_offset :: RealTime
    , monitor_get_now :: IO RealTime
    , monitor_inv_tempo_func :: Transport.InverseTempoFunction
    , monitor_active_sels :: Set.Set (ViewId, [TrackNum])
    , monitor_ui_state :: MVar.MVar State.State
    }

monitor_loop :: UpdaterState -> IO ()
monitor_loop state = do
    -- 'now' can start negative if the events themselves started negative, say
    -- due to an ornament that moves things back in time.  Cmd.Play.cmd_play
    -- will have moved the events up to 0 in this case.
    now <- max 0 . subtract (monitor_offset state) <$> monitor_get_now state
    let fail err = Log.error ("state error in play monitor: " ++ show err)
            >> return []
    ui_state <- MVar.readMVar (monitor_ui_state state)
    let block_pos = monitor_inv_tempo_func state now
    play_pos <- either fail return $ State.eval ui_state $
        Perf.block_pos_to_play_pos block_pos
    Sync.set_play_position play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    mapM_ (Sync.clear_play_position . fst) $
        Set.toList (Set.difference (monitor_active_sels state) active_sels)
    state <- return $ state { monitor_active_sels = active_sels }

    stopped <- Transport.poll_player_stopped (monitor_ctl state)
    if stopped || null block_pos
        then mapM_ (Sync.clear_play_position . fst) $
            Set.toList (monitor_active_sels state)
        else Thread.delay 0.05 >> monitor_loop state
