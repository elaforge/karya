{- | The FFI-using part of "Cmd.Play".  It uses the FFI because the updater
    wants to directly call to the UI to update the playback indicator, which is
    much more efficient than going through the whole "Ui.Diff" thing.

    But that means "Cmd.Play" would import FFI-using modules, which causes
    a problem for ghci, and, since I want the Cmd.Play functions to be
    available there, for the REPL as well.  So I engage in a hack: the play
    cmd returns a special 'Cmd.Play' value which contains the arguments for
    the updater thread.  The responder treats it as a Done but will call
    'start_updater' with the given args.
-}
module Cmd.PlayC (cmd_play_msg, start_updater) where
import Control.Monad
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
        -- Either the performer has declared itself stopped, or the updater
        -- has declared it stopped.  In any case, I don't need a transport
        -- to tell it what to do anymore.
        Transport.Stopped -> Cmd.modify_play_state $ \st ->
            st { Cmd.state_play_control = Nothing }
        Transport.Died err_msg -> Log.warn ("player died: " ++ err_msg)
    derive_status_msg block_id status = do
        State.set_play_box block_id (derive_status_color status)
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
                Trans.liftIO $ Sync.set_track_signals ui_state
                    (Cmd.perf_track_signals perf)
            _ -> return ()
    derive_status_color status = case status of
        Msg.OutOfDate {} -> Color.brightness 1.5 Config.busy_color
        Msg.Deriving {} -> Config.busy_color
        Msg.DeriveComplete {} -> Config.box_color


-- * updater

start_updater :: Transport.Info -> Cmd.UpdaterArgs -> IO ()
start_updater transport_info (Cmd.UpdaterArgs ctl inv_tempo_func start) =
    void $ Thread.start $
        updater_thread ctl transport_info inv_tempo_func start

-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
updater_thread :: Transport.UpdaterControl -> Transport.Info
    -> Transport.InverseTempoFunction -> RealTime -> IO ()
updater_thread ctl transport_info inv_tempo_func start = do
    let get_now = Transport.info_get_current_time transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    offset <- get_now
    let state = UpdaterState ctl (offset - start) get_now
            inv_tempo_func Set.empty (Transport.info_state transport_info)
    let send status = Transport.info_send_status transport_info status
    Exception.bracket_ (send Transport.Playing) (send Transport.Stopped)
        (updater_loop state)

data UpdaterState = UpdaterState {
    updater_ctl :: Transport.UpdaterControl
    , updater_offset :: RealTime
    , updater_get_now :: IO RealTime
    , updater_inv_tempo_func :: Transport.InverseTempoFunction
    , updater_active_sels :: Set.Set (ViewId, [TrackNum])
    , updater_ui_state :: MVar.MVar State.State
    }

updater_loop :: UpdaterState -> IO ()
updater_loop state = do
    now <- subtract (updater_offset state) <$> updater_get_now state
    let fail err = Log.error ("state error in updater: " ++ show err)
            >> return []
    ui_state <- MVar.readMVar (updater_ui_state state)
    play_pos <- either fail return $ State.eval ui_state $
        Perf.find_play_pos (updater_inv_tempo_func state) now
    Sync.set_play_position play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    mapM_ (Sync.clear_play_position . fst) $
        Set.toList (Set.difference (updater_active_sels state) active_sels)
    state <- return $ state { updater_active_sels = active_sels }

    stopped <- Transport.check_player_stopped (updater_ctl state)
    if stopped || null (updater_inv_tempo_func state now)
        then mapM_ (Sync.clear_play_position . fst) $
            Set.toList (updater_active_sels state)
        else Thread.delay 0.05 >> updater_loop state
