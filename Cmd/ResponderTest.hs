module Cmd.ResponderTest where
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.DeepSeq as DeepSeq

import qualified Midi.Midi as Midi

import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Responder as Responder

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Transport as Transport

import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig


mkstates tracks = (ui_state, cmd_state)
    where
    (_, ui_state) = UiTest.run State.empty $ do
        UiTest.mkstate_view "b1" tracks
        State.set_selection UiTest.default_view_id Config.insert_selnum
            (Types.selection 1 0 1 0)
    cmd_state = Cmd.empty_state
        { Cmd.state_focused_view = Just UiTest.default_view_id }

type States = (State.State, Cmd.State)

respond :: States -> [Msg.Msg]
    -> IO ([[Update.Update]], [[Midi.WriteMessage]], States)
respond states [] = return ([], [], states)
respond states (msg:msgs) = do
    (updates, midi, states) <- respond_msg states msg
    (rest_updates, rest_midi, final_states) <- respond states msgs
    return (force updates : rest_updates, force midi : rest_midi, final_states)

force :: (DeepSeq.NFData a) => a -> a
force x = DeepSeq.deepseq x x

-- The updates are normally forced by syncing to the UI, but since that doesn't
-- happen here, they should be forced by the caller.
respond_msg :: States -> Msg.Msg
    -> IO ([Update.Update], [Midi.WriteMessage], States)
respond_msg states msg = do
    update_chan <- new_chan
    midi_chan <- new_chan
    let rstate = make_rstate update_chan midi_chan states
    (_, rstate) <- Responder.respond rstate msg
    -- Thread.delay 0.5
    midi <- get_vals midi_chan
    updates <- get_vals update_chan
    return (concat updates, midi,
        (Responder.state_ui rstate, Responder.state_cmd rstate))

make_rstate update_chan midi_chan (ui_state, cmd_state) =
    Responder.State StaticConfig.empty
        ui_state cmd_state write_midi
        (Transport.Info send_status write_midi abort_midi get_now_ts)
        lang_session loopback dummy_sync
    where
    dummy_sync _ updates = do
        put_val update_chan updates
        return Nothing
    -- I suppose I should loop them back and respond again?
    loopback _ = return ()
    -- Tests should link with the dummy interpreter.
    lang_session = ()
    abort_midi = return ()
    write_midi msg = put_val midi_chan msg
    get_now_ts = return Timestamp.zero
    send_status block_id status =
        loopback (Msg.Transport (Transport.Status block_id status))

new_chan = TVar.newTVarIO []

put_val chan v = STM.atomically $ do
    old <- TVar.readTVar chan
    TVar.writeTVar chan (v:old)

get_vals chan = fmap reverse (STM.readTVarIO chan)
