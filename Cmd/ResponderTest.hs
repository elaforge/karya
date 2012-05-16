module Cmd.ResponderTest where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.StubMidi as StubMidi

import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.Responder as Responder

import qualified Derive.DeriveTest as DeriveTest
import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig
import Types


-- * setup

type States = (State.State, Cmd.State)

mkstates :: [UiTest.TrackSpec] -> States
mkstates tracks = (ui_state, mk_cmd_state ui_state UiTest.default_view_id)
    where
    ui_state = UiTest.exec State.empty $ do
        UiTest.mkblock_view (UiTest.default_block_name, tracks)
        State.set_selection UiTest.default_view_id Config.insert_selnum
            (Just (Types.selection 1 0 1 0))

-- | Many cmds rely on a focused view, and it's easy to forget to add it, so
-- make it mandatory.
mk_cmd_state :: State.State -> ViewId -> Cmd.State
mk_cmd_state ui_state view_id = CmdTest.default_cmd_state
    { Cmd.state_focused_view = Just view_id
    -- Normally this is created by the setup cmd, so pretend I did one.
    , Cmd.state_history = Cmd.History [] (Just present) [] Nothing
    }
    where present = Cmd.HistoryEntry ui_state [] ["setup"] Nothing

-- | It would be nicer to have this happen automatically.
set_midi_config :: State.StateId ()
set_midi_config = State.set_midi_config DeriveTest.default_midi_config


-- * result

data Result = Result {
    -- | In CmdTest, Result has MIDI from the thru mechanism, and updates
    -- collected during the event.  But responder tests scope over the entire
    -- respond cycle, so MIDI potentially also includes play output and
    -- updates include diff output.
    result_cmd :: CmdTest.Result Cmd.Status
    -- | These are the updates emitted to the UI.  The CmdTest.Result updates
    -- are those collected during the cmd.
    , result_updates :: [Update.DisplayUpdate]
    , result_loopback :: Chan.Chan Msg.Msg
    }

-- | Get the performance from a Result.
--
-- TODO error-prone because if you call this on a Result that didn't
-- regenerate the performance this will hang forever
--
-- TODO there should also be a way to pass 0 to Performance.update_performance
-- so tests don't waste time sleeping.
result_perf :: Result -> IO (BlockId, Cmd.Performance)
result_perf = get_perf . result_loopback

get_perf :: Chan.Chan Msg.Msg -> IO (BlockId, Cmd.Performance)
get_perf chan = do
    msg <- Chan.readChan chan
    case msg of
        Msg.DeriveStatus block_id (Msg.DeriveComplete perf) ->
            return (block_id, perf)
        _ -> get_perf chan

result_states :: Result -> States
result_states result = (CmdTest.result_ui_state r, CmdTest.result_cmd_state r)
    where r = result_cmd result

-- * thread

-- TODO to do a realistic simulation, I think I need to asynchronously send
-- loopback results back into the input queue.  This will let me test that
-- cmds complete with a minimum of delay, don't get in loops, and the new
-- performance becomes available eventually.

thread :: States -> [Msg.Msg] -> IO [Result]
thread states msgs = thread_delay False states [(m, 0) | m <- msgs]

thread_delay :: Bool -> States -> [(Msg.Msg, Thread.Seconds)] -> IO [Result]
thread_delay _ _ [] = return []
thread_delay print_timing states ((msg, delay):msgs) = do
    (result, secs) <- timer $ respond1 states msg
    when print_timing $
        Printf.printf "%s -> lag: %.2fs\n" (Pretty.pretty msg) secs
    Thread.delay delay
    (result:) <$> thread_delay print_timing (result_states result) msgs

-- * respond_cmd

-- | Respond to a single Cmd.  This can be used to test cmds in the full
-- responder context without having to fiddle around with keymaps.
respond_cmd :: States -> Cmd.CmdT IO a -> IO Result
respond_cmd states cmd = _respond states (Just (mkcmd cmd)) magic
    where
    -- I run a cmd by adding a cmd that responds only to a specific Msg, and
    -- then sending that Msg.
    mkcmd cmd msg
        | is_magic msg = cmd >> return Cmd.Done
        | otherwise = return Cmd.Continue
    is_magic (Msg.Socket _ "MAGIC!!") = True
    is_magic _ = False
    magic = Msg.Socket IO.stdout "MAGIC!!"

respond1 :: States -> Msg.Msg -> IO Result
respond1 states = _respond states Nothing

type CmdIO = Msg.Msg -> Cmd.CmdIO

_respond :: States -> Maybe CmdIO -> Msg.Msg -> IO Result
_respond (ustate, cstate) cmd msg = do
    update_chan <- new_chan
    loopback_chan <- Chan.newChan
    (interface, midi_chan) <- make_midi_interface
    let rstate = make_rstate update_chan loopback_chan
            ustate (cstate { Cmd.state_midi_interface = interface }) cmd
    (_quit, rstate) <- Responder.respond rstate msg
    -- Updates and MIDI are normally forced by syncing with the UI and MIDI
    -- driver, so force explicitly here.  Not sure if this really makes
    -- a difference.
    midi <- get_vals midi_chan
    force midi
    updates <- concat <$> get_vals update_chan
    force updates
    let res = CmdTest.Result (Right Nothing) (Responder.state_cmd rstate)
            (Responder.state_ui rstate) [] []
            [(Midi.wmsg_dev m, Midi.wmsg_msg m) | m <- midi]
    return $ Result res updates loopback_chan

make_rstate :: TVar.TVar [[Update.DisplayUpdate]]
    -> Chan.Chan Msg.Msg -> State.State -> Cmd.State -> Maybe CmdIO
    -> Responder.State
make_rstate update_chan loopback_chan ui_state cmd_state cmd =
    Responder.State config ui_state cmd_state lang_session loopback dummy_sync
        updater_state
    where
    updater_state = Unsafe.unsafePerformIO (MVar.newMVar State.empty)
    config = StaticConfig.empty
        { StaticConfig.global_cmds = maybe [] (:[]) cmd }
    dummy_sync _ _ _ updates = do
        put_val update_chan updates
        return Nothing
    loopback = Chan.writeChan loopback_chan
    -- Tests should link with the dummy interpreter.
    lang_session = ()

make_midi_interface :: IO (Interface.Interface, TVar.TVar [Midi.WriteMessage])
make_midi_interface = do
    midi_chan <- new_chan
    int <- StubMidi.interface
    let write msg = put_val midi_chan msg >> return True
    return (int { Interface.write_message = write }, midi_chan)

-- Use a TVar as a kind of non-blocking channel.
new_chan :: IO (TVar.TVar [a])
new_chan = TVar.newTVarIO []

put_val :: TVar.TVar [a] -> a -> IO ()
put_val chan v = STM.atomically $ do
    old <- TVar.readTVar chan
    TVar.writeTVar chan (v:old)

get_vals :: TVar.TVar [a] -> IO [a]
get_vals chan = fmap reverse (STM.readTVarIO chan)
