-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is similar in intent to "Cmd.CmdTest", but it simulates the entire
-- respond loop.  So it's more accurate and can test more, but is less
-- convenient in that you can't run Cmds directly (you have to use
-- 'respond_cmd').  In addition, you can't run the tests from ghci since it
-- winds up linking in the GUI libs, even though there's no GUI.
--
-- TODO shouldn't it be possible to lift this restriction?
module Cmd.ResponderTest (
    -- * States
    States
    , mkstates, mk_cmd_state
    -- * Result
    , Result(..), print_results, result_states, result_ui_state
    , result_cmd_state
    , result_perf
    -- * run
    , respond_cmd, next, respond_until, continue_until
    , is_derive_complete
    , thread, thread_delay
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

import qualified Data.Map as Map
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Testing as Testing
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.StubMidi as StubMidi
import qualified Ui.Sel as Sel
import qualified Ui.State as State
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.Repl as Repl
import qualified Cmd.Responder as Responder

import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import qualified App.StaticConfig as StaticConfig

import Global
import Types


-- * setup

type States = (State.State, Cmd.State)

-- | Make a UI state with one block with the given tracks, and a standard cmd
-- state.
mkstates :: [UiTest.TrackSpec] -> States
mkstates tracks = (ui_state, mk_cmd_state ui_state UiTest.default_view_id)
    where
    ui_state = UiTest.exec State.empty $ do
        UiTest.mkblock_view (UiTest.default_block_name, tracks)
        State.set_selection UiTest.default_view_id Config.insert_selnum $
            Just $ Sel.selection 1 0 1 0

-- | Many cmds rely on a focused view, and it's easy to forget to add it, so
-- make it mandatory.
mk_cmd_state :: State.State -> ViewId -> Cmd.State
mk_cmd_state ui_state view_id = CmdTest.default_cmd_state
    { Cmd.state_focused_view = Just view_id
    -- Normally this is created by the setup cmd, so pretend I did one.
    , Cmd.state_history = Cmd.History [] present [] Nothing
    }
    where present = Cmd.HistoryEntry ui_state [] ["setup"] Nothing


-- * result

data Result = Result {
    -- | Msg that gave rise to this Result.
    result_msg :: Msg.Msg
    -- | In CmdTest, Result has MIDI from the thru mechanism, and updates
    -- collected during the event.  But responder tests scope over the entire
    -- respond cycle, so MIDI potentially also includes play output and
    -- updates include diff output.
    , result_cmd :: CmdTest.Result Cmd.Status
    -- | These are the updates emitted to the UI.  The CmdTest.Result updates
    -- are those collected during the cmd.
    , result_updates :: [Update.DisplayUpdate]
    , result_loopback :: Chan.Chan Msg.Msg
    }

instance Pretty.Pretty Result where
    format result = Pretty.format (result_msg result) <> " -> "
        <> Pretty.format (CmdTest.result_logs (result_cmd result))

-- | Print Results and anything interesting in them.
print_results :: [Result] -> IO ()
print_results = mapM_ $ \result -> do
    Pretty.pprint (result_msg result)
    case result_msg result of
        Msg.DeriveStatus _ (Msg.DeriveComplete perf) ->
            mapM_ Pretty.pprint (Cmd.perf_logs perf)
        _ -> return ()
    let logs = CmdTest.result_logs (result_cmd result)
    unless (null logs) $ do
        putStrLn "\tcmd logs:"
        mapM_ Pretty.pprint logs

result_states :: Result -> States
result_states r = (result_ui_state r, result_cmd_state r)

result_cmd_state :: Result -> Cmd.State
result_cmd_state = CmdTest.result_cmd_state . result_cmd

result_ui_state :: Result -> State.State
result_ui_state = CmdTest.result_ui_state . result_cmd

-- | Wait for a DeriveComplete and get the performance from it.
--
-- TODO error-prone because if you call this on a Result that didn't
-- regenerate the performance this will hang forever
result_perf :: Result -> IO (BlockId, Cmd.Performance)
result_perf = get_perf . result_loopback

get_perf :: Chan.Chan Msg.Msg -> IO (BlockId, Cmd.Performance)
get_perf chan = do
    msg <- read_msg chan
    case msg of
        Nothing -> error "get_perf: time out reading chan"
        Just (Msg.DeriveStatus block_id (Msg.DeriveComplete perf)) ->
            return (block_id, perf)
        Just _ -> get_perf chan

-- | Feed msgs back into the responder until and including the matching Msg
-- or a timeout.  The Msg is responded to and all Results returned.
respond_until :: (Msg.Msg -> Bool) -> States -> Cmd.CmdT IO a -> IO [Result]
respond_until is_complete states cmd = do
    putStrLn "---------- new cmd"
    result <- respond_cmd states cmd
    continue_until is_complete result

-- | Continue feeding loopback msgs into the responder, or until I time out
-- reading from the loopback channel.
continue_until :: (Msg.Msg -> Bool) -> Result -> IO [Result]
continue_until is_complete result = reverse <$> go [] (result_states result)
    where
    chan = result_loopback result
    go accum states = do
        maybe_msg <- read_msg chan
        putStrLn $ "ResponderTest.continue_until: " <> prettys maybe_msg
        case maybe_msg of
            Nothing -> return accum
            Just msg -> do
                result <- respond1 (Just chan) states Nothing msg
                if is_complete msg
                    then return (result : accum)
                    else go (result : accum) (result_states result)

read_msg :: Chan.Chan Msg.Msg -> IO (Maybe Msg.Msg)
read_msg = Thread.timeout 7 . Chan.readChan

is_derive_complete :: Msg.Msg -> Bool
is_derive_complete (Msg.DeriveStatus _ (Msg.DeriveComplete {})) = True
is_derive_complete _ = False

-- * thread

-- TODO to do a realistic simulation, I think I need to asynchronously send
-- loopback results back into the input queue.  This will let me test that
-- cmds complete with a minimum of delay, don't get in loops, and the new
-- performance becomes available eventually.

thread :: Bool -> States -> [Msg.Msg] -> IO [Result]
thread print_timing states msgs =
    thread_delay print_timing states [(m, 0) | m <- msgs]

thread_delay :: Bool -> States -> [(Msg.Msg, Thread.Seconds)] -> IO [Result]
thread_delay _ _ [] = return []
thread_delay print_timing states ((msg, delay) : msgs) = do
    Printf.printf "thread msg: %s\n" (prettys msg)
    (result, cpu_secs, _secs) <- Testing.timer $ respond_msg states msg
    when print_timing $
        Printf.printf "%s -> lag: %.2fs\n" (prettys msg) cpu_secs
    Thread.delay delay
    (result:) <$> thread_delay print_timing (result_states result) msgs

-- * respond_cmd

configure_logging :: IO ()
configure_logging = do
    Log.configure $ \state -> state
        { Log.state_write_msg = Log.write_formatted IO.stdout }
    return ()

-- | Respond to a single Cmd.  This can be used to test cmds in the full
-- responder context without having to fiddle around with keymaps.
respond_cmd :: States -> Cmd.CmdT IO a -> IO Result
respond_cmd states cmd =
    configure_logging >> respond1 Nothing states (Just (mkcmd cmd)) magic
    where
    -- I run a cmd by adding a cmd that responds only to a specific Msg, and
    -- then sending that Msg.
    mkcmd cmd msg
        | is_magic msg = cmd >> return Cmd.Done
        | otherwise = return Cmd.Continue
    is_magic (Msg.Socket _ (ReplProtocol.QCommand "MAGIC!!")) = True
    is_magic _ = False
    magic = Msg.Socket IO.stdout (ReplProtocol.QCommand "MAGIC!!")

-- | Respond to a cmd, using the state from the result of the last cmd.
next :: Result -> Cmd.CmdT IO a -> IO Result
next = respond_cmd . result_states

respond_msg :: States -> Msg.Msg -> IO Result
respond_msg states = respond1 Nothing states Nothing

type CmdIO = Msg.Msg -> Cmd.CmdIO

respond1 :: Maybe (Chan.Chan Msg.Msg) -> States -> Maybe CmdIO
    -- ^ if given, this Cmd will get the Msg, otherwise whoever would normally
    -- respond will
    -> Msg.Msg -> IO Result
respond1 reuse_loopback (ui_state, cmd_state) maybe_cmd msg = do
    update_chan <- new_chan
    loopback_chan <- maybe Chan.newChan return reuse_loopback
    (interface, midi_chan) <- make_midi_interface
    ui_chan <- MVar.newMVar []
    let rstate = make_rstate ui_chan update_chan loopback_chan
            ui_state (set_cmd_state interface) maybe_cmd
    (_quit, rstate) <- Responder.respond rstate msg
    -- Updates and MIDI are normally forced by syncing with the UI and MIDI
    -- driver, so force explicitly here.  Not sure if this really makes
    -- a difference.
    midi <- get_vals midi_chan
    Testing.force midi
    updates <- concat <$> get_vals update_chan
    Testing.force updates
    let cmd_result = CmdTest.Result
            { CmdTest.result_val = Right Nothing
            , CmdTest.result_cmd_state = Responder.state_cmd rstate
            , CmdTest.result_ui_state = Responder.state_ui rstate
            , CmdTest.result_updates = []
            , CmdTest.result_logs = []
            , CmdTest.result_midi = midi
            }
    return $ Result
        { result_msg = msg
        , result_cmd = cmd_result
        , result_updates = updates
        , result_loopback = loopback_chan
        }
    where
    set_cmd_state interface = cmd_state
        { Cmd.state_config = (Cmd.state_config cmd_state)
            { Cmd.config_midi_interface = interface }
        , Cmd.state_derive_immediately =
            Map.keysSet (State.state_blocks ui_state)
        }

make_rstate :: Ui.Channel -> TVar.TVar [[Update.DisplayUpdate]]
    -> Chan.Chan Msg.Msg -> State.State -> Cmd.State -> Maybe CmdIO
    -> Responder.State
make_rstate ui_chan update_chan loopback_chan ui_state cmd_state maybe_cmd =
    Responder.State
        { Responder.state_static_config = config
        , Responder.state_ui = ui_state
        , Responder.state_cmd = cmd_state
        -- Tests probably link with the dummy interpreter so this is just
        -- return (), but profiling may try to use the interpreter.
        , Responder.state_session = Unsafe.unsafePerformIO Repl.make_session
        , Responder.state_loopback = loopback
        , Responder.state_sync = dummy_sync
        , Responder.state_monitor_state = play_monitor_state
        , Responder.state_ui_channel = ui_chan
        }
    where
    play_monitor_state = Unsafe.unsafePerformIO (MVar.newMVar State.empty)
    config = StaticConfig.empty
        { StaticConfig.global_cmds = maybe [] (:[]) maybe_cmd }
    dummy_sync _ _ _ updates = do
        put_val update_chan updates
        return Nothing
    loopback = Chan.writeChan loopback_chan

make_midi_interface :: IO (Interface.Interface, TVar.TVar [Interface.Message])
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
