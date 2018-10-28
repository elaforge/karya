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
    , mkstates, mkstates_blocks, mk_cmd_state
    -- * Result
    , Result(..), Seconds, print_results, result_states, result_ui_state
    , result_cmd_state
    , result_perf
    -- * run
    , respond_cmd
    , respond_all, continue_all
    , thread, thread_delay
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.StubMidi as StubMidi
import qualified Ui.Fltk as Fltk
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.Repl as Repl
import qualified Cmd.Responder as Responder

import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import qualified App.StaticConfig as StaticConfig

import Global
import Types


-- * setup

type States = (Ui.State, Cmd.State)

-- | Make a UI state with one block with the given tracks, and a standard cmd
-- state.
mkstates :: [UiTest.TrackSpec] -> States
mkstates tracks = mkstates_blocks [(UiTest.default_block_name, tracks)]

mkstates_blocks :: [UiTest.BlockSpec] -> (Ui.State, Cmd.State)
mkstates_blocks blocks =
    (ui_state, mk_cmd_state ui_state UiTest.default_view_id)
    where
    ui_state = UiTest.exec Ui.empty $ do
        root_id : _ <- UiTest.mkblocks blocks
        view_id <- UiTest.mkview root_id
        Ui.set_selection view_id Config.insert_selnum $
            Just $ Sel.Selection
                { start_track = 1, start_pos = 0
                , cur_track = 1, cur_pos = 0
                , orientation = Sel.Positive
                }

-- | Many cmds rely on a focused view, and it's easy to forget to add it, so
-- make it mandatory.
mk_cmd_state :: Ui.State -> ViewId -> Cmd.State
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
    -- | CPU seconds it took for this Result to come back.  This doesn't
    -- include waiting for the Msg.
    , result_time :: Seconds
    }

type Seconds = Double

instance Pretty Result where
    format result = Pretty.format (result_msg result) <> " -> "
        <> Pretty.format (CmdTest.result_logs (result_cmd result))

-- | Print Results and anything interesting in them.
print_results :: [Result] -> IO ()
print_results = mapM_ $ \result -> do
    Pretty.pprint (result_msg result)
    case result_msg result of
        Msg.DeriveStatus _ (Msg.DeriveComplete perf _) ->
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

result_ui_state :: Result -> Ui.State
result_ui_state = CmdTest.result_ui_state . result_cmd

-- | Wait for a DeriveComplete and get the performance from it.
--
-- This is error-prone because if you call this on a Result that didn't
-- regenerate the performance this will hang until 'read_msg' gives up.
result_perf :: Result -> IO (BlockId, Cmd.Performance)
result_perf = get_perf . result_loopback

get_perf :: Chan.Chan Msg.Msg -> IO (BlockId, Cmd.Performance)
get_perf chan = do
    msg <- read_msg 3 chan
    case msg of
        Nothing -> errorIO "get_perf: time out reading chan"
        Just (Msg.DeriveStatus block_id (Msg.DeriveComplete perf _)) ->
            return (block_id, perf)
        Just _ -> get_perf chan

-- | Continue feeding loopback msgs into the responder until I see
-- DeriveCompletes for all the blocks I expect to derive.
--
-- This is a hack, because I want to make sure all msgs have been processed,
-- but there's no explicit way for the system to say it's done.  But since the
-- only thing chucking things in loopback is background derivation, I can use
-- what I know of how it works to guess when it's done.
--
-- It's dangerous to look for a particular DeriveComplete because the order
-- in which derive threads complete is non-deterministic.
respond_all :: Thread.Seconds -> States -> Cmd.CmdT IO a -> IO [Result]
respond_all timeout states cmd = do
    putStrLn "---------- new cmd"
    result <- respond_cmd states cmd
    (result:) <$> continue_all timeout result

continue_all :: Thread.Seconds -> Result -> IO [Result]
continue_all timeout prev_result = go Set.empty (result_states prev_result)
    where
    loopback = result_loopback prev_result
    expected_blocks = Set.fromList $
        Performance.derive_blocks (result_ui_state prev_result)
    go complete_blocks states
        | complete_blocks == expected_blocks = return []
        | otherwise = do
            maybe_msg <- read_msg timeout loopback
            Text.IO.putStrLn $ "ResponderTest.continue: "
                <> maybe "timed out!" pretty maybe_msg
            case maybe_msg of
                Nothing -> return []
                Just msg -> do
                    result <- respond1 (Just loopback) states Nothing msg
                    let completed = maybe id Set.insert (complete msg)
                            complete_blocks
                    (result:) <$> go completed (result_states result)
    complete (Msg.DeriveStatus block_id (Msg.DeriveComplete {})) = Just block_id
    complete _ = Nothing

read_msg :: Thread.Seconds -> Chan.Chan Msg.Msg -> IO (Maybe Msg.Msg)
read_msg timeout chan = do
    ci <- maybe False (not . null) <$> Environment.lookupEnv "CI_DONT_TIMEOUT"
    (if ci then fmap Just else Thread.timeout timeout) $ Chan.readChan chan

-- * thread

thread :: Bool -> States -> [Msg.Msg] -> IO [Result]
thread print_timing states msgs =
    thread_delay print_timing states [(m, 0) | m <- msgs]

thread_delay :: Bool -> States -> [(Msg.Msg, Thread.Seconds)] -> IO [Result]
thread_delay _ _ [] = return []
thread_delay print_timing states ((msg, delay) : msgs) = do
    Printf.printf "thread msg: %s\n" (prettys msg)
    result <- respond_msg states msg
    when print_timing $
        Printf.printf "%s -> lag: %.2fs\n" (prettys msg) (result_time result)
    Thread.delay delay
    (result:) <$> thread_delay print_timing (result_states result) msgs

-- * respond_cmd

configure_logging :: IO ()
configure_logging = do
    Log.configure $ \state -> state
        { Log.state_write_msg = Log.write_formatted IO.stdout
        -- Otherwise the status updates get spammy.
        , Log.state_log_level = Log.Notice
        }
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

respond_msg :: States -> Msg.Msg -> IO Result
respond_msg states = respond1 Nothing states Nothing

type CmdIO = Msg.Msg -> Cmd.CmdT IO Cmd.Status

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
    ((rstate, midi, updates), cpu_secs, _secs) <- Thread.timeAction $ do
        (_quit, rstate) <- Responder.respond rstate msg
        midi <- get_vals midi_chan
        Thread.force midi
        updates <- concat <$> get_vals update_chan
        Thread.force updates
        return (rstate, midi, updates)
    -- Updates and MIDI are normally forced by syncing with the UI and MIDI
    -- driver, so force explicitly here.  Not sure if this really makes
    -- a difference.
    let cmd_result = CmdTest.Result
            { CmdTest.result_val = Right Nothing
            , CmdTest.result_cmd_state = Responder.state_cmd rstate
            , CmdTest.result_ui_state = Responder.state_ui rstate
            , CmdTest.result_updates = []
            , CmdTest.result_logs = []
            , CmdTest.result_thru = map Cmd.MidiThru midi
            }
    return $ Result
        { result_msg = msg
        , result_cmd = cmd_result
        , result_updates = updates
        , result_loopback = loopback_chan
        , result_time = realToFrac cpu_secs
        }
    where
    set_cmd_state interface = cmd_state
        { Cmd.state_config = (Cmd.state_config cmd_state)
            { Cmd.config_midi_interface = interface }
        , Cmd.state_derive_immediately = Map.keysSet (Ui.state_blocks ui_state)
        }

make_rstate :: Fltk.Channel -> TVar.TVar [[Update.DisplayUpdate]]
    -> Chan.Chan Msg.Msg -> Ui.State -> Cmd.State -> Maybe CmdIO
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
    play_monitor_state = Unsafe.unsafePerformIO (MVar.newMVar Ui.empty)
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
