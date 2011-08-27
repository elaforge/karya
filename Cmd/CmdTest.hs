-- | Utilities for cmd tests.
module Cmd.CmdTest where
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Map as Map
import qualified Debug.Trace as Trace
import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi
import Ui
import qualified Ui.Diff as Diff
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import qualified Cmd.Simple as Simple

import qualified Derive.Call.All as Call.All
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified App.Config as Config


-- * running cmds

data Result val = Result {
    -- | A Nothing val means it aborted.
    result_val :: Either String (Maybe val)
    , result_cmd_state :: Cmd.State
    , result_ui_state :: State.State
    , result_updates :: [Update.CmdUpdate]
    , result_logs :: [Log.Msg]
    , result_midi :: [(Midi.WriteDevice, Midi.Message)]
    }

result_failed :: Result a -> Maybe String
result_failed res = case result_val res of
    Right (Just _) -> Nothing
    Right Nothing -> Just "aborted"
    Left err -> Just err

-- | Run cmd with the given tracks.
run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_tracks track_specs =
    run (DeriveTest.with_instrument ustate) default_cmd_state
    where (_, ustate) = UiTest.run_mkview track_specs

-- | Run a cmd and return everything you could possibly be interested in.
run :: State.State -> Cmd.State -> Cmd.CmdId a -> Result a
run ustate1 cstate1 cmd = Result val cstate2 ustate2 updates logs midi_msgs
    where
    (cstate2, midi_msgs, logs, result) = Cmd.run_id ustate1 cstate1 cmd
    (val, ustate2, updates) = case result of
        Left err -> (Left (Pretty.pretty err), ustate1, [])
        Right (v, ustate2, updates) -> (Right v, ustate2, updates)

-- | Run a Cmd and return just the value.
eval :: State.State -> Cmd.State -> Cmd.CmdId a -> a
eval ustate cstate cmd = case result_val (run ustate cstate cmd) of
    Left err -> error $ "eval got StateError: " ++ show err
    Right Nothing -> error "eval: cmd aborted"
    Right (Just val) -> val

-- | Like 'run', but with a selection on the given track at 0 and note
-- duration set to what will be a ScoreTime 1 with the ruler supplied by
-- UiTest.
run_sel :: TrackNum -> [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_sel tracknum track_specs cmd = run_tracks track_specs $ do
    -- Add one because UiTest inserts a rule at track 0.
    set_sel (tracknum+1) 0 (tracknum+1) 0
    cmd

run_again :: Result a -> Cmd.CmdId b -> Result b
run_again res = run (result_ui_state res) (result_cmd_state res)

update_perf :: State.State -> Result val -> IO (Result val)
update_perf ui_from res = do
    cmd_state <- update_performance ui_from (result_ui_state res)
        (result_cmd_state res) (result_updates res)
    return $ res { result_cmd_state = cmd_state }

-- | Run a DeriveTest extractor on a CmdTest Result.
extract_derive :: (Score.Event -> a) -> Result _a -> ([a], [String])
extract_derive ex = DeriveTest.extract ex . extract_derive_result

-- | Reconstruct a Derive.Result from the root performance, or throw an
-- exception if there is a problem getting it.
extract_derive_result :: Result a -> Derive.Result
extract_derive_result res =
    maybe (eval (result_ui_state res) (result_cmd_state res) mkres)
        (error . (msg++)) (result_failed res)
    where
    msg = "extract_derive_result: cmd failed so result is probably not right: "
    mkres = do
        Cmd.Performance cache events track_env _damage warps tsigs
            <- Perf.get_root
        return $ Derive.Result events cache warps tsigs track_env
            (error "can't fake a Derive.State for an extracted Result")

update_performance :: State.State -> State.State -> Cmd.State
    -> [Update.CmdUpdate] -> IO Cmd.State
update_performance ui_from ui_to cmd_state cmd_updates = do
    (cupdates, _dupdates) <- case Diff.diff cmd_updates ui_from ui_to of
        Left err -> error $ "diff error: " ++ err
        Right updates -> return updates
    chan <- Chan.newChan
    cstate <- Performance.update_performance 0
        (\bid status -> Chan.writeChan chan (bid, status))
        ui_from ui_to cmd_state cupdates
    -- This will delay until the perform thread has derived enough of the
    -- performance to hand over.
    (block_id, perf) <- read_perf chan
    let insert st = st { Cmd.state_performance = Map.insert block_id perf
            (Cmd.state_performance st) }
    return $ cstate { Cmd.state_play = insert (Cmd.state_play cstate) }
    where
    read_perf chan = do
        (block_id, status) <- Chan.readChan chan
        case status of
            Msg.DeriveComplete perf -> return (block_id, perf)
            _ -> read_perf chan

-- | Run several cmds, threading the state through.  The first cmd that fails
-- aborts the whole operation.
thread :: State.State -> Cmd.State -> [Cmd.CmdId a]
    -> Either String (State.State, Cmd.State)
thread ustate cstate cmds = foldl f (Right (ustate, cstate)) cmds
    where
    f (Right (ustate, cstate)) cmd = case run ustate cstate cmd of
        Result (Right _) cstate2 ustate2 _ logs _ ->
            Log.trace_logs logs $ Right (ustate2, cstate2)
        Result (Left err) _ _ _ _ _ -> Left (show err)
    f (Left err) _ = Left err

default_cmd_state :: Cmd.State
default_cmd_state =
    (Cmd.initial_state DeriveTest.default_db Map.empty Call.All.scope)
        { Cmd.state_focused_view = Just UiTest.default_view_id
        , Cmd.state_edit = default_edit_state
        , Cmd.state_play = default_play_state
        }

default_play_state :: Cmd.PlayState
default_play_state =
    Cmd.initial_play_state { Cmd.state_play_step = UiTest.step1 }

default_edit_state :: Cmd.EditState
default_edit_state = Cmd.initial_edit_state
    { Cmd.state_time_step = UiTest.step1
    , Cmd.state_note_duration = UiTest.step1
    }

-- ** cmds

set_sel :: (Cmd.M m) => Types.TrackNum -> ScoreTime -> Types.TrackNum
    -> ScoreTime -> m ()
set_sel t0 p0 t1 p1 = do
    let sel = Types.selection t0 p0 t1 p1
    State.set_selection UiTest.default_view_id Config.insert_selnum (Just sel)

set_point_sel :: (State.M m) => Types.TrackNum -> ScoreTime -> m ()
set_point_sel = set_point_sel_block UiTest.default_block_name

-- | Set a point selection on the default view of the given block name.
set_point_sel_block :: (State.M m) => String -> Types.TrackNum -> ScoreTime
    -> m ()
set_point_sel_block block_name tracknum pos = State.set_selection view_id
        Config.insert_selnum (Just (Types.point_selection tracknum pos))
    where view_id = UiTest.mk_vid_name block_name


-- * extractors

-- | Run this on either 'extract' or 'extract_state' when you don't care about
-- the logs.
trace_logs :: Either a (b, [String]) -> Either a b
trace_logs res = case res of
    Right (b, logs) -> (if null logs then id else trace logs) (Right b)
    Left a -> Left a
    where
    trace = Trace.trace . Seq.strip . unlines . ("\tlogged:":)

e_logs :: Result a -> [String]
e_logs = map DeriveTest.show_log . DeriveTest.trace_low_prio . result_logs

-- ** val

-- | Extract the value from a cmd.  This is meant to be used as the single
-- check on a cmd operation, so it also returns logs and whether the cmd
-- failed or not (the latter is mandatory since otherwise there is no value).
extract :: (val -> e_val) -> Result val
    -> Either String ((Maybe e_val), [String])
extract f res = case result_val res of
    Right val -> Right (fmap f val, e_logs res)
    Left err -> Left err

-- ** state

-- | Get something out of the Result from one of the states.  Like 'extract',
-- this is meant to be used as the single check on a cmd operation.
extract_state :: (State.State -> Cmd.State -> e) -> Result val
    -> Either String (e, [String])
extract_state f res = maybe
    (Right (f (result_ui_state res) (result_cmd_state res), e_logs res))
    Left (result_failed res)

e_tracks :: Result a -> Either String ([(String, [Simple.Event])], [String])
e_tracks = extract_state $ \state _ -> UiTest.extract_tracks state

extract_ui :: State.StateId e -> Result v -> Either String (e, [String])
extract_ui m = extract_state $ \state _ -> UiTest.eval state m

-- * inst db

-- TODO see about getting rid of the inst stuff in favor of using the code
-- from DeriveTest

-- | Configure ustate and cstate with the given instruments.
set_insts :: [String] -> State.State -> Cmd.State -> (State.State, Cmd.State)
set_insts inst_names ustate cstate =
    (ustate { State.state_config = set (State.state_config ustate) },
        cstate { Cmd.state_instrument_db = make_inst_db inst_names })
    where
    set config = config { State.config_midi = default_midi_config inst_names }

make_inst_db :: [String] -> Instrument.Db.Db code
make_inst_db inst_names = Instrument.Db.empty
    { Instrument.Db.db_lookup_midi = make_lookup inst_names }

default_midi_config inst_names =
    Instrument.Config (Map.fromList (zip insts addrs))
    where
    insts = map Score.Instrument inst_names
    addrs = [[(Midi.WriteDevice default_wdev, chan)] | chan <- [0..]]
default_wdev = "test"

make_lookup :: [String] -> MidiDb.LookupMidiInstrument
make_lookup inst_names _attrs (Score.Instrument inst) =
    fmap (\inst -> (inst, Score.no_attrs)) (Map.lookup inst inst_map)
    where inst_map = Map.fromList $ zip inst_names (map make_inst inst_names)

make_inst name = default_perf_inst { Instrument.inst_name = name }
default_perf_inst = Instrument.instrument "i0" [] (-2, 2)
default_synth = Instrument.set_device "test" $ Instrument.synth "synth" []


-- * msg

empty_context :: UiMsg.Context
empty_context = UiMsg.Context Nothing Nothing

make_key_mods :: [Key.Modifier] -> UiMsg.KbdState -> Key.Key -> Msg.Msg
make_key_mods mods state k = Msg.Ui
    (UiMsg.UiMsg empty_context (UiMsg.MsgEvent (UiMsg.Kbd state mods k)))

make_key :: UiMsg.KbdState -> Key.Key -> Msg.Msg
make_key = make_key_mods []

keypress :: Key.Key -> [Msg.Msg]
keypress k = [make_key UiMsg.KeyDown k, make_key UiMsg.KeyUp k]

keypresses :: [Key.Key] -> [Msg.Msg]
keypresses = concatMap keypress

key_down = make_key UiMsg.KeyDown . Key.Char
key_up = make_key UiMsg.KeyUp . Key.Char
backspace = make_key UiMsg.KeyDown Key.Backspace

mouse :: Bool -> Types.MouseButton -> TrackNum -> ScoreTime -> Msg.Msg
mouse down btn track pos = Msg.Ui $ UiMsg.UiMsg context $
    UiMsg.MsgEvent (UiMsg.Mouse state [] (42, 2) 0 True)
    where
    state = if down then UiMsg.MouseDown btn else UiMsg.MouseUp btn
    context = UiMsg.Context Nothing (Just (track, UiMsg.Track pos))

drag :: Types.MouseButton -> TrackNum -> ScoreTime -> Msg.Msg
drag btn track pos = Msg.Ui $ UiMsg.UiMsg context $
    UiMsg.MsgEvent (UiMsg.Mouse (UiMsg.MouseDrag btn) [] (42, 2) 0 False)
    where
    context = UiMsg.Context Nothing (Just (track, UiMsg.Track pos))

make_midi :: Midi.ChannelMessage -> Msg.Msg
make_midi chan_msg = Msg.Midi $
    Midi.ReadMessage (Midi.ReadDevice "test") 0 (Midi.ChannelMessage 0 chan_msg)

note_on note_id nn vel =
    InputNote.NoteOn (nid note_id) (Pitch.InputKey nn) (vel / 127)
note_off note_id vel = InputNote.NoteOff (nid note_id) (vel / 127)
control note_id cont val =
    InputNote.Control (nid note_id) (Score.Control cont) (val / 127)
pitch note_id nn = InputNote.PitchChange (nid note_id) (Pitch.InputKey nn)
nid = InputNote.NoteId

m_note_on note_id nn vel = Msg.InputNote (note_on note_id nn vel)
m_note_off note_id vel = Msg.InputNote (note_off note_id vel)
m_control note_id cont val = Msg.InputNote (control note_id cont val)
m_pitch note_id nn = Msg.InputNote (pitch note_id nn)


-- * setup cmds

set_scale :: (Cmd.M m) => BlockId -> BlockId -> TrackId -> Pitch.ScaleId
    -> m ()
set_scale root_id block_id track_id scale_id =
    set_env root_id block_id track_id
        [(TrackLang.v_scale, TrackLang.VScaleId scale_id)]

-- | Fake up just enough Performance to have environ in it.
set_env :: (Cmd.M m) => BlockId -> BlockId -> TrackId
    -> [(TrackLang.ValName, TrackLang.Val)] -> m ()
set_env root_id block_id track_id environ =
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_performance_threads = Map.insert root_id
            (Unsafe.unsafePerformIO (Thread.start (return ())))
            (Cmd.state_performance_threads st)
        , Cmd.state_performance = Map.insert root_id perf
            (Cmd.state_performance st)
        }
    where
    track_env = Map.singleton (block_id, track_id) (Map.fromList environ)
    perf = Cmd.Performance mempty [] track_env mempty [] mempty
