-- | Utilities for cmd tests.
module Cmd.CmdTest where
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
import qualified Perform.Transport as Transport

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified App.Config as Config


default_block_id = UiTest.default_block_id
default_view_id = UiTest.default_view_id

-- * running cmds

data Result val = Result {
    -- | A Nothing val means it aborted.
    result_val :: Either String (Maybe val, State.State, [Update.Update])
    , result_cmd_state :: Cmd.State
    , result_logs :: [Log.Msg]
    , result_midi :: [(Midi.WriteDevice, Midi.Message)]
    }

-- | Run cmd with the given tracks.
run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_tracks track_specs =
    run (DeriveTest.with_instrument ustate) default_cmd_state
    where (_, ustate) = UiTest.run_mkview track_specs

-- | Run a cmd and return everything you could possibly be interested in.
run :: State.State -> Cmd.State -> Cmd.CmdId a -> Result a
run ustate cmd_state0 cmd = Result val cmd_state logs midi_msgs
    where
    (cmd_state, midi_msgs, logs, result) = Cmd.run_id ustate cmd_state0 cmd
    val = case result of
        Left err -> Left (Pretty.pretty err)
        Right v -> Right v

-- | Run a Cmd and return just the value.
eval :: State.State -> Cmd.State -> Cmd.CmdId a -> a
eval ustate cstate cmd = case result_val (run ustate cstate cmd) of
    Left err -> error $ "eval got StateError: " ++ show err
    Right (Nothing, _, _) -> error "eval: cmd aborted"
    Right (Just val, _, _) -> val

-- | Like 'run', but with a selection on the given track at 0 and note
-- duration set to what will be a ScoreTime 1 with the ruler supplied by
-- UiTest.
run_sel :: TrackNum -> [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_sel tracknum track_specs cmd = run_tracks track_specs $ do
    -- Add one because UiTest inserts a rule at track 0.
    set_sel (tracknum+1) 0 (tracknum+1) 0
    cmd

run_again :: Result a -> Cmd.CmdId a -> Result a
run_again res cmd = case result_val res of
    Left _ -> res
    Right (_, ustate, _) -> run ustate (result_cmd_state res) cmd

update_perf :: State.State -> Result val -> IO (Result val)
update_perf ui_from result = case result_val result of
    Right (_, ui_to, updates) -> do
        cmd_state <- update_performance ui_from ui_to
            (result_cmd_state result) updates
        Thread.delay 0.2
        return $ result { result_cmd_state = cmd_state }
    _ -> return result

-- | Run a DeriveTest extractor on a CmdTest Result.
extract_derive :: (Score.Event -> a) -> Result _a -> ([a], [String])
extract_derive ex = DeriveTest.extract ex . extract_derive_result

-- | Reconstruct a Derive.Result from the root performance, or throw an
-- exception if there is a problem getting it.
extract_derive_result :: Result a -> Derive.Result
extract_derive_result res = case result_val res of
        Right (_, ustate, _) -> eval ustate (result_cmd_state res) mkres
        Left err -> error $ "can't extract_derive_result from error result: "
            ++ err
    where
    mkres = do
        Cmd.Performance cache events track_env _damage tempo closest_warp
            inv_tempo tsigs <- Perf.get_root
        return $ Derive.Result events cache tempo closest_warp inv_tempo tsigs
            track_env
            (error "can't fake a Derive.State for an extracted Result")

update_performance :: State.State -> State.State -> Cmd.State
    -> [Update.Update] -> IO Cmd.State
update_performance ui_from ui_to cmd_state cmd_updates = do
    updates <- case Diff.diff cmd_updates ui_from ui_to of
        Left err -> error $ "diff error: " ++ err
        Right updates -> return updates
    Performance.update_performance 0
        send_status ui_from ui_to cmd_state updates
    where send_status _bid _status = return ()

-- | Run several cmds, threading the state through.
thread :: State.State -> Cmd.State -> [Cmd.CmdId a]
    -> Either String (State.State, Cmd.State)
thread ustate cstate cmds = foldl f (Right (ustate, cstate)) cmds
    where
    f (Right (ustate, cstate)) cmd = case run ustate cstate cmd of
        Result (Right (_, ustate2, _)) cstate2 logs _ ->
            Log.trace_logs logs $ Right (ustate2, cstate2)
        Result (Left err) _ _ _ -> Left (show err)
    f (Left err) _ = Left err

default_cmd_state :: Cmd.State
default_cmd_state =
    (Cmd.initial_state DeriveTest.default_db Map.empty Call.All.scope)
        { Cmd.state_focused_view = Just default_view_id
        , Cmd.state_edit = default_edit_state
        , Cmd.state_play = default_play_state
        }

default_play_state :: Cmd.PlayState
default_play_state =
    Cmd.initial_play_state { Cmd.state_play_step = UiTest.step1 }

default_edit_state :: Cmd.EditState
default_edit_state = Cmd.initial_edit_state
    { Cmd.state_step = UiTest.step1
    , Cmd.state_note_duration = UiTest.step1
    }

-- ** cmds

set_sel :: (Cmd.M m) => Types.TrackNum -> ScoreTime -> Types.TrackNum
    -> ScoreTime -> m ()
set_sel t0 p0 t1 p1 = do
    let sel = Types.selection t0 p0 t1 p1
    State.set_selection UiTest.default_view_id Config.insert_selnum (Just sel)

set_sel_point :: (Cmd.M m) => Types.TrackNum -> ScoreTime -> m ()
set_sel_point tracknum pos = State.set_selection UiTest.default_view_id
    Config.insert_selnum (Just (Types.point_selection tracknum pos))


-- * extractors

-- | Run this on either 'extract' or 'extract_state' when you don't care about
-- the logs.
trace_logs :: Either a (b, [String]) -> Either a b
trace_logs res = case res of
    Right (b, logs) -> (if null logs then id else trace logs) (Right b)
    Left a -> Left a
    where
    trace = Trace.trace . Seq.strip . unlines . ("\tlogged:":)

-- ** val

extract :: (val -> e_val) -> Result val
    -> Either String (Maybe e_val, [String])
extract extract_val result = fmap ex (e_val result)
    where
    ex (val, logs) = (fmap extract_val val, map DeriveTest.show_log logs)
    e_val :: Result val -> Either String (Maybe val, [Log.Msg])
    e_val res = either Left (\(v, _, _) -> Right (v, result_logs res))
        (result_val res)

-- ** state

type Extract e = State.State -> Cmd.State -> e

-- | Get something out of the Result from one of the states.
extract_state :: (State.State -> Cmd.State -> e) -> Result val
    -> Either String (e, [String])
extract_state f res = case result_val res of
    Right (_, ustate, _) ->
        Right (f ustate (result_cmd_state res),
            map DeriveTest.show_log (result_logs res))
    Left err -> Left err

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
    (ustate { State.state_midi_config = default_midi_config inst_names},
        cstate { Cmd.state_instrument_db = make_inst_db inst_names })

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

empty_context = UiMsg.Context Nothing Nothing Nothing

make_key_mods :: [Key.Modifier] -> Bool -> Key.Key -> Msg.Msg
make_key_mods mods down k = Msg.Ui
    (UiMsg.UiMsg empty_context (UiMsg.MsgEvent (UiMsg.Kbd state mods k)))
    where state = if down then UiMsg.KeyDown else UiMsg.KeyUp

make_key :: Bool -> Key.Key -> Msg.Msg
make_key = make_key_mods []

key_down = make_key True . Key.KeyChar
key_up = make_key False . Key.KeyChar
backspace = make_key True Key.Backspace

mouse down btn = Msg.Ui $ UiMsg.UiMsg empty_context $
    UiMsg.MsgEvent (UiMsg.Mouse state [] (42, 2) 0 True)
    where state = if down then UiMsg.MouseDown btn else UiMsg.MouseUp btn

drag btn = Msg.Ui $ UiMsg.UiMsg empty_context $
    UiMsg.MsgEvent (UiMsg.Mouse (UiMsg.MouseDrag btn) [] (42, 2) 0 False)

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
    Cmd.modify_play_state $ \st -> st { Cmd.state_performance_threads =
        Map.insert root_id (make_pthread perf)
        (Cmd.state_performance_threads st) }
    where
    track_env = Map.singleton (block_id, track_id) (Map.fromList environ)
    perf = Cmd.Performance mempty [] track_env mempty
        dummy_tempo dummy_closest_warp dummy_inv_tempo mempty

make_pthread :: Cmd.Performance -> Cmd.PerformanceThread
make_pthread perf = Unsafe.unsafePerformIO $ do
    th <- Thread.start (return ())
    return $ Cmd.PerformanceThread perf th

dummy_tempo :: Transport.TempoFunction
dummy_tempo _ _ _ = []

dummy_closest_warp :: Transport.ClosestWarpFunction
dummy_closest_warp _ _ _ = Score.id_warp

dummy_inv_tempo :: Transport.InverseTempoFunction
dummy_inv_tempo _ = []
