-- | Utilities for cmd tests.
module Cmd.CmdTest where
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Midi.Midi as Midi

import Ui
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Simple as Simple
import qualified Cmd.TimeStep as TimeStep

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

-- | Run cmd with the given tracks.
run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_tracks track_specs = run ustate default_cmd_state
    where (_, ustate) = UiTest.run_mkview track_specs

-- | Run a cmd and return everything you could possibly be interested in.
-- Will be Nothing if the cmd aborted.
run :: State.State -> Cmd.State -> Cmd.CmdId a -> Result a
run ustate cstate cmd = case Cmd.run_id ustate cstate cmd of
    (cmd_state2, _midi_msgs, logs, result) -> case result of
        Left err -> Left (show err)
        Right (val, ui_state2, _updates) ->
            Right (val, ui_state2, cmd_state2, logs)

-- | Like 'run', but with a selection on track 1 at 0, and and note duration
-- set to what will be a ScoreTime 1 with the ruler supplied by UiTest.
run_sel :: TrackNum -> [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_sel tracknum track_specs cmd = run_tracks track_specs $ do
    -- Add one because UiTest inserts a rule at track 0.
    set_sel (tracknum+1) 0 (tracknum+1) 0
    Cmd.modify_edit_state $ \st -> st { Cmd.state_note_duration = step }
    cmd
    where
    step = TimeStep.AbsoluteMark TimeStep.AllMarklists (TimeStep.MatchRank 3 0)

type Result val =
    Either String (Maybe val, State.State, Cmd.State, [Log.Msg])

e_val :: Result val -> Either String (Maybe val, [Log.Msg])
e_val = fmap (\(v, _, _, logs) -> (v, logs))

e_ustate :: (State.State -> e_val) -> (Log.Msg -> e_log) -> Result _val
    -> Either String (e_val, [e_log])
e_ustate e_ustate e_log = fmap $ \(_, ustate, _, logs) ->
    (e_ustate ustate, map e_log logs)

e_tracks :: Result _val -> Either String [(String, [Simple.Event])]
e_tracks result = fmap ex $ e_ustate UiTest.extract_tracks id result
    where ex (val, logs) = (Log.trace_logs logs val)

extract :: (val -> e_val) -> Result val -> Either String (Maybe e_val, [String])
extract extract_val result = fmap ex (e_val result)
    where ex (val, logs) = (fmap extract_val val, map DeriveTest.show_log logs)

eval :: State.State -> Cmd.State -> Cmd.CmdId a -> a
eval ustate cstate cmd = case run ustate cstate cmd of
    Left err -> error $ "eval got StateError: " ++ show err
    Right (Nothing, _, _, _) -> error $ "eval: cmd aborted"
    Right (Just val, _, _, _) -> val

-- | Run several cmds, threading the state through.
thread :: State.State -> Cmd.State -> [Cmd.CmdId a]
    -> Either String (State.State, Cmd.State)
thread ustate cstate cmds = foldl f (Right (ustate, cstate)) cmds
    where
    f (Right (ustate, cstate)) cmd = case run ustate cstate cmd of
        Right (_val, ustate2, cstate2, logs) ->
            Log.trace_logs logs $ Right (ustate2, cstate2)
        Left err -> Left (show err)
    f (Left err) _ = Left err

extract_logs :: Result a -> Either String (Maybe [String])
extract_logs result = case result of
    Right (Just _, _, _, logs) -> Right (Just (map Log.msg_string logs))
    Right (Nothing, _, _, _) -> Right Nothing
    Left err -> Left (show err)

set_sel :: (Cmd.M m) => Types.TrackNum -> ScoreTime -> Types.TrackNum
    -> ScoreTime -> m ()
set_sel t0 p0 t1 p1 = do
    let sel = Types.selection t0 p0 t1 p1
    State.set_selection UiTest.default_view_id Config.insert_selnum (Just sel)

default_cmd_state = Cmd.empty_state
    { Cmd.state_focused_view = Just default_view_id
    }


-- | Configure ustate and cstate with the given instruments.
set_insts inst_names ustate cstate =
    (ustate { State.state_midi_config = default_midi_config inst_names},
        cstate { Cmd.state_instrument_db = make_inst_db inst_names })

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

make_key :: Bool -> Key.Key -> Msg.Msg
make_key down k = Msg.Ui
    (UiMsg.UiMsg empty_context (UiMsg.MsgEvent (UiMsg.Kbd state k)))
    where state = if down then UiMsg.KeyDown else UiMsg.KeyUp

key_down = make_key True . Key.KeyChar
key_up = make_key False . Key.KeyChar
backspace = make_key True Key.Backspace

mouse down btn = Msg.Ui $ UiMsg.UiMsg empty_context $
    UiMsg.MsgEvent (UiMsg.Mouse state (42, 2) 0 True)
    where state = if down then UiMsg.MouseDown btn else UiMsg.MouseUp btn

drag btn = Msg.Ui $ UiMsg.UiMsg empty_context $
    UiMsg.MsgEvent (UiMsg.Mouse (UiMsg.MouseDrag btn) (42, 2) 0 False)

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

set_scale :: (Cmd.M m) => BlockId -> BlockId -> TrackId -> Pitch.ScaleId -> m ()
set_scale root_id block_id track_id scale_id =
    set_env root_id block_id track_id
        [(TrackLang.v_scale, TrackLang.VScaleId scale_id)]

-- | Fake up just enough Performance to have environ in it.
set_env :: (Cmd.M m) => BlockId -> BlockId -> TrackId
    -> [(TrackLang.ValName, TrackLang.Val)] -> m ()
set_env root_id block_id track_id environ = do
    Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
        Map.insert root_id (make_pthread perf)
        (Cmd.state_performance_threads st) }
    where
    track_env = Map.singleton (block_id, track_id) (Map.fromList environ)
    perf = Cmd.Performance mempty [] track_env mempty
        dummy_tempo dummy_closest_warp dummy_inv_tempo mempty

make_pthread :: Cmd.Performance -> Cmd.PerformanceThread
make_pthread perf = Unsafe.unsafePerformIO $ do
    th <- Thread.start (return ())
    selection_pos <- IORef.newIORef 0
    return $ Cmd.PerformanceThread perf th selection_pos

dummy_tempo :: Transport.TempoFunction
dummy_tempo _ _ _ = []

dummy_closest_warp :: Transport.ClosestWarpFunction
dummy_closest_warp _ _ _ = Score.id_warp

dummy_inv_tempo :: Transport.InverseTempoFunction
dummy_inv_tempo _ = []
