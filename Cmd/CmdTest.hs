-- | Utilities for cmd tests.
module Cmd.CmdTest where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Midi.Midi as Midi

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Simple as Simple
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Midi.Controller

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config


default_block_id = UiTest.default_block_id
default_view_id = UiTest.default_view_id

-- | Run a cmd and return everything you could possibly be interested in.
-- Will be Nothing if the cmd aborted.
run :: State.State -> Cmd.State -> Cmd.CmdT Identity.Identity a
    -> (Either State.StateError
        (Maybe a, State.State, Cmd.State, [Log.Msg]))
run ustate cstate cmd = case Cmd.run_id ustate cstate cmd of
    (cmd_state2, _midi_msgs, logs, result) -> case result of
        Left err -> Left err
        Right (val, ui_state2, _updates) ->
            Right (val, ui_state2, cmd_state2, logs)

eval :: State.State -> Cmd.State -> Cmd.CmdT Identity.Identity a -> a
eval ustate cstate cmd = case run ustate cstate cmd of
    Left err -> error $ "eval got StateError: " ++ show err
    Right (Nothing, _, _, _) -> error $ "eval: cmd aborted"
    Right (Just val, _, _, _) -> val

-- | Run several cmds, threading the state through.
thread :: State.State -> Cmd.State -> [Cmd.CmdT Identity.Identity a]
    -> Either String (State.State, Cmd.State, [[String]])
thread ustate cstate cmds = foldl f (Right (ustate, cstate, [])) cmds
    where
    f (Right (ustate, cstate, logs)) cmd = case run ustate cstate cmd of
        Right (_val, ustate2, cstate2, logs2) ->
            Right (ustate2, cstate2, logs ++ [map Log.msg_string logs2])
        Left err -> Left (show err)
    f (Left err) _ = Left err

extract_logs result = case result of
    Right (Just _, _, _, logs) -> Right (Just (map Log.msg_string logs))
    Right (Nothing, _, _, _) -> Right Nothing
    Left err -> Left (show err)

with_sel :: (Monad m) => Maybe Types.Selection -> Cmd.CmdT m a -> Cmd.CmdT m a
with_sel sel cmd = do
    State.set_selection UiTest.default_view_id Config.insert_selnum sel
    Cmd.modify_state $ \st ->
        st { Cmd.state_focused_view = Just UiTest.default_view_id }
    cmd

-- | Run cmd with the given tracks, and return the resulting tracks.
run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdT Identity.Identity a
    -> Either String (Maybe a, [(String, [Simple.Event])], [String])
run_tracks track_specs cmd =
    case run ustate cmd_state cmd of
        Right (val, ustate2, _cstate2, logs) ->
            Right (val, UiTest.extract_tracks ustate2, map Log.msg_string logs)
        Left err -> Left (show err)
    where (_, ustate) = UiTest.run_mkview track_specs

cmd_state = Cmd.empty_state
    { Cmd.state_focused_view = Just default_view_id
    }


-- | Configure ustate and cstate with the given instruments.
set_insts inst_names ustate cstate =
    (ustate { State.state_midi_config = default_midi_config inst_names},
        cstate { Cmd.state_instrument_db = make_inst_db inst_names })

make_inst_db inst_names = Instrument.Db.empty
    { Instrument.Db.db_lookup_midi = make_lookup inst_names }

default_midi_config inst_names =
    Instrument.Config (Map.fromList (zip insts addrs)) (Just (head insts))
    where
    insts = map Score.Instrument inst_names
    addrs = [[(default_wdev, chan)] | chan <- [0..]]
default_wdev = Midi.WriteDevice "test"

make_lookup :: [String] -> MidiDb.LookupMidiInstrument
make_lookup inst_names _attrs (Score.Instrument inst) = Map.lookup inst inst_map
    where inst_map = Map.fromList $ zip inst_names (map make_inst inst_names)

make_inst name = default_perf_inst { Instrument.inst_name = name }
default_perf_inst = Instrument.instrument "synth" "i0" Nothing
            Midi.Controller.empty_map (-2, 2)
default_synth = Instrument.Synth "synth" default_wdev
    (Midi.Controller.controller_map [])


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
    Midi.ReadMessage (Midi.ReadDevice "test") Timestamp.immediately
        (Midi.ChannelMessage 0 chan_msg)

note_on note_id nn vel =
    InputNote.NoteOn (nid note_id) (Pitch.InputKey nn) (vel / 127)
note_off note_id vel = InputNote.NoteOff (nid note_id) (vel / 127)
control note_id cont val =
    InputNote.Control (nid note_id) (Score.Controller cont) (val / 127)
pitch note_id nn = InputNote.PitchChange (nid note_id) (Pitch.InputKey nn)
nid = InputNote.NoteId

m_note_on note_id nn vel = Msg.InputNote (note_on note_id nn vel)
m_note_off note_id vel = Msg.InputNote (note_off note_id vel)
m_control note_id cont val = Msg.InputNote (control note_id cont val)
m_pitch note_id nn = Msg.InputNote (pitch note_id nn)
