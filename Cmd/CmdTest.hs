-- | Utilities for cmd tests.
module Cmd.CmdTest where
import qualified Control.Monad.Identity as Identity

import qualified Util.Log as Log
import qualified Midi.Midi as Midi

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Simple as Simple
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Perform.Timestamp as Timestamp

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

with_sel sel cmd = do
    State.set_selection UiTest.default_view_id Config.insert_selnum sel
    Cmd.modify_state $ \st ->
        st { Cmd.state_focused_view = Just UiTest.default_view_id }
    cmd

-- | Run cmd with the given tracks, and return the resulting tracks.
run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdT Identity.Identity a
    -> Either String (Maybe a, [(String, [Simple.Event])], [String])
run_tracks track_specs cmd =
    case run ustate cstate cmd of
        Right (val, ustate2, _cstate2, logs) ->
            Right (val, extract_tracks ustate2, map Log.msg_text logs)
        Left err -> Left (show err)
    where
    cstate = cmd_state
    (_, ustate) = UiTest.run_mkview track_specs

extract_tracks ustate = map (\(_, title, events) -> (title, events)) tracks
    where
    ((_, _, tracks), _) =
        UiTest.run ustate (Simple.dump_block default_block_id)

cmd_state = Cmd.empty_state
    { Cmd.state_focused_view = Just default_view_id
    }


-- * msg

empty_context = UiMsg.Context Nothing Nothing Nothing

make_key :: Bool -> Key.Key -> Msg.Msg
make_key down k = Msg.Ui
    (UiMsg.UiMsg empty_context (UiMsg.MsgEvent (UiMsg.Kbd state k)))
    where state = if down then UiMsg.KeyDown else UiMsg.KeyUp

key_down = make_key True . Key.KeyChar
key_up = make_key False . Key.KeyChar
backspace = make_key True Key.Backspace

make_midi :: Midi.ChannelMessage -> Msg.Msg
make_midi chan_msg = Msg.Midi $
    Midi.ReadMessage (Midi.ReadDevice "test") Timestamp.immediately
        (Midi.ChannelMessage 0 chan_msg)
