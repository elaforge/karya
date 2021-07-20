-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to interpret hardware controllers.  This basically means magic MIDI
-- msgs from a keyboard or something which is intended to do something other
-- than play a note.
module Cmd.Controller where
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Msg as Msg
import qualified Cmd.Play as Play
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep


-- | Buttons you might find on the transport section of a controller keyboard.
-- Since everyone's setup is different, this is expected to be configured in
-- "Local.Config".
data TransportConfig = TransportConfig {
    config_repeat :: MatchMidi
    , config_backward :: MatchMidi
    , config_forward :: MatchMidi
    , config_stop :: MatchMidi
    , config_play :: MatchMidi
    , config_record :: MatchMidi
    }

type MatchMidi = Midi.Message -> Bool

note_on :: Midi.Key -> MatchMidi
note_on key (Midi.ChannelMessage _ (Midi.NoteOn k _)) = key == k
note_on _ _ = False

transport :: TransportConfig -> Msg.Msg -> Cmd.CmdT IO Cmd.Status
transport config msg = do
    msg <- Cmd.abort_unless $ Msg.midi msg
    if  | config_repeat config msg -> done Edit.toggle_note_duration
        | config_backward config msg ->
            done $ Selection.step TimeStep.Rewind Selection.Replace
        | config_forward config msg ->
            done $ Selection.step TimeStep.Advance Selection.Replace
        | config_stop config msg -> done $ Play.cmd_context_stop
        -- TODO configure what this is
        -- I could have a "default play" cmd
        | config_play config msg -> Cmd.Play <$> Play.local_top
        | config_record config msg -> done Edit.cmd_toggle_val_edit
        | otherwise -> Cmd.abort
    where done = (>> return Cmd.Done)
