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
data TransportConfig = TransportConfig
    { configRepeat :: MatchMidi
    , configBackward :: MatchMidi
    , configForward :: MatchMidi
    , configStop :: MatchMidi
    , configPlay :: MatchMidi
    , configRecord :: MatchMidi
    }

type MatchMidi = Midi.Message -> Bool

noteOn :: Midi.Key -> MatchMidi
noteOn key (Midi.ChannelMessage _ (Midi.NoteOn k _)) = key == k
noteOn _ _ = False

transport :: TransportConfig -> Msg.Msg -> Cmd.CmdT IO Cmd.Status
transport config msg = do
    msg <- Cmd.abortUnless $ Msg.midi msg
    if  | configRepeat config msg -> done Edit.toggleNoteDuration
        | configBackward config msg ->
            done $ Selection.step TimeStep.Rewind Selection.Replace
        | configForward config msg ->
            done $ Selection.step TimeStep.Advance Selection.Replace
        | configStop config msg -> done $ Play.cmdContextStop
        -- TODO configure what this is
        -- I could have a "default play" cmd
        | configPlay config msg -> Cmd.Play <$> Play.localTop
        | configRecord config msg -> done Edit.cmdToggleValEdit
        | otherwise -> Cmd.abort
    where done = (>> pure Cmd.Done)
