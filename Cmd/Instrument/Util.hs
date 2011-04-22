-- | Functions for instrument cmds.
module Cmd.Instrument.Util where
import qualified Data.Map as Map
import Util.Control

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.NoteTrack as NoteTrack

import qualified Util.Log as Log


-- * keymap

-- | Create a note entry Cmd for a keymapped instrument.
--
-- Like NoteEntry, if kbd entry is on but ValEdit is not, then play the sound
-- but don't enter the note.
keymaps :: (Cmd.M m) => [(Char, String, Midi.Key)] -> Msg.Msg -> m Cmd.Status
keymaps inputs = \msg -> do
    unlessM Cmd.is_kbd_entry Cmd.abort
    EditUtil.fallthrough msg
    (down, char) <- Cmd.require $ Msg.key_char msg
    -- Do nothing but return Done if there is no mapping for this key.  That
    -- way this cmd captures all keystrokes and completely shadows the normal
    -- kbd entry.  Otherwise, it's confusing when some keys fall through and
    -- create pitches.
    --
    -- TODO another way to accomplish this would be to put the NoteEntry stuff
    -- in as a default instrument cmd, so I could just replace it entirely.
    case Map.lookup char to_note of
        Nothing -> return Cmd.Done
        Just (note, key) -> do
            -- Log.warn $ show (down, char, note, key, repeat)
            unlessM (Keymap.is_repeat msg) $
                if down then keymap_down note key else keymap_up key
            return Cmd.Done
    where
    to_note = Map.fromList [(char, (note, key)) | (char, note, key) <- inputs]

keymap_down :: (Cmd.M m) => String -> Midi.Key -> m ()
keymap_down note key = do
    whenM Cmd.is_val_edit $
        NoteTrack.modify_event True $ const (Just note, True)
    MidiThru.channel_message (Midi.NoteOn key 64)

keymap_up :: (Cmd.M m) => Midi.Key -> m ()
keymap_up key = MidiThru.channel_message (Midi.NoteOff key 64)

-- * keyswitch

-- emit raw keyswitch, then call cmd_midi_thru with InputNote.NoteOn
keyswitches :: (Cmd.M m) => [(Char, String, Midi.Key)]
    -> Msg.Msg -> m Cmd.Status
keyswitches inputs = undefined

keyswitch :: (Cmd.M m) => Char -> String -> Midi.Key -> [Keymap.Binding m]
keyswitch char text midi_key = Keymap.bind_char char ("keyswitch " ++ text)
    (input_keyswitch text midi_key)

-- | Set note switch, emit keyswitch and note at current pitch.
input_keyswitch :: (Cmd.M m) => String -> Midi.Key -> m ()
input_keyswitch text key = do
    Cmd.modify_edit_state $ \st -> st { Cmd.state_note_text = text }
    -- TODO how to find out current pitch?
    let chan = 0
        wdev = undefined
    -- TODO find out current addr
    Cmd.midi wdev (Midi.ChannelMessage chan (Midi.NoteOn key 64))
    Cmd.midi wdev (Midi.ChannelMessage chan (Midi.NoteOff key 64))
    return ()
