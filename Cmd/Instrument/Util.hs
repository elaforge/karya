-- | Functions for instrument cmds.
module Cmd.Instrument.Util where
import qualified Data.Map as Map

import Util.Control
import qualified Midi.Midi as Midi
import qualified Ui.UiMsg as UiMsg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack


-- * keymap

-- | Create a note entry Cmd for a keymapped instrument.
--
-- Like NoteEntry, if kbd entry is on but ValEdit is not, then play the sound
-- but don't enter the note.
keymaps :: (Cmd.M m) => [(Char, String, Midi.Key)] -> Msg.Msg -> m Cmd.Status
keymaps inputs = \msg -> do
    unlessM Cmd.is_kbd_entry Cmd.abort
    EditUtil.fallthrough msg
    (kstate, char) <- Cmd.require $ Msg.char msg
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
            case kstate of
                UiMsg.KeyRepeat -> return ()
                UiMsg.KeyDown -> keymap_down note key
                UiMsg.KeyUp -> keymap_up key
            return Cmd.Done
    where
    to_note = Map.fromList [(char, (note, key)) | (char, note, key) <- inputs]

keymap_down :: (Cmd.M m) => String -> Midi.Key -> m ()
keymap_down note key = do
    whenM Cmd.is_val_edit $
        NoteTrack.modify_event False True $ const (Just note, True)
    MidiThru.channel_messages True [Midi.NoteOn key 64]

keymap_up :: (Cmd.M m) => Midi.Key -> m ()
keymap_up key = MidiThru.channel_messages True [Midi.NoteOff key 64]

-- * keyswitch

-- | Create a Cmd to set keyswitches.
--
-- This simply sets the note text for subsequent notes, and also configures the
-- instrument to play in the given keyswitch.
--
-- TODO this just emits keyswitches for every addr and emits them redundantly.
-- This is simpler but it would be more correct to use WriteDeviceState to
-- emit them only when needed.  However, it's more complicated because then
-- I need a current attrs (Map Instrument Attrs) along with current note text,
-- so MidiThru can use the attrs to find the keyswitch.
--
-- TODO if I can pull the current or previous note out of the derive then I
-- could use that to play an example note.  Wait until I have a "play current
-- line" framework up for that.
keyswitches :: (Cmd.M m) => [(Char, String, Midi.Key)]
    -> Msg.Msg -> m Cmd.Status
keyswitches inputs = \msg -> do
    EditUtil.fallthrough msg
    char <- Cmd.require $ Msg.char_down msg
    (note, key) <- Cmd.require $ Map.lookup char to_note
    MidiThru.channel_messages False [Midi.NoteOn key 64, Midi.NoteOff key 64]
    Cmd.set_note_text note
    return Cmd.Done
    where
    to_note = Map.fromList [(char, (note, key)) | (char, note, key) <- inputs]
