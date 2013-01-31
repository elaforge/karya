-- | Functions for instrument cmds.
module Cmd.Instrument.Util where
import qualified Data.Map as Map

import Util.Control
import qualified Midi.Midi as Midi
import qualified Ui.UiMsg as UiMsg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Keymap as Keymap
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Call.Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


-- * keymap

keymaps :: (Cmd.M m) => [(Char, String, Midi.Key)] -> Msg.Msg -> m Cmd.Status
keymaps inputs = inst_keymaps [(c, n, k, Nothing) | (c, n, k) <- inputs]

-- | Create a note entry Cmd for a keymapped instrument.
--
-- Like NoteEntry, if kbd entry is on but ValEdit is not, then play the sound
-- but don't enter the note.
inst_keymaps :: (Cmd.M m) => [(Char, String, Midi.Key, Maybe Score.Instrument)]
    -- ^ (kbd key mapped to note, text to insert, midi key to emit for thru,
    -- emit thru on this instrument or current inst if Nothing)
    -> Msg.Msg -> m Cmd.Status
inst_keymaps inputs = \msg -> do
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
        -- Only swallow keys that note entry would have caught, otherwise
        -- space would be swallowed here.
        Nothing
            | Map.member char NoteEntry.note_map -> return Cmd.Done
            | otherwise -> return Cmd.Continue
        Just (note, key, maybe_inst) -> do
            case kstate of
                UiMsg.KeyRepeat -> return ()
                UiMsg.KeyDown -> keymap_down maybe_inst note key
                UiMsg.KeyUp -> keymap_up maybe_inst key
            return Cmd.Done
    where
    to_note = Map.fromList
        [(char, (note, key, inst)) | (char, note, key, inst) <- inputs]

keymap_down :: (Cmd.M m) => Maybe Score.Instrument -> String -> Midi.Key -> m ()
keymap_down maybe_inst note key = do
    whenM Cmd.is_val_edit $ suppressed $ do
        pos <- Selection.get_insert_pos
        NoteTrack.modify_event_at pos False True $ const (Just note, True)
    MidiThru.channel_messages maybe_inst True [Midi.NoteOn key 64]
    where suppressed = Cmd.suppress_history Cmd.ValEdit ("keymap: " ++ note)

keymap_up :: (Cmd.M m) => Maybe Score.Instrument -> Midi.Key -> m ()
keymap_up maybe_inst key =
    MidiThru.channel_messages maybe_inst True [Midi.NoteOff key 64]

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
    MidiThru.channel_messages Nothing False
        [Midi.NoteOn key 64, Midi.NoteOff key 64]
    Cmd.set_note_text note
    return Cmd.Done
    where
    to_note = Map.fromList [(char, (note, key)) | (char, note, key) <- inputs]


-- * drums

drum_code :: [(Drums.Note, Midi.Key)] -> MidiInst.Code
drum_code note_keys =
    MidiInst.note_calls (drum_calls (map fst note_keys))
    <> MidiInst.cmd (drum_cmd note_keys)

inst_drum_code :: [(Drums.Note, Midi.Key, Score.Instrument)] -> MidiInst.Code
inst_drum_code note_keys =
    MidiInst.note_calls (drum_calls [note | (note, _, _) <- note_keys])
    <> MidiInst.cmd (inst_drum_cmd note_keys)

drum_instrument :: [(Drums.Note, Midi.Key)] -> Instrument.Patch
    -> Instrument.Patch
drum_instrument note_keys = Instrument.triggered
    . Instrument.set_attribute_map
        [(Drums.note_attrs note, Drums.note_name note) | (note, _) <- note_keys]
    . Instrument.set_keymap
        [(Drums.note_attrs note, key) | (note, key) <- note_keys]

-- | Create a LookupCall for the given Notes.
drum_calls :: [Drums.Note] -> [(String, Derive.NoteCall)]
drum_calls notes =
    [(Drums.note_name n, note_call (Drums.note_dynamic n) (Drums.note_attrs n))
        | n <- notes]
    where
    note_call dyn attrs = Note.note_call
        ("drum call: " <> ShowVal.show_val attrs)
        (with_dyn dyn . Call.Util.add_attrs attrs . Note.default_note False)
    with_dyn = Derive.multiply_control Score.c_dynamic

-- | Create keymap Cmd for the given Notes.  This should be paired with
-- 'drum_calls' so the Cmd will create calls that the deriver understands.
drum_cmd :: (Cmd.M m) => [(Drums.Note, Midi.Key)] -> Msg.Msg -> m Cmd.Status
drum_cmd note_keys = keymaps
    [(Keymap.physical_key (Drums.note_char n), Drums.note_name n, key)
        | (n, key) <- note_keys]

inst_drum_cmd :: (Cmd.M m) => [(Drums.Note, Midi.Key, Score.Instrument)]
    -> Msg.Msg -> m Cmd.Status
inst_drum_cmd note_keys = inst_keymaps
    [(Keymap.physical_key (Drums.note_char n), Drums.note_name n, key,
            Just inst)
        | (n, key, inst) <- note_keys]
