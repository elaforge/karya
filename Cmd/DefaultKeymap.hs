module Cmd.DefaultKeymap where
import qualified Data.Map as Map

import qualified Ui.Key as Key

import Cmd.Types
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Selection as Selection
import qualified Cmd.Edit as Edit
import qualified Cmd.Save as Save

import qualified App.Config as Config


default_cmds :: Cmd.State -> [Cmd.Cmd]
default_cmds state =
    Edit.cmd_midi_thru
    : (if Cmd.state_edit_mode state
        then [cmd_midi_entry, cmd_note_entry] else [])
    ++ [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc ++ selection ++ edit)
    ]

cmd_io_keymap :: Msg.Msg -> Cmd.CmdT IO Cmd.Status
cmd_io_keymap = Keymap.make_cmd io_keys

io_keys :: [(Keymap.KeySpec, Keymap.CmdSpec IO)]
io_keys =
    [ (Keymap.KeySpec [Cmd.KeyMod Key.MetaL] (Keymap.UiKey (Key.KeyChar 's'))
        , Keymap.CmdSpec "save" save)
    , (Keymap.KeySpec [Cmd.KeyMod Key.MetaL] (Keymap.UiKey (Key.KeyChar 'l'))
        , Keymap.CmdSpec "load" load)
    ]

save _msg = Save.cmd_save Nothing >> return Cmd.Done
load _msg = Save.cmd_load Nothing >> return Cmd.Done

cmd_note_entry :: Cmd.Cmd
cmd_note_entry = Keymap.make_cmd note_entry
cmd_midi_entry msg = Edit.cmd_insert_midi_note msg >> advance_insert

misc =
    [ (Keymap.KeySpec [Cmd.KeyMod Key.MetaL] (Keymap.UiKey Key.Escape),
        Keymap.CmdSpec "quit" (ignore_msg Cmd.cmd_quit))
    , single (Key.KeyChar '=') "quit" Cmd.cmd_quit
    ]

selection =
    [ single Key.Down "advance selection" $
        Selection.cmd_step_selection Config.insert_selnum Advance
    , single Key.Up "rewind selection" $
        Selection.cmd_step_selection Config.insert_selnum Rewind
    , single Key.Right "shift selection right" $
        Selection.cmd_shift_selection Config.insert_selnum 1
    , single Key.Left "shift selection left" $
        Selection.cmd_shift_selection Config.insert_selnum (-1)
    ]

advance_insert =
    Selection.cmd_step_selection Config.insert_selnum Advance

edit =
    [ single Key.Escape "toggle edit mode" Edit.cmd_toggle_edit
    , single Key.Backspace "erase note"
        (Edit.cmd_remove_events >> advance_insert)

    , single (Key.KeyChar '1') "step rank 1" (Edit.cmd_meter_step 1)
    , single (Key.KeyChar '2') "step rank 2" (Edit.cmd_meter_step 2)
    , single (Key.KeyChar '3') "step rank 3" (Edit.cmd_meter_step 3)
    , single (Key.KeyChar '4') "step rank 4" (Edit.cmd_meter_step 4)
    ]

-- ** note entry

note_entry = map make_note_entry (lower_notes ++ upper_notes)
-- midi_note_entry = map make_midi_note_entry [0..127]

lower_notes = zip [0..]
    [ 'z', 's' -- C
    , 'x', 'd' -- D
    , 'c'
    , 'v', 'g' -- F
    , 'b', 'h'
    , 'n', 'j' -- A
    , 'm'
    , ',' -- C
    ]

upper_notes = zip [12..]
    [ 'q', '2' -- C
    , 'w', '3'
    , 'e'
    , 'r', '5' -- F
    , 't', '6'
    , 'y', '7' -- A
    , 'u'
    , 'i' -- C
    ]

make_note_entry (pitch, char) =
    (Keymap.KeySpec [] (Keymap.UiKey (Key.KeyChar (keymap Map.! char))),
        Keymap.CmdSpec ("note with pitch " ++ show pitch)
            (ignore_msg insert_cmd))
    where
    insert_cmd = Edit.cmd_insert_pitch (Edit.PitchClass pitch) >> advance_insert

-- make_midi_note_entry nn =
--     (Keymap.KeySpec [] (Keymap.MidiKey (Keymap.NoteOn nn)),
--         Keymap.CmdSpec ("midi note with pitch " ++ show nn)
--             (\msg -> Edit.cmd_insert_midi_note msg >> advance_insert))

-- TODO: should take [[Modifier]] and produce a mapping for each
-- wait and see what's actually useful
spec = Keymap.KeySpec
single key desc cmd = (spec [] (Keymap.UiKey key), cmdm desc cmd)
cmdm :: String -> Cmd.CmdT m Cmd.Status -> Keymap.CmdSpec m
cmdm name cmd = Keymap.CmdSpec name (ignore_msg cmd)


-- ** util

-- | Cmds that don't actually need the msg can use this.
ignore_msg = const


-- keymaps

qwerty = "1234567890-="
    ++ "qwertyuiop[]\\"
    ++ "asdfghjkl;'"
    ++ "zxcvbnm,./"

-- OS X has this weird thing going on where
dvorak = "1234567890-="
    ++ "',.pyfgcrl[]\\"
    ++ "aoeuidhtns/"
    ++ ";qjkxbmwvz"

qwerty_to_dvorak = Map.fromList (zip qwerty dvorak)
keymap = qwerty_to_dvorak
