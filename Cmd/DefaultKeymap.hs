module Cmd.DefaultKeymap where
import qualified Data.Map as Map

import qualified Ui.Key as Key

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Selection as Selection
import qualified Cmd.Edit as Edit

import qualified App.Config as Config


default_cmds :: Cmd.State -> [Cmd.Cmd]
default_cmds state =
    [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc ++ selection ++ edit ++ add_note_entry)
    ]
    where
    add_note_entry = if Cmd.state_edit_mode state then note_entry else []

misc =
    [ (Keymap.KeySpec [Cmd.KeyMod Key.ControlL] (Keymap.UiKey Key.Escape),
        Keymap.CmdSpec "quit" (ignore_msg Cmd.cmd_quit))
    , single (Key.KeyChar '=') "quit" Cmd.cmd_quit
    ]

selection =
    [ single Key.Down "advance selection" $
        Selection.cmd_step_selection Config.insert_selnum Selection.Advance
    , single Key.Up "rewind selection" $
        Selection.cmd_step_selection Config.insert_selnum Selection.Rewind
    , single Key.Right "shift selection right" $
        Selection.cmd_shift_selection Config.insert_selnum 1
    , single Key.Left "shift selection left" $
        Selection.cmd_shift_selection Config.insert_selnum (-1)
    ]

advance_insert =
    Selection.cmd_step_selection Config.insert_selnum Selection.Advance

edit =
    [ single Key.Escape "toggle edit mode" Edit.cmd_toggle_edit
    , single Key.Backspace "erase note"
        (Edit.cmd_remove_events >> advance_insert)
    ]

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

note_entry = map make_note_entry (lower_notes ++ upper_notes)

make_note_entry (pitch, char) =
    (Keymap.KeySpec [] (Keymap.UiKey (Key.KeyChar (keymap Map.! char))),
        Keymap.CmdSpec ("note with pitch " ++ show pitch)
            (ignore_msg insert_cmd))
    where
    insert_cmd = Edit.cmd_insert_pitch (Edit.PitchClass pitch) >> advance_insert

-- TODO: should take [[Modifier]] and produce a mapping for each
-- wait and see what's actually useful
spec = Keymap.KeySpec
single key desc cmd = (spec [] (Keymap.UiKey key), cmdm desc cmd)
cmdm :: String -> Cmd.CmdM -> Keymap.CmdSpec
cmdm name cmd = Keymap.CmdSpec name (ignore_msg cmd)


-- ** util

-- | Cmds that don't actually need the msg can use this.
ignore_msg :: Cmd.CmdM -> Cmd.Cmd
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
