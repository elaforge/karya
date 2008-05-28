{- The default keyboard layout.

For kbd entry, you only get midi thru when you're in insert mode, which means
you can't try out a key without entering a note.  It's probably useful to
do thru with insert off, but then midi thru cmds take up most of the kbd even
in cmd mode.  Possible solutions:

- Do kbd midi thru anyway, since it doesn't particularly hurt to play a note
when running some cmd.

- Leave the kbd for for notes, and use control or command for all kbd commands.
This isn't too appealing because kbd note entry may be turned off most of the
time (when a midi kbd is available).

TODO I should also be careful how I use the modifiers.  If I must use both
control and command, I should be consistent about their meaning.

-}
module Cmd.DefaultKeymap where
import qualified Data.Map as Map
import Control.Monad
import qualified Control.Monad.Identity as Identity

import qualified App.Config as Config

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import Cmd.Types
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Keymap as Keymap

import qualified Cmd.Selection as Selection
import qualified Cmd.Edit as Edit
import qualified Cmd.Save as Save
import qualified Cmd.Play as Play

import qualified Midi.Midi as Midi -- for default_addr hack

import qualified Perform.Transport as Transport

import qualified Util.Log as Log


default_cmds :: Cmd.State -> [Cmd.Cmd]
default_cmds state =
    Edit.cmd_midi_thru
    : (if Cmd.state_edit_mode state
        then [cmd_midi_entry, cmd_kbd_note_entry] else [])
    ++ [ Selection.cmd_mouse_selection 1 Config.insert_selnum
    , Keymap.make_cmd (misc_bindings ++ selection_bindings ++ edit_bindings)
    ]

cmd_io_keymap :: Transport.Info -> Cmd.CmdIO
cmd_io_keymap player_info = Keymap.make_cmd (io_bindings player_info)

io_bindings :: Transport.Info -> [Keymap.Binding IO]
io_bindings player_info =
    [ kbd_kmod_cmd [Key.MetaL] (Key.KeyChar 's') "save" cmd_save
    , kbd_kmod_cmd [Key.MetaL] (Key.KeyChar 'l') "load" cmd_load
    , kbd_cmd Key.Enter "play block" (Play.cmd_play_block player_info)
    , kbd_cmd (Key.KeyChar ' ') "stop play" Play.cmd_stop
    ]

cmd_save, cmd_load :: Cmd.CmdT IO Cmd.Status
cmd_save = Save.cmd_save Nothing >> return Cmd.Done
cmd_load = Save.cmd_load Nothing >> return Cmd.Done

cmd_kbd_note_entry :: Cmd.Cmd
cmd_kbd_note_entry = Keymap.make_cmd kbd_note_entry
cmd_midi_entry msg = Edit.cmd_insert_midi_note msg >> cmd_advance_insert

misc_bindings =
    [ kbd_cmd (Key.KeyChar '=') "quit" Cmd.cmd_quit
    ]

selection_bindings =
    [ kbd_cmd Key.Down "advance selection" $
        Selection.cmd_step_selection Config.insert_selnum Advance
    , kbd_cmd Key.Up "rewind selection" $
        Selection.cmd_step_selection Config.insert_selnum Rewind
    , kbd_cmd Key.Right "shift selection right" $
        Selection.cmd_shift_selection Config.insert_selnum 1
    , kbd_cmd Key.Left "shift selection left" $
        Selection.cmd_shift_selection Config.insert_selnum (-1)
    ]

cmd_advance_insert = Selection.cmd_step_selection Config.insert_selnum Advance

edit_bindings =
    [ kbd_cmd Key.Escape "toggle edit mode" Edit.cmd_toggle_edit
    , kbd_cmd Key.Backspace "erase note"
        (Edit.cmd_remove_events >> cmd_advance_insert)

    , kbd_cmd (Key.KeyChar '1') "step rank 1" (Edit.cmd_meter_step 1)
    , kbd_cmd (Key.KeyChar '2') "step rank 2" (Edit.cmd_meter_step 2)
    , kbd_cmd (Key.KeyChar '3') "step rank 3" (Edit.cmd_meter_step 3)
    , kbd_cmd (Key.KeyChar '4') "step rank 4" (Edit.cmd_meter_step 4)
    ]

-- ** kbd note entry

-- | Enter notes from the computer keyboard.
kbd_note_entry :: [Keymap.Binding Identity.Identity]
kbd_note_entry = concatMap make_kbd_note_entry (lower_notes ++ upper_notes)

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

-- TODO hardcoded addr for kbd thru.  Later this should come from the track
-- instrument.
default_addr = (Midi.WriteDevice "dummy", 0)

make_kbd_note_entry :: (Edit.NoteNumber, Char)
    -> [Keymap.Binding Identity.Identity]
make_kbd_note_entry (pitch_num, unmapped_char) =
    [ (spec UiMsg.KeyDown, cmdm (desc ++ " down") keydown_cmd)
    , (spec UiMsg.KeyUp, cmdm (desc ++ " up") keyup_cmd)
    ]
    where
    key = Key.KeyChar (hardcoded_kbd_layout Map.! unmapped_char)
    spec state = Keymap.KeySpec Keymap.AnyCharMod (Keymap.UiKey state key)
    desc = "note pitch " ++ show pitch_num
    keydown_cmd = do
        mods <- Cmd.keys_down
        -- Key repeat makes multiple note ons.  Ignore it for note entry too,
        -- since I can't see wanting to spam out a million notes at once.
        when (Cmd.KeyMod key `Map.notMember` mods) $ do
            Edit.cmd_insert_pitch pitch_num
            Edit.cmd_note_on default_addr pitch_num
            cmd_advance_insert
            return ()
        return Cmd.Done
    keyup_cmd = Edit.cmd_note_off default_addr pitch_num

-- | Simple cmd with keyboard key and no modifiers.
kbd_cmd :: Key.Key -> String -> Cmd.CmdT m Cmd.Status -> Keymap.Binding m
kbd_cmd = kbd_mod_cmd []

kbd_kmod_cmd key_mods = kbd_mod_cmd (map Cmd.KeyMod key_mods)

kbd_mod_cmd :: [Cmd.Modifier] -> Key.Key -> String -> Cmd.CmdT m Cmd.Status
    -> Keymap.Binding m
kbd_mod_cmd mods key desc cmd =
    ( Keymap.KeySpec (Keymap.Mods mods) (Keymap.UiKey UiMsg.KeyDown key)
    , cmdm desc cmd)

cmdm :: String -> Cmd.CmdT m Cmd.Status -> Keymap.CmdSpec m
cmdm name cmd = Keymap.CmdSpec name (ignore_msg cmd)

-- ** util

-- | Cmds that don't actually need the msg can use this.
ignore_msg = const


-- keymaps
-- This way I can set up the mapping relative to qwerty, but still have it come
-- out right on dvorak.

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
-- TODO presumably this should eventually be easier to change
hardcoded_kbd_layout = qwerty_to_dvorak
