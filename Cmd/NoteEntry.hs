{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Cmds to add notes to an instrument track and send midi thru.  Uses
Cmd.Edit to do its dirty work.
-}
module Cmd.NoteEntry where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import qualified Midi.Midi as Midi -- for default_addr hack

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Selection as Selection
import qualified Derive.Twelve as Twelve


cmd_kbd_note_entry :: Cmd.Cmd
cmd_kbd_note_entry = Keymap.make_cmd kbd_note_entry
cmd_kbd_note_thru = Keymap.make_cmd kbd_note_thru
cmd_midi_entry msg =
    Edit.cmd_insert_midi_note msg >> Selection.cmd_advance_insert

-- ** kbd note entry

-- | Enter notes from the computer keyboard.
kbd_note_entry :: [Keymap.Binding Identity.Identity]
kbd_note_entry =
    concatMap (make_kbd_note_entry True) (lower_notes ++ upper_notes)

kbd_note_thru :: [Keymap.Binding Identity.Identity]
kbd_note_thru =
    concatMap (make_kbd_note_entry False) (lower_notes ++ upper_notes)

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

make_kbd_note_entry :: Bool -> (Edit.NoteNumber, Char)
    -> [Keymap.Binding Identity.Identity]
make_kbd_note_entry enter_notes (pitch_num, unmapped_char) =
    [ (spec UiMsg.KeyDown, Keymap.cspec_ (desc ++ " down") keydown_cmd)
    , (spec UiMsg.KeyUp, Keymap.cspec_ (desc ++ " up") keyup_cmd)
    ]
    where
    key = Key.KeyChar (Keymap.hardcoded_kbd_layout Map.! unmapped_char)
    spec state = Keymap.KeySpec Keymap.AnyCharMod (Keymap.UiKey state key)
    desc = "note pitch " ++ show pitch_num
    keydown_cmd = do
        mods <- Cmd.keys_down
        -- Key repeat makes multiple note ons.  Ignore it for note entry too,
        -- since I can't see wanting to spam out a million notes at once.
        when (Cmd.KeyMod key `Map.notMember` mods) $ do
            when enter_notes $ do
                Edit.cmd_insert_pitch pitch_num
                Selection.cmd_advance_insert
                return ()
            event_text <-
                fmap Twelve.pitch_event (Edit.pitch_from_kbd pitch_num)
            Cmd.set_status "note" (Just event_text)
            Edit.cmd_note_on default_addr pitch_num
            return ()
        return Cmd.Done
    keyup_cmd = Edit.cmd_note_off default_addr pitch_num
