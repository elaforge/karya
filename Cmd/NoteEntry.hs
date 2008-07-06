{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Cmds to add notes to an instrument track and send midi thru.  Uses
Cmd.Edit to do its dirty work.
-}
module Cmd.NoteEntry where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Selection as Selection
import qualified Derive.Twelve as Twelve

import qualified Perform.Midi.Instrument as Instrument


-- | Note that 'cmd_kbd_note_entry' does a midi thru on an entry.  This is
-- unlike 'cmd_midi_entry', which doesn't.  Midi thru is handled separately by
-- 'Edit.cmd_midi_thru' because midi notes always go through no matter what
-- else they may do.
cmd_kbd_note_entry :: Instrument.Addr -> Cmd.Cmd
cmd_kbd_note_entry addr = Keymap.make_cmd (kbd_note_entry addr)

cmd_kbd_note_thru :: Instrument.Addr -> Cmd.Cmd
cmd_kbd_note_thru addr = Keymap.make_cmd (kbd_note_thru addr)

cmd_midi_entry :: Cmd.Cmd
cmd_midi_entry msg =
    Edit.cmd_insert_midi_note msg >> Selection.cmd_advance_insert

-- ** kbd note entry

-- | Enter notes from the computer keyboard.
kbd_note_entry :: Instrument.Addr -> [Keymap.Binding Identity.Identity]
kbd_note_entry addr =
    concatMap (make_kbd_note_entry True addr) (lower_notes ++ upper_notes)

kbd_note_thru :: Instrument.Addr -> [Keymap.Binding Identity.Identity]
kbd_note_thru addr =
    concatMap (make_kbd_note_entry False addr) (lower_notes ++ upper_notes)

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

make_kbd_note_entry :: Bool -> Instrument.Addr -> (Edit.NoteNumber, Char)
    -> [Keymap.Binding Identity.Identity]
make_kbd_note_entry enter_notes addr (pitch_num, unmapped_char) =
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
            Edit.cmd_note_on addr pitch_num
            return ()
        return Cmd.Done
    keyup_cmd = Edit.cmd_note_off addr pitch_num
