-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{- | This module implements kbd entry by intercepting kbd and MIDI events and
    re-emitting them as InputNotes.  These then go to the track-specific edit
    cmds to enter notes and to "Cmd.MidiThru" which re-emits them as MIDI.
-}
module Cmd.NoteEntry where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg

import qualified Derive.Controls as Controls
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch


-- * with_note

-- | Take a Key (if @kbd_entry@ is True) or a ReadMessage to a Msg.InputNote
-- and pass it to each of @cmds@.  As a minor optimization, @cmd@ is not called
-- if no InputNote was produced.
--
-- For the ASCII keyboard, the two rows of keys each map one octave of C to C.
-- If the instrument responds to breath control, the key will also emit
-- a breath control CC.
--
-- It's a little less graceful than calling it many times applied to a single
-- cmd, but only has to convert the input once and doesn't need tricks to
-- make sure a converted key winds up with Done.
--
-- Another way to do this would be place this as a transformer at the front of
-- the responder, to transform keystrokes and MIDI keys into InputNotes.  That
-- way, other Cmds don't have to worry about state_kbd_entry.  However, it
-- would either require a privileged position for the transformer, or an
-- additional Cmd feature to re-emits a new Msg.  In addition, it would
-- preclude the ability to shadow it and catch MIDI msgs for other purposes.
--
-- TODO it might be nicer to do the scale mapping here.  It would mean one
-- extra mapping here and one less mapping in MidiThru.  The scale lookup would
-- become a little messier since I'd need to lookup input to mapped input and
-- then mapped input to note.  PitchTrack would still need the scale.  I could
-- reduce the scope of InputKey or eliminate it entirely for the more universal
-- NoteNumber.  It seems like a wash at the moment.
cmds_with_note :: KbdMap -> Bool -> Maybe Instrument.Patch -> [Cmd.Cmd]
    -> Cmd.Cmd
cmds_with_note kbd_map kbd_entry maybe_patch cmds msg = do
    has_mods <- are_modifiers_down
    new_msgs <- if kbd_entry && not has_mods
        then do
            octave <- Cmd.gets (Cmd.state_kbd_entry_octave . Cmd.state_edit)
            let is_pressure = maybe False
                    (Instrument.has_flag Instrument.Pressure) maybe_patch
            return $ kbd_input kbd_map is_pressure octave msg
        else return Nothing
    new_msgs <- maybe (midi_input msg) (return . Just) new_msgs
    case new_msgs of
        Nothing -> Cmd.sequence_cmds cmds msg
        Just msgs -> foldr Cmd.merge_status Cmd.Done <$> mapM send msgs
    where
    send msg = do
        case msg of
            Msg.InputNote (InputNote.NoteOn note_id _ _) ->
                Cmd.modify_wdev_state $ \wdev -> wdev
                    { Cmd.wdev_last_note_id = Just note_id }
            _ -> return ()
        Cmd.sequence_cmds cmds msg

are_modifiers_down :: (Cmd.M m) => m Bool
are_modifiers_down = fmap (not . Set.null) Keymap.mods_down

-- ** kbd

-- | Convert a keyboard key-down to a 'Msg.InputNote'.
--
-- @Nothing@ means there's no input, @Just []@ means there was input, but
-- nothing to do.
kbd_input :: KbdMap -> Bool -- ^ Whether this is a Pressure instrument or not.
    -- Pressure instruments respond to breath, and a kbd entry note on will
    -- emit an extra breath control.  This is convenient in practice because
    -- kbd entry is for quick and easy input and breath control gets in the way
    -- of that.
    -> Pitch.Octave -> Msg.Msg -> Maybe [Msg.Msg]
kbd_input kbd_map is_pressure octave (Msg.key -> Just (down, key)) =
    case down of
        UiMsg.KeyRepeat
            -- Just [] makes the repeats get eaten here, but make sure to only
            -- suppress them if this key would have generated a note.
            | Maybe.isJust msg -> Just []
            | otherwise -> Nothing
        _ -> msg
    where
    msg = (fmap . fmap) Msg.InputNote $
        key_to_input kbd_map is_pressure octave (down == UiMsg.KeyDown) key
kbd_input _ _ _ _ = Nothing

type NoteMap = Map.Map Char (Pitch.Octave, Pitch.InputKey)

data KbdMap = KbdMap {
    kbd_note_map :: !NoteMap
    , kbd_per_octave :: !Pitch.InputKey
    } deriving (Show)

twelve_kbd_map :: KbdMap
twelve_kbd_map = KbdMap
    (make_note_map (physical "q2w3er5t6y7ui") (physical "zsxdcvgbhnjm,")) 12

key_to_input :: KbdMap -> Bool -> Pitch.Octave -> Bool
    -> Key.Key -> Maybe [InputNote.Input]
key_to_input (KbdMap note_map per_octave) is_pressure octave is_down
        (Key.Char c) =
    case Map.lookup c note_map of
        Nothing
            | c `Set.member` kbd_input_keys -> Just []
            | otherwise -> Nothing
        Just (octave_offset, input) -> Just $ inputs_of octave_offset input
    where
    inputs_of octave_offset input =
        case InputNote.from_key 0 is_down adjusted of
            input@(InputNote.NoteOn note_id _ _) | is_pressure ->
                -- Breath goes second, otherwise thru won't think it belongs to
                -- this note.
                [input, breath note_id (100/127)]
            input -> [input]
        where
        adjusted = input + fromIntegral (octave + octave_offset) * per_octave
    breath note_id val = InputNote.Control note_id Controls.breath val
key_to_input _ _ _ _ _ = Nothing

kbd_input_keys :: Set.Set Char
kbd_input_keys = Set.fromList $ physical $ concat
    [ "1234567890"
    , "qwertyuiop"
    , "asdfghjkl;"
    , "zxcvbnm,./"
    ]

make_note_map :: [Char] -> [Char] -> NoteMap
make_note_map top_row bottom_row =
    Map.fromList $ mkrow 1 top_row ++ mkrow 0 bottom_row
    where mkrow oct row = zip row (map ((,) oct) (Seq.range_ 0 1))

-- | TODO this hardcodes *bp scale support into NoteEntry.  The key layout
-- should really be expressed in the scale itself, but I'm not really sure what
-- the interface should look like, since I have to reconcile the scale's
-- desires with the keyboard's limitations..  Since *bp is the only scale with
-- a nonstandard layout so far I'll just hardcode for now.  If there is ever
-- a third layout I'll have a better idea of what the possibilities.
bp_kbd_map :: KbdMap
bp_kbd_map = KbdMap (make_note_map (physical bp1) (physical bp2)) 13

bp1, bp2 :: [Char]
bp1 ="q2w e r5t y7u i9o"
bp2 ="zsx c vgb njm ,l."
-- > │ █ │ │ █ │ █ │ █ │ │
-- > │ █ │ │ █ │ █ │ █ │ │
-- > │ │ │ │ │ │ │ │ │ │ │
-- > └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
-- >  1 2 3 4 5 6 7 8 9 A
-- >  A B C D E F G H I A

-- | This should happen at compile time, even though it doesn't.
physical :: [Char] -> [Char]
physical = map Keymap.physical_key . filter (/=' ')

-- ** midi

-- | Convert a 'Msg.Midi' msg into a 'Msg.InputNote'.
midi_input :: (Cmd.M m) => Msg.Msg -> m (Maybe [Msg.Msg])
midi_input (Msg.Midi (Midi.ReadMessage rdev _ midi_msg)) = do
    rstate <- Cmd.get_rdev_state rdev
    case InputNote.from_midi rstate rdev midi_msg of
        Just (input, rstate2) -> do
            Cmd.set_rdev_state rdev rstate2
            return $ Just [Msg.InputNote input]
        Nothing -> return (Just [])
midi_input _ = return Nothing

-- * edit_append

edit_append :: (Cmd.M m) => Msg.Msg -> m Cmd.Status
edit_append msg = do
    edit_input <- Cmd.gets $ Cmd.state_edit_input . Cmd.state_edit
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) | edit_input -> do
            note <- EditUtil.parse_key key
            return $ Cmd.EditInput $ Cmd.EditAppend $
                " (" <> Pitch.note_text note <> ")"
        _ -> return Cmd.Continue
