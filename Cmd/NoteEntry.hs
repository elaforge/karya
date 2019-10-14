-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
{- | This module implements kbd entry by intercepting kbd and MIDI events and
    re-emitting them as InputNotes.  These then go to the track-specific edit
    cmds to enter notes and to "Cmd.MidiThru" which re-emits them as MIDI.
-}
module Cmd.NoteEntry where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg

import qualified Derive.Controls as Controls
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg

import           Global


-- * with_note

{- | Take a Key (if @kbd_entry@ is True) or a ReadMessage to a Msg.InputNote
    and pass it to each of @cmds@.  As a minor optimization, @cmd@ is not
    called if no InputNote was produced.

    For the ASCII keyboard, the two rows of keys each map one octave of C to C.
    If the instrument responds to breath control, the key will also emit
    a breath control CC.

    It's a little less graceful than calling it many times applied to a single
    cmd, but only has to convert the input once and doesn't need tricks to make
    sure a converted key winds up with Done.

    Another way to do this would be place this as a transformer at the front of
    the responder, to transform keystrokes and MIDI keys into InputNotes.  That
    way, other Cmds don't have to worry about state_kbd_entry.  However, it
    would either require a privileged position for the transformer, or an
    additional Cmd feature to re-emit a new Msg.  In addition, it would
    preclude the ability to shadow it and catch MIDI msgs for other purposes.
-}
cmds_with_input :: Cmd.M m => Bool -> Maybe Patch.Config
    -> [Msg.Msg -> m Cmd.Status] -> (Msg.Msg -> m Cmd.Status)
cmds_with_input kbd_entry maybe_config cmds msg =
    msg_to_inputs kbd_entry maybe_config msg >>= \case
        Nothing -> Cmd.sequence_cmds cmds msg
        Just inputs -> foldr Cmd.merge_status Cmd.Done <$> mapM send inputs
    where
    send input = do
        case input of
            InputNote.NoteOn note_id _ _ ->
                Cmd.modify_wdev_state $ \wdev -> wdev
                    { Cmd.wdev_last_note_id = Just note_id }
            _ -> return ()
        Cmd.sequence_cmds cmds (Msg.InputNote input)

-- | Like 'cmds_with_input', but figure out kbd_entry and patch on my own.
run_cmds_with_input :: Cmd.M m => [Msg.Msg -> m Cmd.Status]
    -> (Msg.Msg -> m Cmd.Status)
run_cmds_with_input cmds msg = do
    kbd_entry <- Cmd.gets $ Cmd.state_kbd_entry . Cmd.state_edit
    maybe_config <- justm EditUtil.lookup_instrument $ \inst ->
        justm (Cmd.lookup_instrument inst) $
        return . fmap snd . Cmd.midi_instrument
    cmds_with_input kbd_entry maybe_config cmds msg

-- | Convert a Msg to 'Msg.InputNote's, if applicable.  Returns Nothing if
-- the Msg is not convertible to InputNotes (and therefore other cmds should
-- get it), and Just [] if it is but didn't emit any InputNotes (and therefore
-- this other cmds shouldn't get it).
msg_to_inputs :: Cmd.M m => Bool -> Maybe Patch.Config -> Msg.Msg
    -> m (Maybe [InputNote.Input])
msg_to_inputs kbd_entry maybe_config msg = do
    has_mods <- are_modifiers_down
    new_msgs <- if kbd_entry && not has_mods
        then do
            octave <- Cmd.gets (Cmd.state_kbd_entry_octave . Cmd.state_edit)
            let is_pressure = maybe False
                    (`Patch.has_flag` Patch.Pressure) maybe_config
            return $ kbd_input is_pressure octave msg
        else return Nothing
    maybe (midi_input msg) (return . Just) new_msgs

are_modifiers_down :: Cmd.M m => m Bool
are_modifiers_down = fmap (not . Set.null) Keymap.mods_down

-- ** kbd

-- | Convert a keyboard key-down to a 'Msg.InputNote'.
--
-- @Nothing@ means there's no input, @Just []@ means there was input, but
-- nothing to do.
kbd_input :: Bool -- ^ Whether this is a Pressure instrument or not.
    -- Pressure instruments respond to breath, and a kbd entry note on will
    -- emit an extra breath control.  This is convenient in practice because
    -- kbd entry is for quick and easy input and breath control gets in the way
    -- of that.
    -> Pitch.Octave -> Msg.Msg -> Maybe [InputNote.Input]
kbd_input is_pressure octave (Msg.key -> Just (down, key)) = case down of
    UiMsg.KeyRepeat
        -- Just [] makes the repeats get eaten here, but make sure to only
        -- suppress them if this key would have generated a note.
        | Maybe.isJust mb_inputs -> Just []
        | otherwise -> Nothing
    _ -> mb_inputs
    where
    mb_inputs = -- (fmap . fmap) Msg.InputNote $
        key_to_input is_pressure octave (down == UiMsg.KeyDown) key
kbd_input _ _ _ = Nothing

key_to_input :: Bool -> Pitch.Octave -> Bool -> Key.Key
    -> Maybe [InputNote.Input]
key_to_input is_pressure octave is_down (Key.Char c) = do
    pitch <- Map.lookup c kbd_map
    return $ inputs_of (Pitch.add_octave octave pitch)
    where
    inputs_of pitch = case InputNote.from_ascii is_down pitch of
        input@(InputNote.NoteOn note_id _ _) | is_pressure ->
            -- Breath goes second, otherwise thru won't think it belongs to
            -- this note.
            [input, breath note_id InputNote.keyboard_velocity]
        input -> [input]
    breath note_id val = InputNote.Control note_id Controls.breath val
key_to_input _ _ _ _ = Nothing

kbd_map :: Map Char Pitch.Pitch
kbd_map = Map.fromList $ concat
    -- I leave '-' free since it's mapped to change octave.
    [ [('1', Pitch.Pitch 1 (Pitch.Degree 0 (-1)))]
    , keys 1 "234567890" 1
    , keys 1 "qwertyuiop" 0
    -- 'a' is also the append cmd.
    -- This omits symbol characters so they can retain their edit bindings.
    , keys 0 "sdfghjkl;" 1
    , keys 0 "zxcvbnm,." 0
    ]
    where
    keys oct letters accs =
        [ (c, Pitch.Pitch oct $ Pitch.Degree pc accs)
        -- The mapping should happen at compile time, even though it doesn't.
        | (pc, c) <- zip [0..] (map Keymap.physical_key letters)
        ]

-- ** midi

-- | Convert a 'Msg.Midi' msg.
midi_input :: Cmd.M m => Msg.Msg -> m (Maybe [InputNote.Input])
midi_input (Msg.Midi (Midi.ReadMessage rdev _ midi_msg)) = do
    rstate <- Cmd.gets Cmd.state_rdev_state
    case InputNote.from_midi rstate rdev midi_msg of
        Just (input, rstate2) -> do
            Cmd.modify $ \st -> st { Cmd.state_rdev_state = rstate2 }
            return $ Just [input]
        Nothing -> return (Just [])
midi_input _ = return Nothing

-- * floating_input_insert

floating_input_insert :: Cmd.M m => Msg.Msg -> m Cmd.Status
floating_input_insert msg = do
    floating_input <- Cmd.gets $ Cmd.state_floating_input . Cmd.state_edit
    case msg of
        Msg.InputNote (InputNote.NoteOn _ input _) | floating_input -> do
            note <- EditUtil.input_to_note input
            return $ Cmd.FloatingInput $ Cmd.FloatingInsert $
                " (" <> Pitch.note_text note <> ")"
        _ -> return Cmd.Continue
