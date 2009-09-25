{-# LANGUAGE ViewPatterns #-}
{- | This module implements kbd entry by intercepting kbd and MIDI events and
    re-emitting them as InputNotes.  These then go to the track-specific edit
    cmds to enter notes and to "Cmd.MidiThru" which re-emits them as MIDI.
-}
module Cmd.NoteEntry where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Midi.Midi as Midi
import qualified Ui.Key as Key

import qualified Cmd.Keymap as Keymap
import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg

import qualified Perform.Pitch as Pitch


-- * with_note

-- | Take a Key (if @kbd_entry@ is True) or a ReadMessage to a Msg.InputNote
-- and pass it to each of @cmds@.  As a minor optimization, @cmd@ is not called
-- if no InputNote was produced.
--
-- It's a little less graceful than calling it many times applied to a single
-- cmd, but only has to convert the input once and doesn't need tricks to
-- make sure a converted key winds up with Done.
--
-- TODO it might be nicer to do the scale mapping here.  It would mean one
-- extra mapping here and one less mapping in MidiThru.  The scale lookup would
-- become a little messier since I'd need to lookup input to mapped input and
-- then mapped input to note.  PitchTrack would still need the scale.  I could
-- reduce the scope of InputKey or eliminate it entirely for the more universal
-- NoteNumber.  It seems like a wash at the moment.
cmds_with_note :: Bool -> [Cmd.Cmd] -> Cmd.Cmd
cmds_with_note kbd_entry cmds msg = do
    has_mods <- are_modifiers_down
    kbd_note <- if kbd_entry && not has_mods
        then do
            octave <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
            return (kbd_input octave msg)
        else return Nothing
    midi_note <- get_midi_input msg
    let maybe_new_msg = kbd_note `mplus` midi_note
    case maybe_new_msg of
        Just (Just new_msg) -> do
            forM_ cmds (\cmd -> Cmd.catch_abort (cmd new_msg))
            return Cmd.Done -- I mapped a key, so I must be done
        Just Nothing -> return Cmd.Done
        -- I'm assuming this is only applied to those who want the InputNotes.
        Nothing -> return Cmd.Continue

are_modifiers_down :: (Monad m) => Cmd.CmdT m Bool
are_modifiers_down = fmap (not . Set.null) Keymap.mods_down

-- ** kbd

kbd_input :: Pitch.Octave -> Msg.Msg -> Maybe (Maybe Msg.Msg)
kbd_input octave (Msg.key -> Just (down, key)) =
    (fmap . fmap) Msg.InputNote (key_to_input octave down key)
kbd_input _ _ = Nothing

key_to_input :: Pitch.Octave -> Bool -> Key.Key -> Maybe (Maybe InputNote.Input)
key_to_input oct down (Key.KeyChar c) =
    (fmap . fmap) (InputNote.from_key oct down) (Map.lookup c note_map)
-- Special keys can fall through.
key_to_input _ _ _ = Nothing

note_map :: Map.Map Char (Maybe Pitch.InputKey)
note_map = Map.map Just (Map.fromList (upper_keys ++ lower_keys))
    `Map.union` Map.fromList [(c, Nothing) | c <- all_keys]

-- | These are the 10 keys from left to right, not including @[]-=@ etc. since
-- those don't appear on the lower \"manual\" and they're useful for other
-- things anyway.
--
-- Qwerty's @q@ should be the middle C.
all_keys = "1234567890qwertyuiopasdfghjkl;zxcvbnm,./"
upper_keys, lower_keys :: [(Char, Pitch.InputKey)]
upper_keys = make_key_map 1 "q2w3er5t6y7ui"
lower_keys = make_key_map 0 "zsxdcvgbhnjm,"

make_key_map :: Pitch.Octave -> [Char] -> [(Char, Pitch.InputKey)]
make_key_map oct = map mk_input . zip [0..]
    where
    mk_input (n, c) = (Keymap.hardcoded_kbd_layout Map.! c,
        Pitch.InputKey (fromIntegral (oct*12+n)))

-- ** midi

get_midi_input :: (Monad m) => Msg.Msg -> Cmd.CmdT m (Maybe (Maybe Msg.Msg))
get_midi_input (Msg.Midi (Midi.ReadMessage rdev _ midi_msg)) = do
    rstate <- Cmd.get_rdev_state rdev
    case InputNote.from_midi rstate rdev midi_msg of
        Just (input, rstate2) -> do
            Cmd.set_rdev_state rdev rstate2
            return (Just (Just (Msg.InputNote input)))
        Nothing -> return (Just Nothing)
get_midi_input _ = return Nothing
