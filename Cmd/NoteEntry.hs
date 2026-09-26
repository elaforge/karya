-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{- | This module implements kbd entry by intercepting kbd and MIDI events and
    re-emitting them as InputNotes.  These then go to the track-specific edit
    cmds to enter notes and to "Cmd.MidiThru" which re-emits them as MIDI.
-}
module Cmd.NoteEntry (
    cmdsWithInput
    , floatingInputInsert
#ifdef TESTING
    , module Cmd.NoteEntry
#endif
) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.PhysicalKey as PhysicalKey

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
cmdsWithInput :: Cmd.M m => Bool -> Maybe Patch.Config
    -> [Msg.Msg -> m Cmd.Status] -> (Msg.Msg -> m Cmd.Status)
cmdsWithInput kbdEntry maybeConfig cmds msg =
    msgToInputs kbdEntry maybeConfig msg >>= \case
        Nothing -> Cmd.sequenceCmds cmds msg
        Just inputs -> foldr Cmd.mergeStatus Cmd.Done <$> mapM send inputs
    where
    send input = do
        case input of
            InputNote.NoteOn noteId _ _ ->
                Cmd.modifyWdevState $ \wdev -> wdev
                    { Cmd.wdevLastNoteId = Just noteId }
            _ -> pure ()
        Cmd.sequenceCmds cmds (Msg.InputNote input)

-- | Like 'cmds_with_input', but figure out kbd_entry and patch on my own.
runCmdsWithInput :: Cmd.M m => [Msg.Msg -> m Cmd.Status]
    -> (Msg.Msg -> m Cmd.Status)
runCmdsWithInput cmds msg = do
    kbdEntry <- Cmd.gets $ Cmd.stateKbdEntry . Cmd.stateEdit
    maybeConfig <- justm EditUtil.lookupInstrument $ \inst ->
        justm (Cmd.lookupInstrument inst) $
        pure . fmap snd . Cmd.midiPatch
    cmdsWithInput kbdEntry maybeConfig cmds msg

-- | Convert a Msg to 'Msg.InputNote's, if applicable.  Returns Nothing if
-- the Msg is not convertible to InputNotes (and therefore other cmds should
-- get it), and Just [] if it is but didn't emit any InputNotes (and therefore
-- this other cmds shouldn't get it).
msgToInputs :: Cmd.M m => Bool -> Maybe Patch.Config -> Msg.Msg
    -> m (Maybe [InputNote.Input])
msgToInputs kbdEntry maybeConfig msg = do
    hasMods <- areModifiersDown
    newMsgs <- if kbdEntry && not hasMods
        then do
            octave <- Cmd.gets (Cmd.stateKbdEntryOctave . Cmd.stateEdit)
            let isPressure = maybe False
                    (`Patch.hasFlag` Patch.Pressure) maybeConfig
            pure $ kbdInput isPressure octave msg
        else pure Nothing
    maybe (midiInput msg) (pure . Just) newMsgs

areModifiersDown :: Cmd.M m => m Bool
areModifiersDown = fmap (not . Set.null) Cmd.modsDown

-- ** kbd

-- | Convert a keyboard key-down to a 'Msg.InputNote'.
--
-- @Nothing@ means there's no input, @Just []@ means there was input, but
-- nothing to do.
kbdInput :: Bool -- ^ Whether this is a Pressure instrument or not.
    -- Pressure instruments respond to breath, and a kbd entry note on will
    -- emit an extra breath control.  This is convenient in practice because
    -- kbd entry is for quick and easy input and breath control gets in the way
    -- of that.
    -> Pitch.Octave -> Msg.Msg -> Maybe [InputNote.Input]
kbdInput isPressure octave (Msg.key -> Just (down, key)) = case down of
    UiMsg.KeyRepeat
        -- Just [] makes the repeats get eaten here, but make sure to only
        -- suppress them if this key would have generated a note.
        | Maybe.isJust mbInputs -> Just []
        | otherwise -> Nothing
    _ -> mbInputs
    where
    mbInputs = keyToInput isPressure octave (down == UiMsg.KeyDown) key
kbdInput _ _ _ = Nothing

keyToInput :: Bool -> Pitch.Octave -> Bool -> Key.Key
    -> Maybe [InputNote.Input]
keyToInput isPressure octave isDown (Key.Char c) = do
    pitch <- Map.lookup c PhysicalKey.pitchMap
    pure $ inputsOf (Pitch.addOctave octave pitch)
    where
    inputsOf pitch = case InputNote.fromAscii isDown pitch of
        input@(InputNote.NoteOn noteId _ _) | isPressure ->
            -- Breath goes second, otherwise thru won't think it belongs to
            -- this note.
            [input, breath noteId InputNote.keyboardVelocity]
        input -> [input]
    breath noteId val = InputNote.Control noteId Controls.breath val
keyToInput _ _ _ _ = Nothing

-- ** midi

-- | Convert a 'Msg.Midi' msg.
midiInput :: Cmd.M m => Msg.Msg -> m (Maybe [InputNote.Input])
midiInput (Msg.Midi (Midi.ReadMessage rdev _ midiMsg)) = do
    rstate <- Cmd.gets Cmd.stateRdevState
    case InputNote.fromMidi rstate rdev midiMsg of
        Just (input, rstate2) -> do
            Cmd.modify $ \st -> st { Cmd.stateRdevState = rstate2 }
            pure $ Just [input]
        Nothing -> pure (Just [])
midiInput _ = pure Nothing

-- * floating_input_insert

floatingInputInsert :: Cmd.M m => Msg.Msg -> m Cmd.Status
floatingInputInsert = \case
    Msg.InputNote (InputNote.NoteOn _ input _) ->
        Cmd.gets (Cmd.stateFloatingInput . Cmd.stateEdit) >>= \case
            True -> do
                note <- EditUtil.inputToNote input
                pure $ Cmd.FloatingInput $ Cmd.FloatingInsert $
                    " (" <> Pitch.noteText note <> ")"
            False -> pure Cmd.Continue
    _ -> pure Cmd.Continue
