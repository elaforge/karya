{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Cmds to add notes to a note track and send midi thru.

    This module is sister to 'Derive.Note' since it edits events that
    Derive.Note parses.

    A note track event has three fields: the interpolation method, the pitch,
    and the call.  The method and pitch are just as an indexed control track
    except that the pitch is interpreted relative to a given scale.  The call
    is used in sub-derivation, as documented in Derive.Note.

    - Midi keys send midi thru, and enter the scale degree (based on the octave
    offset) if edit mode is on.  Later this will go through a scale mapping
    layer, but for now it's hardcoded to Twelve.

    - In kbd_entry mode, letter keys do the same as midi keys, according to an
    organ-like layout.

    - But in non kbd_entry mode, letter keys edit the call text.

    - Interpolation methods are edited when shift is held down.
    TODO This might eat too much keyboard space, but we'll see.
-}
module Cmd.NoteTrack where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Midi.Midi as Midi

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControllerTrack as ControllerTrack
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Note as Note

import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller

import qualified App.Config as Config


-- | Some data all these functions tend to need.
data Info = Info {
    info_scale :: Pitch.Scale
    , info_addr :: Instrument.Addr
    , info_pb_range :: Controller.PbRange
    }

-- | Note that 'cmd_kbd_note_entry' does a midi thru on an entry.  This is
-- unlike 'cmd_midi_entry', which doesn't.  Midi thru is handled separately by
-- 'cmd_midi_thru' because midi notes always go through no matter what
-- else they may do.
cmd_kbd_note_entry :: Info -> Cmd.Cmd
cmd_kbd_note_entry = Keymap.make_cmd . kbd_note_entry

cmd_kbd_note_thru :: Info -> Cmd.Cmd
cmd_kbd_note_thru = Keymap.make_cmd . kbd_note_thru

cmd_midi_entry :: Pitch.Scale -> Cmd.Cmd
cmd_midi_entry scale msg = cmd_insert_midi_note scale msg
    >> Selection.cmd_advance_insert


-- * kbd note entry

-- | Enter notes from the computer keyboard.
kbd_note_entry :: Info -> [Keymap.Binding Identity.Identity]
kbd_note_entry info = concatMap (make_kbd_note_entry info True)
    (lower_notes ++ upper_notes)

kbd_note_thru :: Info -> [Keymap.Binding Identity.Identity]
kbd_note_thru info = concatMap (make_kbd_note_entry info False)
    (lower_notes ++ upper_notes)

oct_key oct0 (n0, c) = ((oct0 + oct1, n1), c)
    where (oct1, n1) = (n0 `divMod` 12)

lower_notes = map (oct_key 0) $ zip [0..]
    [ 'z', 's' -- C
    , 'x', 'd' -- D
    , 'c'
    , 'v', 'g' -- F
    , 'b', 'h'
    , 'n', 'j' -- A
    , 'm'
    , ',' -- C
    ]

upper_notes = map (oct_key 1) $ zip [0..]
    [ 'q', '2' -- C
    , 'w', '3'
    , 'e'
    , 'r', '5' -- F
    , 't', '6'
    , 'y', '7' -- A
    , 'u'
    , 'i' -- C
    ]

make_kbd_note_entry :: Info -> Bool -> (Pitch.KeyNumber, Char)
    -> [Keymap.Binding Identity.Identity]
make_kbd_note_entry info enter_notes (k_keynum, unmapped_char) =
    [ (spec UiMsg.KeyDown, Keymap.cspec_ (desc ++ " down") keydown_cmd)
    , (spec UiMsg.KeyUp, Keymap.cspec_ (desc ++ " up") keyup_cmd)
    ]
    where
    key = Key.KeyChar (Keymap.hardcoded_kbd_layout Map.! unmapped_char)
    spec state = Keymap.KeySpec Keymap.AnyCharMod (Keymap.UiKey state key)
    desc = "note pitch " ++ show k_keynum
    keydown_cmd = do
        mods <- Cmd.keys_down
        -- Key repeat makes multiple note ons.  Ignore it for note entry too,
        -- since I can't see wanting to spam out a million notes at once.
        when (Cmd.KeyMod key `Map.notMember` mods) $ do
            keynum <- transpose_keynum k_keynum
            when enter_notes $ do
                cmd_insert_pitch (info_scale info) keynum
                Selection.cmd_advance_insert
                return ()
            note <- keynum_to_note (info_scale info) keynum
            Cmd.set_status "note" (Just (Pitch.note_text note))
            cmd_note_on info keynum
            return ()
        return Cmd.Done
    keyup_cmd = cmd_note_off info =<< transpose_keynum k_keynum


-- * note entry

-- | Insert an event with the given pitch at the current insert point.
cmd_insert_pitch :: Pitch.Scale -> Pitch.KeyNumber -> Cmd.CmdId
cmd_insert_pitch scale keynum = keynum_to_note scale keynum >>= insert_note
    >> return Cmd.Done

cmd_insert_midi_note :: (Monad m) => Pitch.Scale -> Msg.Msg
    -> Cmd.CmdT m Cmd.Status
cmd_insert_midi_note scale msg = do
    key <- case msg of
        Msg.Midi (Midi.ReadMessage _ _
            (Midi.ChannelMessage _ (Midi.NoteOn key _))) -> return key
        _ -> Cmd.abort
    insert_note =<< keynum_to_note scale (fromIntegral key `divMod` 12)
    return Cmd.Done

transpose_keynum :: (Monad m) => Pitch.KeyNumber -> Cmd.CmdT m Pitch.KeyNumber
transpose_keynum (oct, num) = do
    oct_transpose <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    return (oct + oct_transpose, num)

keynum_to_note :: (Monad m) => Pitch.Scale -> Pitch.KeyNumber
    -> Cmd.CmdT m Pitch.Note
keynum_to_note scale keynum = do
    case Pitch.scale_key_to_note scale keynum of
        Left err -> Cmd.throw $ "couldn't convert keynum " ++ show keynum
            ++ ": " ++ err
        Right note -> return note

keynum_to_nn :: (Monad m) => Pitch.Scale -> Pitch.KeyNumber
    -> Cmd.CmdT m Pitch.NoteNumber
keynum_to_nn scale keynum = do
    note <- keynum_to_note scale keynum
    case Pitch.scale_to_nn scale note of
        Nothing -> Cmd.throw $ "can't convert note to nn from " ++ show keynum
        Just nn -> return nn


-- * edit note events

-- | Actually convert the given note to an event and insert it.
insert_note :: (Monad m) => Pitch.Note -> Cmd.CmdT m ()
insert_note note = modify_note (edit_note note)

edit_note note (method, _, call) = (method, Pitch.note_text note, call)

cmd_edit_call :: Cmd.Cmd
cmd_edit_call msg = do
    key <- Cmd.require (Msg.key msg)
    unless (is_edit_key key) Cmd.abort
    Log.debug $ "edit call: " ++ show key
    modify_note (edit_call key)
    return Cmd.Done

edit_call key (method, note, call) = (method, note, modify_text call key)

cmd_edit_method :: Cmd.Cmd
cmd_edit_method msg = do
    key <- Cmd.require (Msg.key msg)
    unless (is_edit_key key) Cmd.abort
    modify_note (edit_method key)
    return Cmd.Done

edit_method key (method, note, call) = (modify_text method key, note, call)

modify_note :: (Monad m) =>
    (Note.NoteTokens -> Note.NoteTokens) -> Cmd.CmdT m ()
modify_note f = do
    (track_id, tracknum, pos) <- Selection.get_insert_track
    end_pos <- Selection.step_from tracknum pos TimeStep.Advance
    event <- ControllerTrack.get_event track_id pos (end_pos - pos)

    tokens <- either (Cmd.throw . ("tokenizing note: "++)) return $
        Note.tokenize_note (Event.event_text event)
    let new_text = Note.untokenize_note (f tokens)
    State.insert_events track_id [(pos, event { Event.event_text = new_text })]

modify_text s Key.Backspace = Seq.rdrop 1 s
modify_text s (Key.KeyChar c) | Char.isPrint c = s ++ [c]
modify_text s _ = s

is_edit_key Key.Backspace = True
is_edit_key (Key.KeyChar c) | Id.is_identifier c = True
is_edit_key _ = False

-- * midi thru

-- | Send a midi thru msg for the given keynum.  Intended for midi \"thru\" for
-- kbd entry.
cmd_note_on :: (Monad m) => Info -> Pitch.KeyNumber -> Cmd.CmdT m Cmd.Status
cmd_note_on (Info scale (wdev, chan) pb_range) keynum = do
    msgs <- keynum_to_midi scale pb_range True 100 keynum
    mapM_ (Cmd.midi wdev) (map (Midi.ChannelMessage chan) msgs)
    return Cmd.Continue

cmd_note_off :: (Monad m) => Info -> Pitch.KeyNumber -> Cmd.CmdT m Cmd.Status
cmd_note_off (Info scale (wdev, chan) pb_range) keynum = do
    msgs <- keynum_to_midi scale pb_range False 0 keynum
    mapM_ (Cmd.midi wdev) (map (Midi.ChannelMessage chan) msgs)
    return Cmd.Continue

-- | Send midi thru, remapping notes and controllers to the given Addr.
-- NoteOns and NoteOffs are remapped based on the scale.
cmd_midi_thru :: Info -> Cmd.Cmd
cmd_midi_thru (Info scale (wdev, chan) pb_range) msg = do
    chan_msg <- case msg of
        Msg.Midi (Midi.ReadMessage _ _ (Midi.ChannelMessage _ msg)) ->
            return msg
        _ -> Cmd.abort
    tuned_msgs <- case chan_msg of
        Midi.NoteOn key vel -> keynum_to_midi scale pb_range True vel
            (fromIntegral key `divMod` 12)
        Midi.NoteOff key vel -> keynum_to_midi scale pb_range False vel
            (fromIntegral key `divMod` 12)
        _ -> return [chan_msg]
    mapM_ (Cmd.midi wdev) (map (Midi.ChannelMessage chan) tuned_msgs)
    return Cmd.Continue


keynum_to_midi :: (Monad m) => Pitch.Scale -> Controller.PbRange -> Bool
    -> Midi.Velocity -> Pitch.KeyNumber -> Cmd.CmdT m [Midi.ChannelMessage]
keynum_to_midi scale pb_range note_on vel keynum = do
    nn <- keynum_to_nn scale keynum
    let (key, pb) = Controller.pitch_to_midi pb_range (Pitch.un_nn nn)
    return $ if note_on
        then Midi.NoteOn key vel : if Pitch.scale_set_pitch_bend scale
            then [Midi.PitchBend pb] else []
        else [Midi.NoteOff key vel]
