{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Cmds to add notes to a note track and send midi thru.

    - Midi keys send midi thru, and enter the scale degree (based on the octave
    offset) if edit mode is on.  Later this will go through a scale mapping
    layer, but for now it's hardcoded to Twelve.

    - Letter keys do the same as midi keys.  Once I have a small usb keyboard
    I may remove this to have letter keys available.

    - Interpolation methods are set by shifted keys.  This won't work for
    arbitrary #e, but I can think about that later.  In any case, the way to
    edit control track methods and values should be the same as note track
    ones.

    - For block calls, holding down option and typing will insert characters.
-}
module Cmd.NoteTrack where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import qualified Midi.Midi as Midi

import qualified Util.Log as Log

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

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


-- ** kbd note entry

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
make_kbd_note_entry info enter_notes (keynum, unmapped_char) =
    [ (spec UiMsg.KeyDown, Keymap.cspec_ (desc ++ " down") keydown_cmd)
    , (spec UiMsg.KeyUp, Keymap.cspec_ (desc ++ " up") keyup_cmd)
    ]
    where
    key = Key.KeyChar (Keymap.hardcoded_kbd_layout Map.! unmapped_char)
    spec state = Keymap.KeySpec Keymap.AnyCharMod (Keymap.UiKey state key)
    desc = "note pitch " ++ show keynum
    keydown_cmd = do
        mods <- Cmd.keys_down
        -- Key repeat makes multiple note ons.  Ignore it for note entry too,
        -- since I can't see wanting to spam out a million notes at once.
        when (Cmd.KeyMod key `Map.notMember` mods) $ do
            when enter_notes $ do
                cmd_insert_pitch (info_scale info) keynum
                Selection.cmd_advance_insert
                return ()
            note <- keynum_to_note (info_scale info) keynum
            Cmd.set_status "note" (Just (Pitch.note_text note))
            cmd_note_on info keynum
            return ()
        return Cmd.Done
    keyup_cmd = cmd_note_off info keynum


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

get_keynum :: (Monad m) => Pitch.KeyNumber -> Cmd.CmdT m Pitch.KeyNumber
get_keynum (oct, num) = do
    oct_transpose <- fmap Cmd.state_kbd_entry_octave Cmd.get_state
    return (oct + oct_transpose, num)

keynum_to_note :: (Monad m) => Pitch.Scale -> Pitch.KeyNumber
    -> Cmd.CmdT m Pitch.Note
keynum_to_note scale keynum0 = do
    keynum <- get_keynum keynum0
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

-- | Actually convert the given note to an event and insert it.
insert_note :: (Monad m) => Pitch.Note -> Cmd.CmdT m ()
insert_note note = do
    (pos, tracknum, track_id) <- Selection.get_insert_pos
    end_pos <- Selection.step_from tracknum pos TimeStep.Advance
    -- assert (end_pos >= pos)

    Log.debug $ "insert note " ++ show note ++ " at " ++ show (track_id, pos)
    let event = Config.event (Pitch.note_text note) (end_pos - pos)
    State.insert_events track_id [(pos, event)]

-- ** midi thru

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
    let (key, pb) = Controller.nn_to_midi pb_range nn
    return $ if note_on
        then Midi.NoteOn key vel : if Pitch.scale_set_pitch_bend scale
            then [Midi.PitchBend pb] else []
        else [Midi.NoteOff key vel]
