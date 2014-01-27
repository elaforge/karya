-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for instrument cmds.  This is called CUtil because there is also
-- "Derive.Instrument.DUtil" and they are usually imported together.
--
-- I need a better name than \"Util\" for everything.
module Cmd.Instrument.CUtil where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Midi.Midi as Midi
import qualified Ui.UiMsg as UiMsg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Keymap as Keymap
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Call as Call
import qualified Derive.Call.Grace as Grace
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Call.Util
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified App.MidiInst as MidiInst
import Types


-- | Text of the event to create.
type Call = Text

-- * eval call

insert_call :: (Cmd.M m) => Map.Map Char Call -> Msg.Msg -> m Cmd.Status
insert_call = insert_expr . Map.fromList . map (Keymap.physical_key *** to_expr)
        . Map.toList
    where to_expr call = TrackLang.call (TrackLang.Symbol call) [] :| []

notes_to_calls :: [Drums.Note] -> Map.Map Char Call
notes_to_calls notes =
    Map.fromList [(Drums.note_char n, Drums.note_name n) | n <- notes]

-- | Create a custom kbd entry cmd that inserts tracklang expressions at
-- the insertion point.  It also attempts to evaluate the expression to produce
-- MIDI thru.
--
-- This is more accurate and principled than what the usual kbd entry cmds do,
-- since it reuses the deriver and performer directly, while they recreate
-- the performer in an ad-hoc way, e.g. in "Cmd.MidiThru".  However, this
-- allows them to play chords and is thus more suitable for pitched
-- instruments.  Actually, what MidiThru recreates is the channel allocation
-- part of the performer, ultimately becasue the performer's allocator doesn't
-- work in real time.  Still, perhaps it would be possible to integrate them
-- better than I have.
insert_expr :: (Cmd.M m) => Map.Map Char TrackLang.Expr -> Msg.Msg
    -> m Cmd.Status
insert_expr char_to_expr msg = do
    unlessM Cmd.is_kbd_entry Cmd.abort
    EditUtil.fallthrough msg
    (kstate, char) <- Cmd.require $ Msg.char msg
    case Map.lookup char char_to_expr of
        -- Only swallow keys that note entry would have caught, otherwise
        -- space would be swallowed here.
        --
        -- TODO another, possibly cleaner way to accomplish this would be to
        -- put the NoteEntry stuff in as a default instrument cmd, so I could
        -- just replace it entirely.
        Nothing
            | Map.member char NoteEntry.kbd_map -> return Cmd.Done
            | otherwise -> return Cmd.Continue
        Just expr -> (>> return Cmd.Done) $ case kstate of
            UiMsg.KeyRepeat -> return ()
            UiMsg.KeyDown -> call_keydown expr
            UiMsg.KeyUp -> call_keyup expr

call_keydown :: (Cmd.M m) => TrackLang.Expr -> m ()
call_keydown expr = do
    whenM Cmd.is_val_edit $ suppressed $ do
        pos <- EditUtil.get_pos
        NoteTrack.modify_event_at pos False True $
            const (Just (ShowVal.show_val expr), True)
    (block_id, _, track_id, pos) <- Selection.get_insert
    msgs <- expr_to_midi block_id track_id pos expr
    let note_ons = [(Midi.wmsg_dev wmsg, Midi.wmsg_msg wmsg) | wmsg <- msgs,
            Midi.is_note_on (Midi.wmsg_msg wmsg)]
    mapM_ (uncurry Cmd.midi) note_ons
    where
    suppressed = Cmd.suppress_history Cmd.ValEdit
        (untxt $ "keymap: " <> ShowVal.show_val expr)

call_keyup :: (Cmd.M m) => TrackLang.Expr -> m ()
call_keyup expr = do
    -- This runs expr_to_midi twice, and relies on it producing exactly the
    -- same thing twice, so the NoteOffs will cancel out the NoteOns.
    -- If this seems too unreliable, I could keep a Map from exprs to the note
    -- offs.  Keeping state around is also a bit unreliable, but maybe less so.
    (block_id, _, track_id, pos) <- Selection.get_insert
    msgs <- expr_to_midi block_id track_id pos expr
    let note_offs =
            [ (dev, msg)
            | Midi.WriteMessage dev _
                msg@(Midi.ChannelMessage _ (Midi.NoteOff _ _)) <- msgs
            ]
    mapM_ (uncurry Cmd.midi) note_offs

-- | Call a note call and return the MIDI msgs it produces.
expr_to_midi :: (Cmd.M m) => BlockId -> TrackId -> TrackTime -> TrackLang.Expr
    -> m [Midi.WriteMessage]
expr_to_midi block_id track_id pos expr = do
    (result, logs) <- Perf.derive_expr block_id track_id pos expr
    mapM_ Log.write logs
    events <- Cmd.require_right ("expr_to_midi: "<>) result
    (msgs, logs) <- Perf.perform events
    mapM_ Log.write logs
    return msgs

-- * keyswitch

-- | Create a Cmd to set keyswitches.
--
-- This simply sets the note text for subsequent notes, and also configures the
-- instrument to play in the given keyswitch.
--
-- TODO this just emits keyswitches for every addr and emits them redundantly.
-- This is simpler but it would be more correct to use WriteDeviceState to
-- emit them only when needed.  However, it's more complicated because then
-- I need a current attrs (Map Instrument Attrs) along with current note text,
-- so MidiThru can use the attrs to find the keyswitch.
--
-- TODO if I can pull the current or previous note out of the derive then I
-- could use that to play an example note.  Wait until I have a "play current
-- line" framework up for that.
keyswitches :: (Cmd.M m) => [(Char, Call, Midi.Key)] -> Msg.Msg -> m Cmd.Status
keyswitches inputs = \msg -> do
    EditUtil.fallthrough msg
    char <- Cmd.require $ Msg.char_down msg
    (note, key) <- Cmd.require $ Map.lookup char to_note
    MidiThru.channel_messages Nothing False
        [Midi.NoteOn key 64, Midi.NoteOff key 64]
    Cmd.set_note_text note
    return Cmd.Done
    where
    to_note = Map.fromList [(char, (note, key)) | (char, note, key) <- inputs]


-- * drums

-- ** code

-- | Construct code from drum notes.  This is both the deriver calls to
-- interpret the stroke names, and the cmds to enter them.
drum_code :: [Drums.Note] -> MidiInst.Code
drum_code notes = MidiInst.note_generators (drum_calls notes)
    <> MidiInst.cmd (drum_cmd notes)

drum_cmd :: [Drums.Note] -> Cmd.Cmd
drum_cmd = insert_call . notes_to_calls

-- ** patch

drum_patch :: [(Drums.Note, Midi.Key)] -> Instrument.Patch -> Instrument.Patch
drum_patch note_keys = Instrument.triggered
    . (Instrument.call_map #= make_call_map (map fst note_keys))
    . (Instrument.attribute_map #= Instrument.simple_keymap
        [(Drums.note_attrs note, key) | (note, key) <- note_keys])

-- | (keyswitch, low, high, root_pitch)
type KeyswitchRange = (Midi.Key, Midi.Key, Midi.Key, Pitch.NoteNumber)
type PitchedNotes = [(Drums.Note, KeyswitchRange)]

pitched_drum_patch :: PitchedNotes -> Instrument.Patch -> Instrument.Patch
pitched_drum_patch attr_map = Instrument.triggered
    . (Instrument.call_map #= make_call_map (map fst attr_map))
    . (Instrument.attribute_map #= make_attribute_map attr_map)

make_call_map :: [Drums.Note] -> Instrument.CallMap
make_call_map =
    Map.fromList . map (\n -> (Drums.note_attrs n, Drums.note_name n))

make_attribute_map :: PitchedNotes -> Instrument.AttributeMap
make_attribute_map attr_map = Instrument.make_attribute_map
    [ (Drums.note_attrs note, [Instrument.Keyswitch ks],
        Just (Instrument.PitchedKeymap low high root))
    | (note, (ks, low, high, root)) <- attr_map
    ]

-- | Create calls for the given Notes.
drum_calls :: [Drums.Note] -> [(Call, Derive.Generator Derive.Note)]
drum_calls notes =
    [(Drums.note_name n, note_call (Drums.note_dynamic n) (Drums.note_attrs n))
        | n <- notes]
    where
    note_call dyn attrs = Note.note_call
        ("drum: " <> ShowVal.show_val attrs) "" mempty
        (with_dyn dyn . Call.Util.add_attrs attrs
            . Note.default_note Note.no_duration_attributes)
    with_dyn = Derive.multiply_control Score.c_dynamic

multiple_calls :: [(Call, [Call])] -> [(Call, Derive.Generator Derive.Note)]
multiple_calls calls =
    [(call, multiple_call call subcalls) | (call, subcalls) <- calls]

-- | Create a call that just dispatches to other calls.
multiple_call :: Call -> [Call] -> Derive.Generator Derive.Note
multiple_call name calls = Derive.make_call name Tags.inst
    -- I intentionally omit the calls from the doc string, so they will
    -- combine in the call doc.  Presumably the calls are apparent from the
    -- name.
    "Dispatch to multiple calls." $ Sig.call0 $ \args ->
        mconcat $ map (Call.reapply_gen args . TrackLang.Symbol) calls

double_calls :: [(Call, Call)] -- ^ (call_name, repeated_call)
    -> [(Call, Derive.Generator Derive.Note)]
double_calls calls =
    [(name, double_call repeated) | (name, repeated) <- calls]

double_call :: Call -> Derive.Generator Derive.Note
double_call repeated = Derive.make_call "double" Tags.inst
    "Doubled call. This is a specialization of `roll`."
    $ Sig.call ((,)
    <$> Sig.defaulted "time" Grace.default_grace_dur "Time between the strokes."
    <*> Sig.defaulted "dyn" 0.5 "Dyn scale for grace notes."
    ) $ \(TrackLang.DefaultReal time, dyn) args ->
        Grace.repeat_notes (Call.reapply_gen_normalized args
                (TrackLang.Symbol repeated))
            1 time dyn args
