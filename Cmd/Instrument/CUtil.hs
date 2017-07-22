-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for instrument cmds.  This is called CUtil because there is also
-- "Derive.Instrument.DUtil" and they are usually imported together.
--
-- I need a better name than \"Util\" for everything.
module Cmd.Instrument.CUtil where
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.UiMsg as UiMsg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Keymap as Keymap
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified App.Config as Config
import Global
import Types


-- | Text of the event to create.
type Call = Text

-- * eval call

insert_call :: Cmd.M m => Map Char Expr.Symbol -> Msg.Msg -> m Cmd.Status
insert_call = insert_expr . Map.fromList . map (Keymap.physical_key *** to_expr)
        . Map.toList
    where to_expr call = Expr.generator0 call

notes_to_calls :: [Drums.Note] -> Map Char Expr.Symbol
notes_to_calls notes =
    Map.fromList [(Drums.note_char n, Drums.note_name n) | n <- notes]

{- | Create a custom kbd entry cmd that inserts tracklang expressions at
    the insertion point.  It also attempts to evaluate the expression to produce
    MIDI thru.

    This is more accurate and principled than what the usual kbd entry cmds do,
    since it reuses the deriver and performer directly, while they recreate
    the performer in an ad-hoc way, e.g. in "Cmd.MidiThru".  However, this
    allows them to play chords and is thus more suitable for pitched
    instruments.  Actually, what MidiThru recreates is the channel allocation
    part of the performer, ultimately becasue the performer's allocator doesn't
    work in real time.  Still, perhaps it would be possible to integrate them
    better than I have.
-}
insert_expr :: Cmd.M m => Map Char BaseTypes.Expr -> Msg.Msg
    -> m Cmd.Status
insert_expr char_to_expr msg = do
    unlessM Cmd.is_kbd_entry Cmd.abort
    EditUtil.fallthrough msg
    (kstate, char) <- Cmd.abort_unless $ Msg.char msg
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
            UiMsg.KeyDown -> do
                Cmd.set_status Config.status_note $ Just $ ShowVal.show_val expr
                call_keydown expr
            UiMsg.KeyUp -> call_keyup expr

call_keydown :: Cmd.M m => BaseTypes.Expr -> m ()
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
        ("keymap: " <> ShowVal.show_val expr)

call_keyup :: Cmd.M m => BaseTypes.Expr -> m ()
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
expr_to_midi :: Cmd.M m => BlockId -> TrackId -> TrackTime -> BaseTypes.Expr
    -> m [Midi.WriteMessage]
expr_to_midi block_id track_id pos expr = do
    result <- LEvent.write_snd_prefix "CUtil.expr_to_midi"
        =<< Perf.derive_expr block_id track_id pos expr
    events <- Cmd.require_right ("CUtil.expr_to_midi: "<>) result
    LEvent.write_snd =<< Perf.perform events

-- * keyswitch

{- | Create a Cmd to set keyswitches.

    This simply sets the note text for subsequent notes, and also configures
    the instrument to play in the given keyswitch.

    TODO this just emits keyswitches for every addr and emits them redundantly.
    This is simpler but it would be more correct to use WriteDeviceState to
    emit them only when needed.  However, it's more complicated because then
    I need a current attrs (Map Instrument Attrs) along with current note text,
    so MidiThru can use the attrs to find the keyswitch.

    TODO if I can pull the current or previous note out of the derive then
    I could use that to play an example note.  Wait until I have a "play
    current line" framework up for that.
-}
keyswitches :: Cmd.M m => [(Char, Expr.Symbol, Midi.Key)] -> Msg.Msg
    -> m Cmd.Status
keyswitches inputs = \msg -> do
    EditUtil.fallthrough msg
    char <- Cmd.abort_unless $ Msg.char_down msg
    (call, key) <- Cmd.abort_unless $ Map.lookup char to_call
    MidiThru.channel_messages Nothing False
        [Midi.NoteOn key 64, Midi.NoteOff key 64]
    Cmd.set_note_text (Expr.unsym call)
    return Cmd.Done
    where
    to_call = Map.fromList [(char, (call, key)) | (char, call, key) <- inputs]


-- * drums

-- | Create an unpitched drum instrument.  This is an instrument with an
-- enumeration of symbols and no pitch or duration.  Each key maps to its
-- own symbol.
simple_drum :: Maybe Score.Control -> [(Drums.Note, Midi.Key)]
    -> MidiInst.Patch -> MidiInst.Patch
simple_drum tune_control note_keys patch =
    MidiInst.code #= code $ drum_patch note_keys patch
    where code = drum_code tune_control (map fst note_keys)

-- ** code

-- | Construct code from drum notes.  This is both the deriver calls to
-- interpret the stroke names, and the cmds to enter them.
drum_code :: Maybe Score.Control -- ^ If given, this control indicates semitone
    -- offsets above or below the natural pitch.  Actual pitched drums which
    -- are tuned to a definite note should use 'pitched_drum_patch' and a
    -- pitch track.
    -> [Drums.Note] -> MidiInst.Code
drum_code tuning_control notes =
    MidiInst.note_generators (drum_calls Nothing tuning_control notes)
    <> MidiInst.cmd (drum_cmd notes)

drum_cmd :: Cmd.M m => [Drums.Note] -> Msg.Msg -> m Cmd.Status
drum_cmd = insert_call . notes_to_calls

-- ** patch

drum_patch :: [(Drums.Note, Midi.Key)] -> MidiInst.Patch -> MidiInst.Patch
drum_patch note_keys = MidiInst.patch
    %= MidiInst.add_flag Patch.Triggered
        . (Patch.call_map #= make_call_map (map fst note_keys))
        . (Patch.attribute_map #= Patch.unpitched_keymap
            [(Drums.note_attrs note, key) | (note, key) <- note_keys])

-- | (keyswitch, low, high, root_pitch).  The root pitch is the pitch at the
-- bottom of the key range, and winds up in 'Patch.PitchedKeymap'.
type KeyswitchRange = ([Patch.Keyswitch], Midi.Key, Midi.Key, Midi.Key)
type PitchedNotes = [(Drums.Note, KeyswitchRange)]

-- | Make a KeyswitchRange for each grouped Attributes set.  Attributes in the
-- same group get the same range and are differentiated by keyswitch.
make_keymap :: Maybe Midi.Key -- ^ Keyswitches start here.  If not given,
    -- this patch doesn't use keyswitches.
    -> Midi.Key -- ^ notes start here
    -> Midi.Key -- ^ each sound is mapped over this range
    -> Midi.Key -- ^ the pitch of the bottom note of each range
    -> [[Attrs.Attributes]] -> Map Attrs.Attributes KeyswitchRange
make_keymap base_keyswitch base_key range root_pitch groups = Map.fromList $ do
    (group, low) <- zip groups [base_key, base_key+range ..]
    (attrs, ks) <- zip group $ maybe (repeat [])
        (\base -> map ((:[]) . Patch.Keyswitch) [base..]) base_keyswitch
    return (attrs, (ks, low, low + (range-1), root_pitch))

-- | This is like 'make_keymap', except with the arguments rearranged to more
-- closely match the sample utils I use.
make_keymap2 :: Maybe Midi.Key -> Midi.Key -> Midi.Key -> Midi.Key
    -> Midi.Key -> [[Attrs.Attributes]]
    -> Map Attrs.Attributes KeyswitchRange
make_keymap2 base_keyswitch base_key natural_key range natural_nn =
    make_keymap base_keyswitch base_key range
        (natural_nn - Midi.from_key natural_key)

-- | This is like 'make_keymap', except that attributes are differentiated by
-- a 'Patch.ControlSwitch'.  CCs start at 102, and only groups of size >1
-- get a CC.  Since each group is controlled by its own CC number, you can then
-- select each variation independently.  This means any set of variations can
-- be played simultaneously, which is not true for keyswitches.
make_cc_keymap :: Midi.Key -- ^ notes start here
    -> Midi.Key -- ^ each sound is mapped over this range
    -> Midi.Key -- ^ the pitch of the bottom note of each range
    -> [[Attrs.Attributes]] -> Map Attrs.Attributes KeyswitchRange
make_cc_keymap base_key range root_pitch =
    Map.fromList . go base_cc . zip [base_key, base_key + range ..]
    where
    go _ [] = []
    go cc ((low, group) : groups) = case group of
        [] -> go cc groups
        [attrs] -> (attrs, ([], low, low + (range-1), root_pitch))
            : go cc groups
        _ ->
            [ (attrs, ([Patch.ControlSwitch cc cc_val],
                low, low + (range-1), root_pitch))
            | (attrs, cc_val) <- zip group [0 ..]
            ] ++ go (cc+1) groups
    -- There is an unallocated block [102 .. 119], which should be enough.
    base_cc = 102

-- | Annotate a Patch with an 'Patch.AttributeMap' from the given
-- PitchedNotes.
pitched_drum_patch :: PitchedNotes -> MidiInst.Patch -> MidiInst.Patch
pitched_drum_patch notes =
    MidiInst.patch %= MidiInst.add_flag Patch.Triggered
        . (Patch.call_map #= make_call_map (map fst notes))
        . (Patch.attribute_map #= make_attribute_map notes)

make_call_map :: [Drums.Note] -> Patch.CallMap
make_call_map =
    Map.fromList . map (\n -> (Drums.note_attrs n, Drums.note_name n))

make_attribute_map :: PitchedNotes -> Patch.AttributeMap
make_attribute_map notes = Common.attribute_map $ Seq.unique
    -- It's ok to have Notes with the same (attr, keyswitch), for instance if
    -- there are loud and soft versions, but make_attribute_map will see them
    -- as overlapping attrs, so filter out duplicates.
    [ (Drums.note_attrs note, (ks, Just (Patch.PitchedKeymap low high root)))
    | (note, (ks, low, high, root)) <- notes
    ]

-- | Make PitchedNotes by pairing each 'Drums.Note' with its 'KeyswitchRange'.
drum_pitched_notes :: [Drums.Note] -> Map Attrs.Attributes KeyswitchRange
    -> (PitchedNotes, ([Drums.Note], [Attrs.Attributes]))
    -- ^ Also return the notes with no mapping (so they can't be played), and
    -- keymap ranges with no corresponding notes (so there is no call to
    -- play them).
drum_pitched_notes notes keymap = (found, (not_found, unused))
    where
    unused = filter (`notElem` note_attrs) (Map.keys keymap)
    note_attrs = map Drums.note_attrs notes
    (not_found, found) = Either.partitionEithers $ map find notes
    find n = maybe (Left n) (Right . (,) n)
        (Map.lookup (Drums.note_attrs n) keymap)

-- | Create 0 duration calls from the given drum notes.
--
-- This should probably go in DUtil, but that would make it depend on
-- "Cmd.Instrument.Drums".
drum_calls :: Maybe ([Attrs.Attributes], Pitch.NoteNumber)
    -- ^ If Just, only strokes which are a superset of one of these move with
    -- the pitch, otherwise the stay at the given NoteNumber.  If Nothing, all
    -- strokes move with the pitch.
    -> Maybe Score.Control -> [Drums.Note]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
drum_calls pitched_strokes maybe_tuning_control = map $ \note ->
    ( Drums.note_name note
    , drum_call maybe_tuning_control (Drums.note_dynamic note)
        (Drums.note_attrs note) (set_pitch (Drums.note_attrs note))
    )
    where
    set_pitch attrs = case pitched_strokes of
        Just (pitched, natural_nn) | not (is_pitched pitched attrs) ->
            Call.with_pitch (PSignal.nn_pitch natural_nn)
        _ -> id
    is_pitched pitched attrs = any (Attrs.contain attrs) pitched

drum_call :: Maybe Score.Control -> Signal.Y -> Attrs.Attributes
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
drum_call maybe_tuning_control dyn attrs transform =
    Derive.generator Module.instrument name Tags.attr doc generate
    where
    name = Derive.CallName $ "drum attrs: " <> ShowVal.show_val attrs
    generate = Sig.call Sig.no_args $ \() -> Sub.inverting $ \args ->
        Call.multiply_dynamic dyn $ Call.add_attributes attrs $
            with_tuning args $ transform $
            Note.default_note Note.no_duration_attributes args
    with_tuning args = maybe id (tuning_control args) maybe_tuning_control
    doc = case maybe_tuning_control of
        Nothing -> ""
        Just control -> "This instrument is unpitched, but its tuning can be\
            \ adjusted with " <> ShowVal.doc control <> ", in semitones\
            \ from the natural pitch."

tuning_control :: Derive.NoteArgs -> Score.Control -> Derive.Deriver a
    -> Derive.Deriver a
tuning_control args control deriver = do
    tuning <- fromMaybe 0 <$>
        (Derive.untyped_control_at control =<< Args.real_start args)
    let nn = NN.middle_c + Pitch.nn tuning
    Call.with_pitch (PSignal.nn_pitch nn) deriver

-- * util

-- | Given a map describing how Attributes are mapped to the MIDI key range,
-- take a key binding to a 'PitchedNotes'.  The reason these are separate is
-- that the map describes how a particular patch maps attributes, while the
-- key binding describes the capabilities of the instrument itself.
--
-- If a mapping has 'Attrs.soft', it's looked up without the soft, but gets
-- the given dynamic.
resolve_strokes :: Signal.Y -> Map Attrs.Attributes KeyswitchRange
    -> [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
    -- ^ (key_binding, emits_text, call_attributes, stop_group)
    -> (PitchedNotes, [Text]) -- ^ also return errors
resolve_strokes soft_dyn keymap =
    check_dups . Either.partitionEithers . map resolve
    where
    resolve (char, call, attrs, group) =
        maybe (Right $ "unmapped: " <> pretty attrs) (Left . (note,)) $
            Map.lookup (Attrs.remove Attrs.soft attrs) keymap
        where
        note = (Drums.note_dyn char call attrs dyn) { Drums.note_group = group }
        dyn = if Attrs.contain attrs Attrs.soft then soft_dyn else 1
    check_dups (notes, msgs) = (notes3, dup_msgs ++ msgs)
        where
        dup_msgs = map ((">1 call with same name: "<>) . extract) by_name
            ++ map ((">1 call mapped to same key: "<>) . extract) by_key
        extract = pretty . map fst . (\(x, xs) -> x : xs)
        (notes2, by_name) = Seq.partition_dups (Drums.note_name . fst) notes
        (notes3, by_key) = Seq.partition_dups (Drums.note_char . fst) notes2
