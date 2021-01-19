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
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Perf as Perf
import qualified Cmd.PhysicalKey as PhysicalKey
import qualified Cmd.Selection as Selection

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Instrument.Common as Common
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Thru as Thru
import qualified Ui.KeycapsT as KeycapsT
import qualified Ui.UiMsg as UiMsg

import           Global
import           Types


-- * eval call

insert_call :: Cmd.M m => Thru -> [(Char, Expr.Symbol)] -> Cmd.Handler m
insert_call thru char_syms = insert_expr thru (Cmd.WithoutOctave char_to_expr)
    where
    to_expr call = Expr.generator0 call
    char_to_expr = Map.fromList $
        map (bimap PhysicalKey.physical_key to_expr) char_syms

strokes_to_calls :: [Drums.Stroke] -> [(Char, Expr.Symbol)]
strokes_to_calls strokes = [(Drums._char s, Drums._name s) | s <- strokes]

-- | Select the flavor of thru to use when inserting an expression.  This
-- selects either 'expr_midi_thru' or 'expr_im_thru'.
--
-- Choosing manually is silly because the valid kind of thru depends on the
-- patch type.  It's just that due to history and wanting to avoid duplicated
-- code, the Cmd and Derive code in here doesn't care about MIDI vs. im...
-- except for thru.
data Thru = MidiThru | ImThru !Thru.ThruFunction | NoThru
    -- WRT ImThru, I can't just delegate to MidiThru.cmd_midi_thru because it
    -- doesn't know about the attrs, since it uses
    -- Cmd.get_instrument_attributes.

-- | Create a custom kbd entry cmd that inserts tracklang expressions at
-- the insertion point.  Since this shadows the default note entry cmd, it
-- has to handle thru on its own.
insert_expr :: Cmd.M m => Thru -- ^ Evaluate the expression and emit MIDI thru.
    -> Cmd.NoteEntryMap DeriveT.Expr
    -> Cmd.Handler m
insert_expr thru note_entry_map = handler $ \msg -> do
    unlessM Cmd.is_kbd_entry Cmd.abort
    EditUtil.fallthrough msg
    (kstate, char) <- Cmd.abort_unless $ Msg.char msg
    octave <- Cmd.gets $ Cmd.state_kbd_entry_octave . Cmd.state_edit
    case Cmd.note_entry_lookup octave char note_entry_map of
        Nothing
            -- Eat keys that normally would be eaten by kbd entry.  Otherwise
            -- it'll fall through to normal kbd entry and try to enter a
            -- pitched note.
            --
            -- TODO another possibly cleaner way to accomplish this would be to
            -- put the NoteEntry stuff in as a default instrument cmd, so I
            -- could just replace it entirely.  But I still want to mask out
            -- those PhysicalKey.pitch_map keys because it seems to confusing
            -- to fall through to an editing key.
            | Map.member char PhysicalKey.pitch_map -> return Cmd.Done
            | otherwise -> return Cmd.Continue
        Just expr -> do
            case thru of
                NoThru -> return ()
                MidiThru -> expr_midi_thru kstate expr
                ImThru thru_f -> case kstate of
                    UiMsg.KeyDown ->
                        mapM_ Cmd.write_thru =<< expr_im_thru thru_f expr
                    _ -> return ()
            case kstate of
                UiMsg.KeyDown -> keydown expr
                _ -> return ()
            return Cmd.Done
    where
    handler = Cmd.Handler
        (Just $ merge_kbd_entry "" $ to_keycap <$> note_entry_map)
        . Cmd.NamedCmd "insert_expr"
    keydown expr = do
        Cmd.set_status Config.status_note $ Just $ ShowVal.show_val expr
        whenM Cmd.is_val_edit $ suppressed $ do
            pos <- EditUtil.get_pos
            NoteTrack.modify_event_at pos False True $
                const (Just (ShowVal.show_val expr), True)
        where
        suppressed = Cmd.suppress_history Cmd.ValEdit
            ("keymap: " <> ShowVal.show_val expr)

to_keycap :: DeriveT.Expr -> KeycapsT.KeyDoc
to_keycap = Expr.show_val_expr

merge_kbd_entry :: a -> Cmd.NoteEntryMap a -> Cmd.NoteEntryMap a
merge_kbd_entry val = \case
    Cmd.WithOctave m -> Cmd.WithOctave $ (`Map.union` empty) <$> m
    Cmd.WithoutOctave m -> Cmd.WithoutOctave $ Map.union m empty
    where empty = const val <$> PhysicalKey.pitch_map

expr_im_thru :: Cmd.M m => Thru.ThruFunction -> DeriveT.Expr -> m [Cmd.Thru]
expr_im_thru thru_f expr = do
    notes <- eval_thru_notes expr
    msg <- Cmd.require_right ("thru_f: "<>) $ thru_f notes
    return [Cmd.ImThru msg]

eval_thru_notes :: Cmd.M m => DeriveT.Expr -> m [Thru.Note]
eval_thru_notes expr = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    result <- LEvent.write_snd_prefix "CUtil.expr_attributes"
        =<< Perf.derive_expr block_id track_id pos expr
    events <- Cmd.require_right ("CUtil.expr_attributes: "<>) result
    when (null events) $ Cmd.throw $ "expected events when evaluating: "
        <> ShowVal.show_val expr
    return $ map make events
    where
    make event = Thru.Note
        { _pitch = fromMaybe 0 (Score.initial_nn event)
        , _velocity = Score.initial_dynamic event
        , _attributes = Score.event_attributes event
        , _startOffset = maybe 0 (floor . ScoreT.typed_val) $
            Score.control_at (Score.event_start event)
                (Controls.from_shared Control.sampleStartOffset) event
        }

{- | Emit MIDI thru for an arbitrary expresison.

    This is more accurate and principled than what the usual kbd entry cmds do,
    since it reuses the deriver and performer directly, while they recreate the
    performer in an ad-hoc way, e.g. in "Cmd.MidiThru".  However, this allows
    them to play chords and is thus more suitable for pitched instruments.
    Actually, what MidiThru recreates is the channel allocation part of the
    performer, ultimately becasue the performer's allocator doesn't work in
    real time.  Still, perhaps it would be possible to integrate them better
    than I have.
-}
expr_midi_thru :: Cmd.M m => UiMsg.KbdState -> DeriveT.Expr -> m ()
expr_midi_thru kstate expr = case kstate of
    UiMsg.KeyRepeat -> return ()
    UiMsg.KeyDown -> do
        (block_id, _, track_id, pos) <- Selection.get_insert
        msgs <- expr_to_midi block_id track_id pos expr
        let note_ons =
                [ (Midi.wmsg_dev wmsg, Midi.wmsg_msg wmsg)
                | wmsg <- msgs
                , Midi.is_note_on (Midi.wmsg_msg wmsg)
                ]
        mapM_ (uncurry Cmd.midi) note_ons
    UiMsg.KeyUp -> do
        -- This runs relies on 'expr_to_midi' producing exactly the same thing
        -- as the call above, so the NoteOffs will cancel out the NoteOns.  If
        -- this seems too unreliable, I could keep a Map from exprs to the note
        -- offs.  Keeping state around is also a bit unreliable, but maybe less
        -- so.
        (block_id, _, track_id, pos) <- Selection.get_insert
        msgs <- expr_to_midi block_id track_id pos expr
        let note_offs =
                [ (dev, msg)
                | Midi.WriteMessage dev _
                    msg@(Midi.ChannelMessage _ (Midi.NoteOff _ _)) <- msgs
                ]
        mapM_ (uncurry Cmd.midi) note_offs

-- | Call a note call and return the MIDI msgs it produces.
expr_to_midi :: Cmd.M m => BlockId -> TrackId -> TrackTime -> DeriveT.Expr
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
keyswitches :: Cmd.M m => [(Char, Expr.Symbol, Midi.Key)] -> Cmd.Handler m
keyswitches inputs = handler $ \msg -> do
    EditUtil.fallthrough msg
    char <- Cmd.abort_unless $ Msg.char_down msg
    (call, key) <- Cmd.abort_unless $ Map.lookup char to_call
    MidiThru.channel_messages Nothing False
        [Midi.NoteOn key 64, Midi.NoteOff key 64]
    Cmd.set_note_text (Expr.unsym call)
    return Cmd.Done
    where
    handler = Cmd.Handler (Just note_entry_map) . Cmd.NamedCmd "keyswitches"
    note_entry_map = Cmd.WithoutOctave $ Expr.unsym . fst <$> to_call
    to_call = Map.fromList [(char, (call, key)) | (char, call, key) <- inputs]


-- * drums

-- | Create an unpitched drum instrument.  This is an instrument with an
-- enumeration of symbols and no pitch or duration.  Each key maps to its
-- own symbol.
simple_drum :: Thru -> Maybe ScoreT.Control -> [(Drums.Stroke, Midi.Key)]
    -> MidiInst.Patch -> MidiInst.Patch
simple_drum thru tuning_control stroke_keys patch =
    MidiInst.code #= code $ drum_patch stroke_keys patch
    where
    code = drum_code thru (map ((,config) . fst) stroke_keys)
    config = call_config { _tuning_control = tuning_control }

-- ** code

-- | Construct code from drum strokes.  This is both the deriver calls to
-- interpret the stroke names, and the cmds to enter them.
drum_code :: Thru -> [(Drums.Stroke, CallConfig)] -> MidiInst.Code
drum_code = drum_code_cmd []

-- | 'drum_code', but with the opportunity to insert extra keys for
-- 'insert_call'.  This is because 'insert_expr' can't be stacked, since it
-- consumes kbd entry keys it doesn't map, since it's confusing if it doesn't.
drum_code_cmd :: [(Char, Expr.Symbol)] -> Thru -> [(Drums.Stroke, CallConfig)]
    -> MidiInst.Code
drum_code_cmd extra_cmds thru stroke_configs =
    MidiInst.note_generators (drum_calls stroke_configs)
    <> MidiInst.cmd (drum_cmd extra_cmds thru (map fst stroke_configs))

drum_code_ :: Thru -> [Drums.Stroke] -> MidiInst.Code
drum_code_ thru = drum_code thru . map (,call_config)

drum_cmd :: Cmd.M m => [(Char, Expr.Symbol)] -> Thru -> [Drums.Stroke]
    -> Cmd.Handler m
drum_cmd extras thru = insert_call thru . (extras++) . strokes_to_calls

-- ** patch

-- | Create a MIDI patch for the traditional kind of drum instrument where
-- each different stroke is mapped to a single MIDI keys.  'pitched_drum_patch'
-- is the variant where each stroke has a range of keys.
drum_patch :: [(Drums.Stroke, Midi.Key)] -> MidiInst.Patch -> MidiInst.Patch
drum_patch stroke_keys =
    MidiInst.triggered
    . (MidiInst.common#Common.call_map #= make_call_map (map fst stroke_keys))
    . (MidiInst.patch#Patch.attribute_map #= keymap)
    where
    keymap = Patch.unpitched_keymap
        [(Drums._attributes stroke, key) | (stroke, key) <- stroke_keys]

-- | im is much simpler than MIDI and doesn't need all the keymap garbage.
-- However, like 'drum_patch', this just sets patch config, the cmd and deriver
-- code has to be added separately, see 'drum_code' or 'drum_calls' for that.
im_drum_patch :: [Drums.Stroke] -> ImInst.Patch -> ImInst.Patch
im_drum_patch strokes =
    ImInst.triggered . (ImInst.common#Common.call_map #= make_call_map strokes)

-- | Used with 'pitched_attribute_map' for MIDI instruments that do the
-- strategy of assigning pitch ranges to each drum stroke.
type PitchedStrokes = [(Drums.Stroke, KeyswitchRange)]

-- | (keyswitch, low, high, root_pitch).  The root pitch is the pitch at the
-- bottom of the key range, and winds up in 'Patch.PitchedKeymap'.
type KeyswitchRange = ([Patch.Keyswitch], Midi.Key, Midi.Key, Midi.Key)

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
-- PitchedStrokes.
pitched_drum_patch :: PitchedStrokes -> MidiInst.Patch -> MidiInst.Patch
pitched_drum_patch strokes =
    MidiInst.triggered
    . (MidiInst.common#Common.call_map #= make_call_map (map fst strokes))
    . (MidiInst.patch#Patch.attribute_map #= pitched_attribute_map strokes)

make_call_map :: [Drums.Stroke] -> Common.CallMap
make_call_map = Map.fromList . map (\n -> (Drums._attributes n, Drums._name n))

pitched_attribute_map :: PitchedStrokes -> Patch.AttributeMap
pitched_attribute_map strokes = Common.attribute_map $ Seq.unique
    -- It's ok to have Notes with the same (attr, keyswitch), for instance if
    -- there are loud and soft versions, but pitched_attribute_map will see them
    -- as overlapping attrs, so filter out duplicates.
    [ (Drums._attributes stroke, (ks, Just (Patch.PitchedKeymap low high root)))
    | (stroke, (ks, low, high, root)) <- strokes
    ]

-- | Make PitchedStrokes by pairing each 'Drums.Stroke' with its
-- 'KeyswitchRange'.
drum_pitched_strokes :: [Drums.Stroke] -> Map Attrs.Attributes KeyswitchRange
    -> (PitchedStrokes, ([Drums.Stroke], [Attrs.Attributes]))
    -- ^ Also return the strokes with no mapping (so they can't be played), and
    -- keymap ranges with no corresponding strokes (so there is no call to
    -- play them).
drum_pitched_strokes strokes keymap = (found, (not_found, unused))
    where
    unused = filter (`notElem` stroke_attrs) (Map.keys keymap)
    stroke_attrs = map Drums._attributes strokes
    (not_found, found) = Either.partitionEithers $ map find strokes
    find n = maybe (Left n) (Right . (,) n)
        (Map.lookup (Drums._attributes n) keymap)

-- | Create a 'drum_call' for each Drums.Stroke.
--
-- This should probably go in DUtil, but that would make it depend on
-- "Cmd.Instrument.Drums".
drum_calls :: [(Drums.Stroke, CallConfig)]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
drum_calls = map $ \(stroke, config) ->
    ( Drums._name stroke
    , drum_call
        (config { _stroke_dyn = Drums._dynamic stroke * _stroke_dyn config })
        (Drums._name stroke) (Drums._attributes stroke)
    )

-- | For 'drum_calls'.  If Just, only strokes which are a superset of one of
-- these move with the pitch, otherwise the stay at the given NoteNumber.  If
-- Nothing, all strokes move with the pitch.
pitched_strokes :: [Attrs.Attributes] -> Pitch.NoteNumber
    -> Attrs.Attributes -> CallConfig
pitched_strokes pitched natural_nn attrs
    | any (Attrs.contain attrs) pitched = call_config
    | otherwise = call_config
        { _transform = Derive.with_constant_pitch (PSignal.nn_pitch natural_nn)
        }

data CallConfig = CallConfig {
    -- | If set, look at this control for relative pitch adjustment.  For
    -- unpitched drums which nonetheless can use some tweaking.
    _tuning_control :: Maybe ScoreT.Control
    -- | If set, take pitch from a $name-pitch arg, otherwise use the given
    -- pitch.
    , _natural_nn :: Maybe Pitch.NoteNumber
    -- | Multiply dyn signal by this.
    , _stroke_dyn :: Signal.Y
    , _transform :: Derive.NoteDeriver -> Derive.NoteDeriver
    }

call_config :: CallConfig
call_config = CallConfig
    { _tuning_control = Nothing
    , _natural_nn = Nothing
    , _stroke_dyn = 1
    , _transform = id
    }

-- | This is the common deriver call that all drums and drum-like instruments
-- use at the bottom.
drum_call :: CallConfig -> Expr.Symbol -> Attrs.Attributes
    -> Derive.Generator Derive.Note
drum_call (CallConfig tuning_control mb_natural_nn stroke_dyn transform)
        (Expr.Symbol name) attrs =
    Derive.generator Module.instrument (Derive.CallName name) Tags.attr doc $
    Sig.call ((,)
        <$> Sig.defaulted "dyn" 1 "Dyn multiplier."
        <*> (if Maybe.isNothing mb_natural_nn then pure Nothing
            else Sig.environ "pitch" Derive.Prefixed Nothing "doc")
    ) $ \(dyn, mb_pitch) -> Sub.inverting $ \args ->
        Call.multiply_dynamic (stroke_dyn * dyn) $ Call.add_attributes attrs $
            with_tuning args $ transform $ set_pitch mb_pitch $
            Note.default_note Note.no_duration_attributes args
    where
    with_tuning args = maybe id (apply_tuning_control args) tuning_control
    set_pitch mb_pitch = case mb_natural_nn of
        Nothing -> id
        Just natural_nn -> Derive.with_constant_pitch
            (fromMaybe (PSignal.nn_pitch natural_nn) mb_pitch)
    doc = case tuning_control of
        Nothing -> ""
        Just control -> "This instrument is unpitched, but its tuning can be\
            \ adjusted with " <> ShowVal.doc control <> ", in semitones\
            \ from the natural pitch."

apply_tuning_control :: Derive.NoteArgs -> ScoreT.Control -> Derive.Deriver a
    -> Derive.Deriver a
apply_tuning_control args control deriver = do
    tuning <- fromMaybe 0 <$>
        (Derive.untyped_control_at control =<< Args.real_start args)
    let nn = NN.middle_c + Pitch.nn tuning
    Derive.with_constant_pitch (PSignal.nn_pitch nn) deriver

-- * util

-- | Given a map describing how Attributes are mapped to the MIDI key range,
-- take a key binding to a 'PitchedStrokes'.  The reason these are separate is
-- that the map describes how a particular patch maps attributes, while the
-- key binding describes the capabilities of the instrument itself.
--
-- If a mapping has 'Attrs.soft', it's looked up without the soft, but gets
-- the given dynamic.
resolve_strokes :: Signal.Y -> Map Attrs.Attributes KeyswitchRange
    -> [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
    -- ^ (key_binding, emits_text, call_attributes, stop_group)
    -> (PitchedStrokes, [Text]) -- ^ also return errors
resolve_strokes soft_dyn keymap =
    check_dups . Either.partitionEithers . map resolve
    where
    resolve (char, call, attrs, group) =
        maybe (Left $ "unmapped: " <> pretty attrs) (Right . (stroke,)) $
            Map.lookup (Attrs.remove Attrs.soft attrs) keymap
        where
        stroke = (Drums.stroke_dyn char call attrs dyn) { Drums._group = group }
        dyn = if Attrs.contain attrs Attrs.soft then soft_dyn else 1
    check_dups (msgs, strokes) = (strokes3, dup_msgs ++ msgs)
        where
        dup_msgs = map ((">1 call with same name: "<>) . extract) by_name
            ++ map ((">1 call mapped to same key: "<>) . extract) by_key
        extract = pretty . map fst . (\(x, xs) -> x : xs)
        (strokes2, by_name) = Seq.partition_dups (Drums._name . fst) strokes
        (strokes3, by_key) = Seq.partition_dups (Drums._char . fst) strokes2
