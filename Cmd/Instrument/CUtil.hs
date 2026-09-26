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

import qualified Util.Lists as Lists
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
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

insertCall :: Cmd.M m => Thru -> [(Char, Expr.Symbol)] -> Cmd.Handler m
insertCall thru charSyms = insertExpr thru (Cmd.WithoutOctave charToExpr)
    where
    toExpr call = Expr.generator0 call
    charToExpr = Map.fromList $
        map (bimap PhysicalKey.physicalKey toExpr) charSyms

strokesToCalls :: [Drums.Stroke] -> [(Char, Expr.Symbol)]
strokesToCalls strokes =
    [ (c, Drums._name s)
    | s <- strokes, Just c <- [Drums._char s]
    ]

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
insertExpr :: Cmd.M m => Thru -- ^ Evaluate the expression and emit MIDI thru.
    -> Cmd.NoteEntryMap DeriveT.Expr
    -> Cmd.Handler m
insertExpr thru noteEntryMap = handler $ \msg -> do
    unlessM Cmd.isKbdEntry Cmd.abort
    EditUtil.fallthrough EditUtil.NoBackspace msg
    (kstate, char) <- Cmd.abortUnless $ Msg.char msg
    octave <- Cmd.gets $ Cmd.stateKbdEntryOctave . Cmd.stateEdit
    case Cmd.noteEntryLookup octave char noteEntryMap of
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
            | Map.member char PhysicalKey.pitchMap -> pure Cmd.Done
            | otherwise -> pure Cmd.Continue
        Just expr -> do
            case thru of
                NoThru -> pure ()
                MidiThru -> exprMidiThru kstate expr
                ImThru thruF -> case kstate of
                    UiMsg.KeyDown ->
                        mapM_ Cmd.writeThru =<< exprImThru thruF expr
                    _ -> pure ()
            case kstate of
                UiMsg.KeyDown -> keydown expr
                _ -> pure ()
            pure Cmd.Done
    where
    handler = Cmd.Handler
        (Just $ mergeKbdEntry "" $ toKeycap <$> noteEntryMap)
        . Cmd.NamedCmd "insert_expr"
    keydown expr = do
        Cmd.setStatus Config.statusNote $ Just $ ShowVal.showVal expr
        whenM Cmd.isValEdit $ suppressed $ do
            pos <- EditUtil.getPos
            EditUtil.modifyEventAtTrigger pos False True $
                const (Just (ShowVal.showVal expr), True)
        where
        suppressed = Cmd.suppressHistory Cmd.ValEdit
            ("keymap: " <> ShowVal.showVal expr)

toKeycap :: DeriveT.Expr -> KeycapsT.KeyDoc
toKeycap = Expr.showValExpr

mergeKbdEntry :: a -> Cmd.NoteEntryMap a -> Cmd.NoteEntryMap a
mergeKbdEntry val = \case
    Cmd.WithOctave m -> Cmd.WithOctave $ (`Map.union` empty) <$> m
    Cmd.WithoutOctave m -> Cmd.WithoutOctave $ Map.union m empty
    where empty = const val <$> PhysicalKey.pitchMap

exprImThru :: Cmd.M m => Thru.ThruFunction -> DeriveT.Expr -> m [Cmd.Thru]
exprImThru thruF expr = do
    notes <- evalThruNotes expr
    msg <- Cmd.requireRight ("thru_f: "<>) $ thruF notes
    pure [Cmd.ImThru msg]

evalThruNotes :: Cmd.M m => DeriveT.Expr -> m [Thru.Note]
evalThruNotes expr = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    result <- LEvent.writeSndPrefix "CUtil.expr_attributes"
        =<< Perf.deriveExpr blockId trackId pos expr
    events <- Cmd.requireRight ("CUtil.expr_attributes: "<>) result
    when (null events) $ Cmd.throw $ "expected events when evaluating: "
        <> ShowVal.showVal expr
    pure $ map make events
    where
    make event = Thru.Note
        { _pitch = fromMaybe 0 (Score.initialNn event)
        , _velocity = Score.initialDynamic event
        , _attributes = Score.eventAttributes event
        , _startOffset = maybe 0 (floor . ScoreT.valOf) $
            Score.controlAt (Score.eventStart event)
                (Controls.fromShared Control.sampleStartOffset) event
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
exprMidiThru :: Cmd.M m => UiMsg.KbdState -> DeriveT.Expr -> m ()
exprMidiThru kstate expr = case kstate of
    UiMsg.KeyRepeat -> pure ()
    UiMsg.KeyDown -> do
        (blockId, _, trackId, pos) <- Selection.getInsert
        msgs <- exprToMidi blockId trackId pos expr
        let noteOns =
                [ (Midi.wmsgDev wmsg, Midi.wmsgMsg wmsg)
                | wmsg <- msgs
                , Midi.isNoteOn (Midi.wmsgMsg wmsg)
                ]
        mapM_ (uncurry Cmd.midi) noteOns
    UiMsg.KeyUp -> do
        -- This runs relies on 'expr_to_midi' producing exactly the same thing
        -- as the call above, so the NoteOffs will cancel out the NoteOns.  If
        -- this seems too unreliable, I could keep a Map from exprs to the note
        -- offs.  Keeping state around is also a bit unreliable, but maybe less
        -- so.
        (blockId, _, trackId, pos) <- Selection.getInsert
        msgs <- exprToMidi blockId trackId pos expr
        let noteOffs =
                [ (dev, msg)
                | Midi.WriteMessage dev _
                    msg@(Midi.ChannelMessage _ (Midi.NoteOff _ _)) <- msgs
                ]
        mapM_ (uncurry Cmd.midi) noteOffs

-- | Call a note call and return the MIDI msgs it produces.
exprToMidi :: Cmd.M m => BlockId -> TrackId -> TrackTime -> DeriveT.Expr
    -> m [Midi.WriteMessage]
exprToMidi blockId trackId pos expr = do
    result <- LEvent.writeSndPrefix "CUtil.expr_to_midi"
        =<< Perf.deriveExpr blockId trackId pos expr
    events <- Cmd.requireRight ("CUtil.expr_to_midi: "<>) result
    LEvent.writeSnd =<< Perf.perform events

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
    EditUtil.fallthrough EditUtil.NoBackspace msg
    char <- Cmd.abortUnless $ Msg.charDown msg
    (call, key) <- Cmd.abortUnless $ Map.lookup char toCall
    MidiThru.channelMessages Nothing False
        [Midi.NoteOn key 64, Midi.NoteOff key 64]
    Cmd.setNoteText (Expr.unsym call)
    pure Cmd.Done
    where
    handler = Cmd.Handler (Just noteEntryMap) . Cmd.NamedCmd "keyswitches"
    noteEntryMap = Cmd.WithoutOctave $ Expr.unsym . fst <$> toCall
    toCall = Map.fromList [(char, (call, key)) | (char, call, key) <- inputs]


-- * drums

-- | Create an unpitched drum instrument.  This is an instrument with an
-- enumeration of symbols and no pitch or duration.  Each key maps to its
-- own symbol.
simpleDrum :: Thru -> Maybe ScoreT.Control -> [(Drums.Stroke, Midi.Key)]
    -> MidiInst.Patch -> MidiInst.Patch
simpleDrum thru tuningControl strokeKeys patch =
    MidiInst.code #= code $ drumPatch strokeKeys patch
    where
    code = drumCode thru (map ((,config) . fst) strokeKeys)
    config = callConfig { _tuningControl = tuningControl }

-- ** code

-- | Construct code from drum strokes.  This is both the deriver calls to
-- interpret the stroke names, and the cmds to enter them.
drumCode :: Thru -> [(Drums.Stroke, CallConfig)] -> MidiInst.Code
drumCode = drumCodeCmd []

-- | 'drum_code', but with the opportunity to insert extra keys for
-- 'insert_call'.  This is because 'insert_expr' can't be stacked, since it
-- consumes kbd entry keys it doesn't map, since it's confusing if it doesn't.
drumCodeCmd :: [(Char, Expr.Symbol)] -> Thru -> [(Drums.Stroke, CallConfig)]
    -> MidiInst.Code
drumCodeCmd extraCmds thru strokeConfigs =
    MidiInst.noteGenerators (drumCalls strokeConfigs)
    <> MidiInst.cmd (drumCmd extraCmds thru (map fst strokeConfigs))

drumCode_ :: Thru -> [Drums.Stroke] -> MidiInst.Code
drumCode_ thru = drumCode thru . map (,callConfig)

drumCmd :: Cmd.M m => [(Char, Expr.Symbol)] -> Thru -> [Drums.Stroke]
    -> Cmd.Handler m
drumCmd extras thru = insertCall thru . (extras++) . strokesToCalls

-- ** patch

-- | Create a MIDI patch for the traditional kind of drum instrument where
-- each different stroke is mapped to a single MIDI keys.  'pitched_drum_patch'
-- is the variant where each stroke has a range of keys.
drumPatch :: [(Drums.Stroke, Midi.Key)] -> MidiInst.Patch -> MidiInst.Patch
drumPatch strokeKeys =
    MidiInst.triggered
    . (MidiInst.common#Common.callMap #= makeCallMap (map fst strokeKeys))
    . (MidiInst.patch#Patch.attributeMap #= keymap)
    where
    keymap = Patch.unpitchedKeymap
        [(Drums._attributes stroke, key) | (stroke, key) <- strokeKeys]

-- | im is much simpler than MIDI and doesn't need all the keymap garbage.
-- However, like 'drum_patch', this just sets patch config, the cmd and deriver
-- code has to be added separately, see 'drum_code' or 'drum_calls' for that.
imDrumPatch :: [Drums.Stroke] -> ImInst.Patch -> ImInst.Patch
imDrumPatch strokes =
    ImInst.triggered . (ImInst.common#Common.callMap #= makeCallMap strokes)

-- | Used with 'pitched_attribute_map' for MIDI instruments that do the
-- strategy of assigning pitch ranges to each drum stroke.
type PitchedStrokes = [(Drums.Stroke, KeyswitchRange)]

-- | (keyswitch, low, high, root_pitch).  The root pitch is the pitch at the
-- bottom of the key range, and winds up in 'Patch.PitchedKeymap'.
type KeyswitchRange = ([Patch.Keyswitch], Midi.Key, Midi.Key, Midi.Key)

-- | Make a KeyswitchRange for each grouped Attributes set.  Attributes in the
-- same group get the same range and are differentiated by keyswitch.
makeKeymap :: Maybe Midi.Key -- ^ Keyswitches start here.  If not given,
    -- this patch doesn't use keyswitches.
    -> Midi.Key -- ^ notes start here
    -> Midi.Key -- ^ each sound is mapped over this range
    -> Midi.Key -- ^ the pitch of the bottom note of each range
    -> [[Attrs.Attributes]] -> Map Attrs.Attributes KeyswitchRange
makeKeymap baseKeyswitch baseKey range rootPitch groups = Map.fromList $ do
    (group, low) <- zip groups [baseKey, baseKey+range ..]
    (attrs, ks) <- zip group $ maybe (repeat [])
        (\base -> map ((:[]) . Patch.Keyswitch) [base..]) baseKeyswitch
    pure (attrs, (ks, low, low + (range-1), rootPitch))

-- | This is like 'make_keymap', except with the arguments rearranged to more
-- closely match the sample utils I use.
makeKeymap2 :: Maybe Midi.Key -> Midi.Key -> Midi.Key -> Midi.Key
    -> Midi.Key -> [[Attrs.Attributes]]
    -> Map Attrs.Attributes KeyswitchRange
makeKeymap2 baseKeyswitch baseKey naturalKey range naturalNn =
    makeKeymap baseKeyswitch baseKey range
        (naturalNn - Midi.fromKey naturalKey)

-- | This is like 'make_keymap', except that attributes are differentiated by
-- a 'Patch.ControlSwitch'.  CCs start at 102, and only groups of size >1
-- get a CC.  Since each group is controlled by its own CC number, you can then
-- select each variation independently.  This means any set of variations can
-- be played simultaneously, which is not true for keyswitches.
makeCcKeymap :: Midi.Key -- ^ notes start here
    -> Midi.Key -- ^ each sound is mapped over this range
    -> Midi.Key -- ^ the pitch of the bottom note of each range
    -> [[Attrs.Attributes]] -> Map Attrs.Attributes KeyswitchRange
makeCcKeymap baseKey range rootPitch =
    Map.fromList . go baseCc . zip [baseKey, baseKey + range ..]
    where
    go _ [] = []
    go cc ((low, group) : groups) = case group of
        [] -> go cc groups
        [attrs] -> (attrs, ([], low, low + (range-1), rootPitch))
            : go cc groups
        _ ->
            [ (attrs, ([Patch.ControlSwitch cc ccVal],
                low, low + (range-1), rootPitch))
            | (attrs, ccVal) <- zip group [0 ..]
            ] ++ go (cc+1) groups
    -- There is an unallocated block [102 .. 119], which should be enough.
    baseCc = 102

-- | Annotate a Patch with an 'Patch.AttributeMap' from the given
-- PitchedStrokes.
pitchedDrumPatch :: PitchedStrokes -> MidiInst.Patch -> MidiInst.Patch
pitchedDrumPatch strokes =
    MidiInst.triggered
    . (MidiInst.common#Common.callMap #= makeCallMap (map fst strokes))
    . (MidiInst.patch#Patch.attributeMap #= pitchedAttributeMap strokes)

makeCallMap :: [Drums.Stroke] -> Common.CallMap
makeCallMap = Map.fromList . map (\n -> (Drums._attributes n, Drums._name n))

pitchedAttributeMap :: PitchedStrokes -> Patch.AttributeMap
pitchedAttributeMap strokes = Common.attributeMap $ Lists.unique
    -- It's ok to have Notes with the same (attr, keyswitch), for instance if
    -- there are loud and soft versions, but pitched_attribute_map will see them
    -- as overlapping attrs, so filter out duplicates.
    [ (Drums._attributes stroke, (ks, Just (Patch.PitchedKeymap low high root)))
    | (stroke, (ks, low, high, root)) <- strokes
    ]

-- | Make PitchedStrokes by pairing each 'Drums.Stroke' with its
-- 'KeyswitchRange'.
drumPitchedStrokes :: [Drums.Stroke] -> Map Attrs.Attributes KeyswitchRange
    -> (PitchedStrokes, ([Drums.Stroke], [Attrs.Attributes]))
    -- ^ Also return the strokes with no mapping (so they can't be played), and
    -- keymap ranges with no corresponding strokes (so there is no call to
    -- play them).
drumPitchedStrokes strokes keymap = (found, (notFound, unused))
    where
    unused = filter (`notElem` strokeAttrs) (Map.keys keymap)
    strokeAttrs = map Drums._attributes strokes
    (notFound, found) = Either.partitionEithers $ map find strokes
    find n = maybe (Left n) (Right . (,) n)
        (Map.lookup (Drums._attributes n) keymap)

-- | Create a 'drum_call' for each Drums.Stroke.
--
-- This should probably go in DUtil, but that would make it depend on
-- "Cmd.Instrument.Drums".
drumCalls :: [(Drums.Stroke, CallConfig)]
    -> [(Expr.Symbol, Derive.Generator Derive.Note)]
drumCalls = map $ \(stroke, config) ->
    ( Drums._name stroke
    , drumCall
        (config { _strokeDyn = Drums._dynamic stroke * _strokeDyn config })
        (Drums._name stroke) (Drums._attributes stroke)
    )

-- | For 'drum_calls'.  If Just, only strokes which are a superset of one of
-- these move with the pitch, otherwise the stay at the given NoteNumber.  If
-- Nothing, all strokes move with the pitch.
pitchedStrokes :: [Attrs.Attributes] -> Pitch.NoteNumber
    -> Attrs.Attributes -> CallConfig
pitchedStrokes pitched naturalNn attrs
    | any (Attrs.contain attrs) pitched = callConfig
    | otherwise = callConfig
        { _transform = Derive.withConstantPitch (PSignal.nnPitch naturalNn)
        }

data CallConfig = CallConfig
    -- | If set, look at this control for relative pitch adjustment.  For
    -- unpitched drums which nonetheless can use some tweaking.
    { _tuningControl :: Maybe ScoreT.Control
    -- | If set, take pitch from a $name-pitch arg, otherwise use the given
    -- pitch.
    , _naturalNn :: Maybe Pitch.NoteNumber
    -- | Multiply dyn signal by this.
    , _strokeDyn :: Signal.Y
    , _transform :: Derive.NoteDeriver -> Derive.NoteDeriver
    }

callConfig :: CallConfig
callConfig = CallConfig
    { _tuningControl = Nothing
    , _naturalNn = Nothing
    , _strokeDyn = 1
    , _transform = id
    }

-- | This is the common deriver call that all drums and drum-like instruments
-- use at the bottom.
drumCall :: CallConfig -> Expr.Symbol -> Attrs.Attributes
    -> Derive.Generator Derive.Note
drumCall (CallConfig tuningControl mbNaturalNn strokeDyn transform)
        (Expr.Symbol name) attrs =
    Derive.generator Module.instrument (Derive.CallName name) Tags.attr doc $
    Sig.call ((,)
        <$> Sig.defaulted "dyn" (1 :: Double) "Dyn multiplier."
        <*> (if Maybe.isNothing mbNaturalNn then pure Nothing
            else Sig.environ "pitch" Derive.Prefixed
                (Nothing :: Maybe Sig.Dummy) "doc")
    ) $ \(dyn, mbPitch) -> Sub.inverting $ \args ->
        Call.multiplyDynamic (strokeDyn * dyn) $ Call.addAttributes attrs $
            withTuning args $ transform $ setPitch mbPitch $
            Note.defaultNoteIntegrate name Note.noDurationAttributes args
    where
    withTuning args = maybe id (applyTuningControl args) tuningControl
    setPitch mbPitch = case mbNaturalNn of
        Nothing -> id
        Just naturalNn -> Derive.withConstantPitch
            (fromMaybe (PSignal.nnPitch naturalNn) mbPitch)
    doc = case tuningControl of
        Nothing -> ""
        Just control -> "This instrument is unpitched, but its tuning can be\
            \ adjusted with " <> ShowVal.doc control <> ", in semitones\
            \ from the natural pitch."

applyTuningControl :: Derive.NoteArgs -> ScoreT.Control -> Derive.Deriver a
    -> Derive.Deriver a
applyTuningControl args control deriver = do
    tuning <- fromMaybe 0 <$>
        (Derive.untypedControlAt control =<< Args.realStart args)
    let nn = NN.middleC + Pitch.nn tuning
    Derive.withConstantPitch (PSignal.nnPitch nn) deriver

-- * util

-- | Given a map describing how Attributes are mapped to the MIDI key range,
-- take a key binding to a 'PitchedStrokes'.  The reason these are separate is
-- that the map describes how a particular patch maps attributes, while the
-- key binding describes the capabilities of the instrument itself.
--
-- If a mapping has 'Attrs.soft', it's looked up without the soft, but gets
-- the given dynamic.
resolveStrokes :: Signal.Y -> Map Attrs.Attributes KeyswitchRange
    -> [(Char, Expr.Symbol, Attrs.Attributes, Drums.Group)]
    -- ^ (key_binding, emits_text, call_attributes, stop_group)
    -> (PitchedStrokes, [Text]) -- ^ also return errors
resolveStrokes softDyn keymap =
    checkDups . Either.partitionEithers . map resolve
    where
    resolve (char, call, attrs, group) =
        maybe (Left $ "unmapped: " <> pretty attrs) (Right . (stroke,)) $
            Map.lookup (Attrs.remove Attrs.soft attrs) keymap
        where
        stroke = (Drums.strokeDyn char call attrs dyn) { Drums._group = group }
        dyn = if Attrs.contain attrs Attrs.soft then softDyn else 1
    checkDups (msgs, strokes) = (strokes3, dupMsgs ++ msgs)
        where
        dupMsgs = map ((">1 call with same name: "<>) . extract) byName
            ++ map ((">1 call mapped to same key: "<>) . extract) byKey
        extract = pretty . map fst . (\(x, xs) -> x : xs)
        (strokes2, byName) = Lists.partitionDups (Drums._name . fst) strokes
        (strokes3, byKey) = Lists.partitionDups (Drums._char . fst) strokes2
