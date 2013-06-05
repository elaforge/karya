-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.Util as CUtil
import qualified Cmd.Keymap as Keymap

import Derive.Attrs
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Instrument.Util as DUtil
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return synth_descs

synth_descs :: [MidiInst.SynthDesc]
synth_descs = MidiInst.make $
    (MidiInst.softsynth synth "Native Instruments Kontakt" pb_range [])
    { MidiInst.extra_patches = patches }

synth :: Instrument.SynthName
synth = "kkt"

patches :: [MidiInst.Patch]
patches = concat [hang, wayang, kendang_patches, mridangam_patches]
    where
    hang = MidiInst.with_code hang_code [inst "hang" hang_ks]
    wayang =
        [ (Instrument.set_scale wayang_umbang $ inst "wayang-umbang" wayang_ks,
            with_tuning "umbang" <> wayang_code)
        , (Instrument.set_scale wayang_isep $ inst "wayang-isep" wayang_ks,
            with_tuning "isep" <> wayang_code)
        ]
    with_tuning tuning =
        MidiInst.environ Environ.scale Wayang.scale_id
        <> MidiInst.environ Environ.tuning (tuning :: Text)
    inst name ks = Instrument.set_keyswitches ks $
        Instrument.patch $ Instrument.instrument name [] pb_range

pb_range :: Instrument.PbRange
pb_range = (-12, 12)

-- * hang

hang_code :: MidiInst.Code
hang_code = MidiInst.empty_code
    { MidiInst.code_note_calls = [Derive.map_lookup hang_calls]
    , MidiInst.code_cmds = [hang_cmd]
    }

hang_calls :: Derive.NoteCallMap
hang_calls = Derive.make_calls
    [ (text, Make.attributed_note attrs)
    | (attrs, _, Just text, _) <- hang_strokes
    -- Make sure to not shadow the default "" call.
    , not (Text.null text)
    ]

hang_cmd :: Cmd.Cmd
hang_cmd = CUtil.keyswitches [(Keymap.physical_key char, text, key)
    | (_, key, Just text, Just char) <- hang_strokes]

-- | The order is important because it determines attr lookup priority.
hang_strokes :: [(Score.Attributes, Midi.Key, Maybe Text, Maybe Char)]
hang_strokes =
    [ (center,  Key.c2,     Just "",            Just 'Z')
    , (edge,    Key.cs2,    Just "`pang2`",     Just 'X')
    , (slap,    Key.d2,     Just "`da3`",       Just 'C')
    , (middle,  Key.ds2,    Just "`zhong1`",    Just 'V')
    , (knuckle, Key.e2,     Just "`zhi3`",      Just 'B')
    , (mempty,  Key.c2,     Nothing,            Nothing)
    ]

hang_ks :: [(Score.Attributes, Midi.Key)]
hang_ks = [(attrs, key) | (attrs, key, _, _) <- hang_strokes]


-- * gender wayang

wayang_code :: MidiInst.Code
wayang_code =
    MidiInst.note_calls $ MidiInst.null_call (DUtil.note0_attrs mute)

wayang_ks :: [(Score.Attributes, Midi.Key)]
wayang_ks = [(mute, Key.gs2), (open, Key.g2), (mempty, Key.g2)]

wayang_umbang :: Instrument.PatchScale
wayang_umbang = Instrument.make_patch_scale $ zip wayang_keys Wayang.umbang

wayang_isep :: Instrument.PatchScale
wayang_isep = Instrument.make_patch_scale $ zip wayang_keys Wayang.isep

wayang_keys :: [Midi.Key]
wayang_keys =
    [ Key.a2 -- 6..
    , Key.c3, Key.d3, Key.e3, Key.g3, Key.a3 -- 1. to 6.
    , Key.c4, Key.d4, Key.e4, Key.g4 -- 1 to 5
    ]

-- * kendang

kendang_patches :: [MidiInst.Patch]
kendang_patches =
    [ (inst tunggal wadon_name, CUtil.drum_code tunggal)
    , (inst tunggal lanang_name, CUtil.drum_code tunggal)
    , (inst composite "kendang",
        kendang_composite_code (wadon_inst, lanang_inst))
    ]
    where
    tunggal = map fst Drums.kendang_tunggal
    composite = map fst Drums.kendang_composite
    inst notes name = CUtil.drum_instrument notes $
        Instrument.patch $ Instrument.instrument name [] pb_range
    wadon_inst = Score.instrument synth wadon_name
    lanang_inst = Score.instrument synth lanang_name
    wadon_name = "kendang-wadon"
    lanang_name = "kendang-lanang"

kendang_composite_code :: (Score.Instrument, Score.Instrument) -> MidiInst.Code
kendang_composite_code insts@(wadon, lanang) =
    MidiInst.note_calls (("realize", c_realize_kendang insts)
        : CUtil.drum_calls (map (fst . fst) Drums.kendang_composite))
    <> MidiInst.cmd (CUtil.inst_drum_cmd note_insts)
    where
    note_insts = [(note, key, inst_of kendang)
        | ((note, key), (_, kendang)) <- Drums.kendang_composite]
    inst_of Drums.Wadon = wadon
    inst_of Drums.Lanang = lanang

c_realize_kendang :: (Score.Instrument, Score.Instrument) -> Derive.NoteCall
c_realize_kendang insts = Derive.transformer "realize-kendang"
    (Tags.idiom <> Tags.postproc)
    ("Realize a composite kendang score into separate lanang and wadon parts."
    ) $ Sig.call0t $ \_ deriver -> do
        events <- deriver
        return $ realize_kendang insts events

-- | Split a composite kendang part into two separate wadon and lanang parts.
-- The realization is only a best guess and likely needs to be edited further.
--
-- This passes unrecognized notes through unchanged, so you can apply different
-- realizations in different places.
realize_kendang :: (Score.Instrument, Score.Instrument)
    -> Derive.Events -> Derive.Events
realize_kendang insts events = Derive.merge_asc_events $
    LEvent.map_around (emit_kendang insts) $ map (fmap lookup_attrs) events
    where
    lookup_attrs event = (event, attrs_of event)
    attrs_of event = Map.lookup (Score.event_attributes event) attrs_to_inst

type KendangEvent = (Attributes, Drums.Kendang)

-- | The realization is not correct because I don't yet fully understand how it
-- works.
--
-- > c kPtTtT+o+oo-+
-- > l .P.TPTP+^++.^
-- > w P.TPTP+.+.^-+
--
-- > c kPktT t T T t T .kP.tT.tTØØØ
-- > l .P.^T P T T P T .^P^.T .TØØØ
-- > w P^.TP T P P T P .P^.TP.TP. .
--
-- > c kP+otT kPkP+o+o kPuUtT+o
-- > l P.+.T^ P.P.+.+. P.o.T^+.
-- > w .P.+.T .P.P.+.+ .P.O.T^+
--
-- > c kPtTtT
-- > l .P.TPTP
-- > w P.TPTP
--
-- > tTkPtTkP
-- > T.P.T.P
-- > .T.P.T.P
--
-- > tT+otT+o
-- > TP+.TP+.
-- > .TP+.TP+
emit_kendang :: (Score.Instrument, Score.Instrument)
    -> [(Score.Event, Maybe KendangEvent)]
    -> (Score.Event, Maybe KendangEvent)
    -> [(Score.Event, Maybe KendangEvent)] -> [Score.Event]
emit_kendang _ _ (event, Nothing) _ = [event]
emit_kendang insts prev (event, Just (attrs, kendang)) next =
    make_event attrs main_inst : case filler of
        Nothing -> []
        Just second_attrs -> [make_event second_attrs second_inst]
    where
    make_event attrs inst = Score.modify_attributes (const attrs) $
        event { Score.event_instrument = inst }
    (main_inst, second_inst) = case kendang of
        Drums.Wadon -> insts
        Drums.Lanang -> (snd insts, fst insts)
    -- TODO This is not quite right, because wadon should be preparing lanang
    -- strokes, so the decision should be based on what the next lanang stroke
    -- is.  Also multiple fillers will alternate hands.
    -- TODO omit the filler if it's too fast, unless it's alternating hand with
    -- prev and next strokes.
    filler
        | attrs == plak || attrs == lanang <> tut = Nothing
        -- Use left hand if the pattern is T-T or T-+
        | prev_attrs == pang && next_attrs `elem` [pang, de, mempty] =
            Just pak
        | otherwise = Just (ka <> soft)
    prev_attrs = attrs_of prev
    next_attrs = attrs_of next
    attrs_of events = fromMaybe mempty $
        Seq.head [a | (_, Just (a, _)) <- events]

attrs_to_inst :: Map.Map Attributes KendangEvent
attrs_to_inst =
    Map.fromList [(Drums.note_attrs n, (attrs, kendang))
        | ((n, _), (attrs, kendang)) <- Drums.kendang_composite]


-- * mridangam

mridangam_patches :: [MidiInst.Patch]
mridangam_patches = [(inst, code)]
    where
    inst = Instrument.triggered $
        -- Instrument.set_attribute_map $
        Instrument.keymap #= mridangam_keymap $
        Instrument.patch $ Instrument.instrument "mridangam" [] pb_range
    code =
        MidiInst.note_calls (CUtil.drum_calls (concatMap fst mridangam_notes))
        <> MidiInst.cmd (CUtil.drum_cmd note_keys)
    note_keys = map (first head) mridangam_notes

mridangam_notes :: [([Drums.Note], Midi.Key)]
mridangam_notes = map mk mridangam
    where
    mk (name, attrs, char, (_, mid, _)) = (notes, mid)
        where
        notes = [Drums.Note name attr char 1 | attr <- attrs]

mridangam_keymap :: Instrument.Keymap
mridangam_keymap = Map.fromList
    [ (attr, (low, high, Just NN.e3))
    | (_, attrs, _, (low, _, high)) <- mridangam
    , attr <- attrs
    ]

mridangam :: [(Text, [Score.Attributes], Char, (Midi.Key, Midi.Key, Midi.Key))]
mridangam =
    -- left
    [ ("tha", [tha],            'z', (Key.g_1, Key.c0, Key.e0))
    , ("thom", [thom],          'x', (Key.g0, Key.c1, Key.e1))
    , ("thom/", [thom <> mute], 'c', (Key.g1, Key.c2, Key.e2))
    -- right
    -- TODO dhi as synonym for ki
    , ("ki", [ki],              'q', (Key.g2, Key.c3, Key.e3))
    , ("ta", [ta],              'w', (Key.g3, Key.c4, Key.e4))
    , ("nam", [nam],            'e', (Key.g4, Key.c5, Key.e5))
    , ("dhin", [dhin],          'r', (Key.g5, Key.c6, Key.e6))
    , ("dhin2", [dhin <> v2],   't', (Key.g6, Key.c7, Key.e7))
    , ("open", [open],          'y', (Key.g7, Key.c8, Key.e8))
    ]

tha = Score.attr "tha"
thom = Score.attr "thom"
ki = Score.attr "ki"
ta = Score.attr "ta"
nam = Score.attr "nam"
dhin = Score.attr "dhin"
