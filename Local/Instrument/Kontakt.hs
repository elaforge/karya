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
import qualified Midi.CC as CC
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Keymap as Keymap
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Attrs as Attrs
import Derive.Attrs
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Bali.Kotekan as Kotekan
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.LEvent as LEvent
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch

import qualified Local.Instrument.Reaktor as Reaktor
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
patches = concat
    [ misc_patches
    , hang_patches, wayang_patches, kendang_patches, mridangam_patches
    ]

patch :: Instrument.InstrumentName -> Instrument.Patch
patch name = Instrument.patch $ Instrument.instrument name [] pb_range

-- One pitch bend modulator can only do +-12, but if you put two on you get
-- +-24.
pb_range :: Instrument.PbRange
pb_range = (-24, 24)

-- * misc

misc_patches :: [MidiInst.Patch]
misc_patches = concat [balalaika, mcgill]

library :: [MidiInst.Patch]
library = MidiInst.with_empty_code
    [ inst "choir"
        [ (1, "vowel") ]
    ]
    where
    inst name controls = Instrument.patch $
        Instrument.instrument name controls pb_range

-- | From the McGill sample library.
mcgill :: [MidiInst.Patch]
mcgill = MidiInst.with_empty_code
    [ pressure "viol", pressure "shawm", pressure "crumhorn"
    , plucked "lute"
    , filtered (pressure "filtered")
    ]
    where
    plucked name = Instrument.patch $ Instrument.instrument name [] pb_range
    pressure name = MidiInst.pressure $ Instrument.patch $
        Instrument.instrument name
            [(CC.cc14, Controls.fc), (CC.cc15, Controls.q)] pb_range
    filtered = Instrument.composite #= [Reaktor.filter_composite]

-- | Ilya Efimov Bailalaika Prima
-- I changed it to support (-24, 24) pb range.
balalaika :: [MidiInst.Patch]
balalaika =
    with_code $ (:[]) $
        Instrument.attribute_map #= Instrument.simple_keyswitches ks $
        Instrument.patch $ (Instrument.hold_keyswitch #= True) $
        Instrument.instrument "balalaika" controls pb_range
    where
    with_code = MidiInst.with_code $ MidiInst.note_generators
        [("(", Articulation.c_attr_legato)]
    -- g6 strum, a6 solo, b6 harmony
    controls =
        [ (1, "trem-dyn")
        , (2, "trem-speed")
        ]
    ks =
        [ (Score.attr "str2", Key.ds4)
        , (Attrs.gliss, Key.c4)
        , (Attrs.legato, Key.as3)
        , (Attrs.vib, Key.d4)
        , (Attrs.harm, Key.gs3)
        , (Attrs.staccato, Key.cs4)
        -- These are just pressed, not held, but hold_keyswitch is
        -- per-patch, not per-keyswitch.
        , (Attrs.trem, Key.a3)
        , (mempty, Key.b3)
        ]

sonic_couture :: [MidiInst.Patch]
sonic_couture = MidiInst.with_empty_code
    [ inst "ebow"
        [ (1, "harm")
        , (21, "lpf")
        , (22, "q")
        , (23, "hpf")
        ]
    ]
    where
    inst name controls = Instrument.patch $
        Instrument.instrument name controls pb_range

-- * hang

hang_patches :: [MidiInst.Patch]
hang_patches = MidiInst.with_code hang_code
    [ Instrument.attribute_map #= Instrument.simple_keyswitches hang_ks $
        patch "hang"
    ]

hang_code :: MidiInst.Code
hang_code =
    MidiInst.note_calls
        [ MidiInst.both text (Make.attributed_note attrs)
        | (attrs, _, Just text, _) <- hang_strokes
        -- Make sure to not shadow the default "" call.
        , not (Text.null text)
        ]
    <> MidiInst.cmd hang_cmd

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

{- | Layout:

    > 0         10        20        30        40        50        60        70        80        90        100       110       120    127
    > 01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    > c-2         c-1         c0          c1          c2          c3          c4          c5          c6          c7          c8     g8
    >                  p----------------------|
    >                              k----------------------|
    >          X X|-----------------------------------------------|
    >                                                                  p----------------------|
    >                                                                              k----------------------|
    >                                                             |-----------------------------------------------|

    > pemade mute: (f_1, e1), open: (f3, e5)
    > kantil mute: (f0, e2), open: (f4, e6)
    > mute keyswitch: a_2, b_2
-}
wayang_patches :: [MidiInst.Patch]
wayang_patches =
    [ (scale Wayang.umbang $ wayang "wayang-umbang",
        set_tuning "umbang" <> wayang_code)
    , (scale Wayang.isep $ wayang "wayang-isep",
        set_tuning "isep" <> wayang_code)
    , (wayang "wayang", pasang_code)
    -- These have "kempyung-top" set for the benefit of the kempyung call.
    -- TODO probably at some point maybe I'll want a generic way to get
    -- instrument ranges.
    , (wayang "wayang-p", pasang_code <> top_pitch Wayang.pemade_top)
    , (wayang "wayang-k", pasang_code <> top_pitch Wayang.kantilan_top)
    , (Instrument.text #= "Tuned to 12TET." $ wayang "wayang12", wayang_code)
    ]
    where
    wayang = (Instrument.attribute_map #= wayang_keymap) . patch
    scale scale = (Instrument.text #= doc)
        . (Instrument.scale #= wayang_scale scale)
    set_tuning tuning = MidiInst.default_scale Wayang.scale_id
        <> MidiInst.environ Environ.tuning (tuning :: Text)
    top_pitch nn = MidiInst.environ "kempyung-top" (Pitches.constant nn)
    doc = "These set the scale and tuning automatically, and expect the patch\
        \ to be tuned to its natural scale."

wayang_code :: MidiInst.Code
wayang_code = MidiInst.note_calls $ MidiInst.null_call $
    DUtil.zero_duration "Add `+mute+loose` attribute and scale dyn by 0.75." $
        Util.add_attrs (Attrs.mute <> Attrs.loose) . Util.multiply_dynamic 0.75

-- | Emit events for both polos and sangsih.
pasang_code :: MidiInst.Code
pasang_code = MidiInst.cmd pasang_thru

-- | Dispatch MIDI through to both polos and sangsih instruments.
pasang_thru :: Cmd.Cmd
pasang_thru msg = do
    NoteEntry.run_cmds_with_input [cmd] msg
    -- This cmd just does midi thru, so I don't want it to return Done and
    -- prevent track editing cmds from happening.
    return Cmd.Continue
    where
    cmd msg = do
        input <- case msg of
            Msg.InputNote input -> return input
            _ -> Cmd.abort
        (block_id, _, track_id, _) <- Selection.get_insert
        polos <- Perf.lookup_val block_id (Just track_id) Kotekan.inst_polos
        sangsih <- Perf.lookup_val block_id (Just track_id) Kotekan.inst_sangsih
        whenJust polos $ \inst ->
            MidiThru.midi_thru_instrument inst input
        whenJust sangsih $ \inst ->
            MidiThru.midi_thru_instrument inst $
                InputNote.multiply_note_id 1 input
        return Cmd.Continue

wayang_scale :: [Pitch.NoteNumber] -> Instrument.PatchScale
wayang_scale scale = Instrument.make_patch_scale $ zip wayang_keys scale

wayang_keys :: [Midi.Key]
wayang_keys = take (5*3) $ drop 1 $ concatMap keys [0..]
    where
    keys oct = map (Midi.to_key (oct * 12) +)
        [Key2.e3, Key2.f3, Key2.a3, Key2.b3, Key2.c4]

wayang_keymap :: Instrument.AttributeMap
wayang_keymap = Instrument.AttributeMap
    [ (Attrs.mute <> Attrs.loose, [Instrument.Keyswitch Key2.a_2], keymap)
    , (Attrs.mute, [Instrument.Keyswitch Key2.b_2], keymap)
    ]
    where
    keymap = Just $
        Instrument.PitchedKeymap Key2.c_1 Key2.b2 (Midi.from_key Key2.c3)


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
    MidiInst.note_transformers [("realize", c_realize_kendang insts)]
    <> MidiInst.note_generators
        (CUtil.drum_calls (map (fst . fst) Drums.kendang_composite))
    <> MidiInst.cmd (CUtil.inst_drum_cmd note_insts)
    where
    note_insts = [(note, key, inst_of kendang)
        | ((note, key), (_, kendang)) <- Drums.kendang_composite]
    inst_of Drums.Wadon = wadon
    inst_of Drums.Lanang = lanang

c_realize_kendang :: (Score.Instrument, Score.Instrument)
    -> Derive.Transformer Derive.Note
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
        Instrument.attribute_map #= mridangam_keymap $
        Instrument.patch $ Instrument.instrument "mridangam" [] pb_range
    code =
        MidiInst.note_generators
            (CUtil.drum_calls (concatMap fst mridangam_notes))
        <> MidiInst.cmd (CUtil.drum_cmd note_keys)
    note_keys = map (first head) mridangam_notes

mridangam_notes :: [([Drums.Note], Midi.Key)]
mridangam_notes = map mk mridangam
    where
    mk (name, attrs, char, (_, mid, _)) = (notes, mid)
        where
        notes = [Drums.Note name attr char 1 | attr <- attrs]

mridangam_keymap :: Instrument.AttributeMap
mridangam_keymap = Instrument.keymap
    [ (attr, Instrument.PitchedKeymap low high NN.e3)
    | (_, attrs, _, (low, _, high)) <- mridangam
    , attr <- attrs
    ]

mridangam :: [(Text, [Score.Attributes], Char, (Midi.Key, Midi.Key, Midi.Key))]
mridangam =
    -- left
    [ ("+", [a "tha"],          'z', (Key.g_1, Key.c0, Key.e0))
    , ("o", [a "thom"],         'x', (Key.g0, Key.c1, Key.e1))
    , ("o/", [a "thom" <> mute],'c', (Key.g1, Key.c2, Key.e2))
    -- right
    , ("k", [a "ki"],           'q', (Key.g2, Key.c3, Key.e3))
    , ("t", [a "ta"],           'w', (Key.g3, Key.c4, Key.e4))
    , ("n", [a "nam"],          'e', (Key.g4, Key.c5, Key.e5))
    , ("d", [a "din"],          'r', (Key.g5, Key.c6, Key.e6))
    , ("d2", [a "din" <> v2],   't', (Key.g6, Key.c7, Key.e7))
    , ("m", [a "dheem"],        'y', (Key.g7, Key.c8, Key.e8))
    , ("u", [a "araichapu"],    'u', (Key.g8, Key.c9, Key.e9))
    , (">", [a "muruchapu"],    'i', (Key.g9, Key.g9, Key.g9))
    -- TODO out of keys!  Replace o/ and d2 with a keyswitch?
    ]
    where a = Score.attr
