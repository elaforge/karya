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
import qualified Midi.CC as CC
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Keymap as Keymap

import qualified Derive.Attrs as Attrs
import Derive.Attrs
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Environ as Environ
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch

import qualified Local.Instrument.KontaktKendang as KontaktKendang
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
    , hang_patches, wayang_patches, KontaktKendang.patches
    , mridangam_patches, mridangam2_patches
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
        [ MidiInst.both call (Make.attributed_note attrs)
        | (attrs, _, Just call, _) <- hang_strokes
        -- Make sure to not shadow the default "" call.
        , call /= ""
        ]
    <> MidiInst.cmd hang_cmd

hang_cmd :: Cmd.Cmd
hang_cmd = CUtil.keyswitches [(Keymap.physical_key char, text, key)
    | (_, key, Just text, Just char) <- hang_strokes]

-- | The order is important because it determines attr lookup priority.
hang_strokes :: [(Score.Attributes, Midi.Key, Maybe TrackLang.CallId,
    Maybe Char)]
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

    TODO if I want to support both +mute and +mute+loose, perhaps null_call
    should add just +mute, and can inherit +loose if it's set.
-}
wayang_patches :: [MidiInst.Patch]
wayang_patches =
    [ (set_tuning Environ.umbang $ scale Wayang.umbang $ wayang "wayang-umbang",
        wayang_code)
    , (set_tuning Environ.isep $ scale Wayang.isep $ wayang "wayang-isep",
        wayang_code)
    , (wayang "wayang", Bali.pasang_code)
    , (set_range Wayang.pemade_bottom Wayang.pemade_top $ wayang "wayang-p",
        Bali.pasang_code)
    , (set_range Wayang.kantilan_bottom Wayang.kantilan_top $ wayang "wayang-k",
        Bali.pasang_code)
    , (Instrument.text #= "Tuned to 12TET." $ wayang "wayang12", wayang_code)
    ]
    where
    wayang = (Instrument.attribute_map #= wayang_keymap) . patch
    scale scale = (Instrument.text #= scale_doc)
        . (Instrument.scale #= wayang_scale scale)
    scale_doc = "These set the scale and tuning automatically, and expect the\
        \ patch to be tuned to the instrument's natural scale."
    set_tuning tuning = MidiInst.default_scale Wayang.scale_id
        . MidiInst.environ Environ.tuning tuning
    set_range bottom top = MidiInst.environ Environ.instrument_bottom bottom
        . MidiInst.environ Environ.instrument_top top

wayang_code :: MidiInst.Code
wayang_code = MidiInst.note_calls $ MidiInst.null_call $
    DUtil.zero_duration "Add `+mute+loose` attribute and scale dyn by 0.75." $
        Util.add_attrs (Attrs.mute <> Attrs.loose) . Util.multiply_dynamic 0.75

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


-- ** util

-- do the cmd keybindings in the same way as mridangam and move utils to
-- to Cmd.Instrument.Drums
drum_attributes :: Midi.Key -> Midi.Key -> Midi.Key -> [[Score.Attributes]]
    -> [(Attributes, (Midi.Key, Midi.Key, Midi.Key))]
drum_attributes base_keyswitch base_key range = assign
    where
    assign groups =
        [ (attrs, (ks, low, low + (range-1)))
        | (group, low) <- zip groups [base_key, base_key+range ..]
        , (attrs, ks) <- zip group keyswitches
        ]
    keyswitches = [base_keyswitch ..]

make_attribute_map :: [(Attributes, (Midi.Key, Midi.Key, Midi.Key))]
    -> Instrument.AttributeMap
make_attribute_map attr_map = Instrument.make_attribute_map
    [ (attrs, [Instrument.Keyswitch ks],
        Just (Instrument.PitchedKeymap low high NN.gs3))
    | (attrs, (ks, low, high)) <- attr_map
    ]

set_attribute_map :: [(Attributes, (Midi.Key, Midi.Key, Midi.Key))]
    -> Instrument.Patch -> Instrument.Patch
set_attribute_map attr_map =
    Instrument.triggered
    . (Instrument.attribute_map #= make_attribute_map attr_map)


-- * mridangam

-- ** 1

mridangam_patches :: [MidiInst.Patch]
mridangam_patches = [(inst, code)]
    where
    inst = Instrument.triggered $
        Instrument.attribute_map #= mridangam_keymap $
        Instrument.patch $ Instrument.instrument "mridangam" [] pb_range
    code = MidiInst.note_generators call_code
        <> MidiInst.cmd (CUtil.insert_call char_to_call)
    call_code = concat
        [ CUtil.drum_calls
            [Drums.Note call attrs char 1 | (call, attrs, char, _) <- mridangam]
        , DUtil.multiple_calls
            [(call, subcalls) | (call, subcalls, _) <- mridangam_both]
        , DUtil.double_calls
            [(call, subcall) | (call, subcall, _) <- mridangam_double]
        ]
    char_to_call = Map.fromList $ concat
        [ [(c, call) | (call, _, c, _) <- mridangam]
        , [(c, call) | (call, _, Just c) <- mridangam_both]
        , [(c, call) | (call, _, Just c) <- mridangam_double]
        ]

mridangam_keymap :: Instrument.AttributeMap
mridangam_keymap = Instrument.keymap
    [ (attrs, Instrument.PitchedKeymap low high NN.e3)
    | (_, attrs, _, (low, high)) <- mridangam
    ]

mridangam_both :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
mridangam_both =
    [(call, subcalls, lookup call keys) | (call, subcalls) <- pairs]
    where
    keys = [("do", 'c'), ("ko", 'v'), ("to", 'b')]
    pairs =
        [ (TrackLang.Symbol $ u rcall <> u lcall, [rcall, lcall])
        | (lcall, _, _, _) <- mridangam_left
        , (rcall, _, _, _) <- mridangam_right
        , Text.length (u lcall) == 1
        , Text.length (u rcall) == 1
        ]
    u = TrackLang.unsym

mridangam_double :: [(TrackLang.CallId, TrackLang.CallId, Maybe Char)]
mridangam_double =
    [ (TrackLang.Symbol $ u call <> u call, call, lookup call keys)
    | (call, _, _, _) <- mridangam
    , Text.length (u call) == 1
    ]
    where
    keys =
        [ ("+", 'a'), ("o", 'd')
        , ("k", '1'), ("n", '3'), ("d", '4')
        ]
    u = TrackLang.unsym

mridangam, mridangam_left, mridangam_right
    :: [(TrackLang.CallId, Score.Attributes, Char, (Midi.Key, Midi.Key))]
mridangam = mridangam_left ++ mridangam_right
mridangam_left =
    [ ("+", a "tha",          'z', (Key.g_1, Key.e0))
    , ("o", a "thom",         'x', (Key.g0, Key.e1))
    , ("o/", a "thom" <> mute,'s', (Key.g1, Key.e2))
    ] where a = Score.attr
mridangam_right =
    [ ("k", a "ki",           'q', (Key.g2, Key.e3))
    , ("t", a "ta",           'w', (Key.g3, Key.e4))
    , ("n", a "nam",          'e', (Key.g4, Key.e5))
    , ("d", a "din",          'r', (Key.g5, Key.e6))
    , ("d2", a "din" <> v2,   '4', (Key.g6, Key.e7))
    , ("m", a "dheem",        't', (Key.g7, Key.e8))
    , ("u", a "araichapu",    'y', (Key.g8, Key.e9))
    , ("v", a "muruchapu",    'u', (Key.g9, Key.g9))
    -- TODO out of keys!  Replace o/ and d2 with a keyswitch?
    ] where a = Score.attr

-- ** 2

-- | Alternate keymap.
mridangam2_patches :: [MidiInst.Patch]
mridangam2_patches = [(inst, code)]
    where
    inst = Instrument.triggered $
        Instrument.attribute_map #= make_attribute_map mridangam2_attributes $
        Instrument.patch $ Instrument.instrument "mridangam2" [] pb_range
    code = MidiInst.note_generators call_code
        <> MidiInst.cmd (CUtil.insert_call char_to_call)
    call_code = concat
        [ CUtil.drum_calls
            [Drums.Note call attrs char 1 | (char, call, attrs) <- mridangam2]
        , DUtil.multiple_calls
            [(call, subcalls) | (call, subcalls, _) <- mridangam2_both]
        , DUtil.double_calls
            [(call, subcall) | (call, subcall, _) <- mridangam2_double]
        ]
    char_to_call = Map.fromList $ concat
        [ [(char, call) | (char, call, _) <- mridangam2]
        , [(char, call) | (call, _, Just char) <- mridangam2_both]
        , [(char, call) | (call, _, Just char) <- mridangam2_double]
        ]

mridangam2_double :: [(TrackLang.CallId, TrackLang.CallId, Maybe Char)]
mridangam2_double =
    [ (TrackLang.Symbol $ u call <> u call, call, lookup call keys)
    | (_, call, _) <- mridangam2
    , Text.length (u call) == 1
    ]
    where
    keys =
        [ ("+", 'a'), ("o", 'd')
        , ("k", '1'), ("n", '3'), ("d", '4')
        ]
    u = TrackLang.unsym

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
mridangam2_both :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
mridangam2_both =
    [(call, subcalls, lookup call keys) | (call, subcalls) <- pairs]
    where
    keys = [("do", 'c'), ("ko", 'v'), ("to", 'b')]
    pairs =
        [ (TrackLang.Symbol $ u rcall <> u lcall, [rcall, lcall])
        | (_, lcall, _) <- mridangam2_left
        , (_, rcall, _) <- mridangam2_right
        , Text.length (u lcall) == 1
        , Text.length (u rcall) == 1
        ]
    u = TrackLang.unsym

mridangam2, mridangam2_left, mridangam2_right
    :: [(Char, TrackLang.CallId, Attributes)]
mridangam2_left = map (\(a, b, c) -> (a, b, Score.attr c))
    [ ('z', "+", "tha")
    , ('x', "o", "thom")
    ]
mridangam2_right = map (\(a, b, c) -> (a, b, Score.attr c))
    [ ('q', "k", "ki")
    , ('w', "t", "ta")
    , ('e', "n", "nam")
    , ('r', "d", "din")
    , ('t', "m", "dheem")
    , ('y', "u", "arai")
    , ('u', "v", "muru")
    ]
mridangam2 = mridangam2_left ++ mridangam2_right

mridangam2_attributes :: [(Attributes, (Midi.Key, Midi.Key, Midi.Key))]
mridangam2_attributes = drum_attributes Key2.g_2 Key2.c_1 12
    [ [a "tha"]
    , [a "thom", a "thom" <> low, a "thom" <> open]
    , [a "ta"], [a "ki"], [a "nam"], [a "din"]
    , [a "arai", a "muru"]
    , [a "dheem", a "dheem" <> staccato]
    , [a "meetu"]
    ]
    where a = Score.attr
