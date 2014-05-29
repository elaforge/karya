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
import qualified Cmd.Repl.LInst as LInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Bali.Kotekan as Kotekan
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import Derive.Score (attr)
import qualified Derive.ShowVal as ShowVal
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
synth = "kontakt"

patches :: [MidiInst.Patch]
patches = concat
    [ misc_patches
    , hang_patches, wayang_patches, KontaktKendang.patches
    , mridangam_patches
    ]

patch :: Instrument.InstrumentName -> [(Midi.Control, Score.Control)]
    -> Instrument.Patch
patch name controls =
    Instrument.patch $ Instrument.instrument name controls pb_range

-- One pitch bend modulator can only do +-12, but if you put two on you get
-- +-24.
pb_range :: Instrument.PbRange
pb_range = (-24, 24)

-- * misc

misc_patches :: [MidiInst.Patch]
misc_patches = concat [library, mcgill, balalaika, sonic_couture, sc_bali, misc]

library :: [MidiInst.Patch]
library = MidiInst.with_empty_code
    [ inst "choir" [ (1, "vowel") ]
    ]
    where
    inst name controls = Instrument.patch $
        Instrument.instrument name controls pb_range

-- | From the McGill sample library.
mcgill :: [MidiInst.Patch]
mcgill = MidiInst.with_empty_code
    [ pressure "viol", pressure "shawm", pressure "crumhorn"
    , plucked "lute"
    ]
    where
    plucked name = patch name []
    pressure name = MidiInst.pressure $
        patch name [(CC.cc14, Controls.lpf), (CC.cc15, Controls.q)]

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
sonic_couture = concat $
    [ MidiInst.with_empty_code
        [ patch "ebow" [(1, "harm"), (21, Controls.lpf), (22, Controls.q),
            (23, Controls.hpf)]
        ]
    , guzheng
    ]

guzheng :: [MidiInst.Patch]
guzheng = MidiInst.with_code code
    [ Instrument.instrument_#Instrument.maybe_decay #= Just 5 $
        Instrument.attribute_map #= Instrument.simple_keyswitches guzheng_ks $
        patch "guzheng" [(23, Controls.lpf), (24, Controls.q),
            (27, Controls.hpf)]
    ]
    where
    code = MidiInst.note_generators [("左", DUtil.attrs_note Attrs.left)]
        <> MidiInst.note_transformers [("standard-strings", standard_strings)]
    standard_strings = DUtil.make_call0t "standard-strings"
        ("Set `open-strings` to standard pitches: " <> ShowVal.show_val strings)
        $ \_ deriver -> do
            pitches <- mapM (Eval.eval_pitch 0) (map TrackLang.call0 strings)
            Derive.with_val "open-strings" pitches deriver
    guzheng_ks =
        [ (Attrs.harm, Key2.as5)
        , (Attrs.left, Key2.b5) -- left hand, no pick
        , (mempty, Key2.c6) -- right hand, picked
        ]
    strings = take (4*5 + 1) -- 4 octaves + 1, so D to D
        [TrackLang.Symbol $ showt oct <> note | oct <- [2..],
            note <- ["d", "e", "f#", "a", "b"]]

sc_bali :: [MidiInst.Patch]
sc_bali = MidiInst.with_code mute_null_call
    [ Instrument.text #= "Sonic Couture's Balinese gamelan sample set." $
        Instrument.attribute_map #= Instrument.simple_keyswitches gangsa_ks $
        Instrument.patch $ Instrument.instrument "sc-gangsa12" [] (-2, 2)
    ]
    where gangsa_ks = [(Attrs.mute, Key2.cs1), (mempty, Key2.c1)]

misc :: [MidiInst.Patch]
misc = MidiInst.with_code Reaktor.resonant_filter [patch "filtered" []]

-- * hang

hang_patches :: [MidiInst.Patch]
hang_patches = MidiInst.with_code hang_code
    [ Instrument.attribute_map #= Instrument.simple_keyswitches hang_ks $
        patch "hang" []
    ]

hang_code :: MidiInst.Code
hang_code =
    MidiInst.note_calls
        [ MidiInst.both call (Make.attributed_note Module.instrument attrs)
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
    [ (Attrs.center,  Key.c2,     Just "",            Just 'Z')
    , (Attrs.edge,    Key.cs2,    Just "`pang2`",     Just 'X')
    , (Attrs.slap,    Key.d2,     Just "`da3`",       Just 'C')
    , (Attrs.middle,  Key.ds2,    Just "`zhong1`",    Just 'V')
    , (Attrs.knuckle, Key.e2,     Just "`zhi3`",      Just 'B')
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
wayang_patches = MidiInst.with_code mute_null_call
    [ set_tuning Environ.umbang $ scale Wayang.umbang $ wayang "wayang-umbang"
    , set_tuning Environ.isep $ scale Wayang.isep $ wayang "wayang-isep"
    , Instrument.text #= "Tuned to 12TET." $ wayang "wayang12"
    ] ++ MidiInst.with_code Bali.pasang_code
    [ wayang "wayang"
    , set_range Wayang.pemade_bottom Wayang.pemade_top $ wayang "wayang-p"
    , set_range Wayang.kantilan_bottom Wayang.kantilan_top $ wayang "wayang-k"
    ]
    where
    wayang = (Instrument.attribute_map #= wayang_keymap) . flip patch []
    scale scale = (Instrument.text #= scale_doc)
        . (Instrument.scale #= wayang_scale scale)
    scale_doc = "These set the scale and tuning automatically, and expect the\
        \ patch to be tuned to the instrument's natural scale."
    set_tuning tuning = MidiInst.default_scale Wayang.scale_id
        . MidiInst.environ Environ.tuning tuning
    set_range bottom top = MidiInst.environ Environ.instrument_bottom bottom
        . MidiInst.environ Environ.instrument_top top

-- | Set up a gender wayang quartet.
--
-- There are two pasang instruments, which then rely on the kotekan calls to
-- split into inst-polos and inst-sangsih.  This uses the traditional setup
-- with polos on umbang.
configure_wayang :: Text -> Cmd.CmdL ()
configure_wayang dev = do
    LInst.create "p" "kontakt/wayang-p" "" []
    LInst.add_environ "p" Kotekan.inst_polos (Score.Instrument "p-umbang")
    LInst.add_environ "p" Kotekan.inst_sangsih (Score.Instrument "p-isep")
    LInst.create "k" "kontakt/wayang-k" "" []
    LInst.add_environ "k" Kotekan.inst_polos (Score.Instrument "k-umbang")
    LInst.add_environ "k" Kotekan.inst_sangsih (Score.Instrument "k-isep")
    LInst.create "p-isep" "kontakt/wayang-isep" dev [0]
    LInst.create "p-umbang" "kontakt/wayang-umbang" dev [1]
    LInst.create "k-isep" "kontakt/wayang-isep" dev [2]
    LInst.create "k-umbang" "kontakt/wayang-umbang" dev [3]

mute_null_call :: MidiInst.Code
mute_null_call = MidiInst.note_calls $ MidiInst.null_call $
    DUtil.zero_duration "Add `+mute` attribute and scale dyn by 0.75." $
        Util.add_attrs Attrs.mute . Util.multiply_dynamic 0.75

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


-- * mridangam

mridangam_patches :: [MidiInst.Patch]
mridangam_patches =
    [ (patch "mridangam2" mridangam_notes, code)
    , (patch "mridangam" old_mridangam_notes, code)
    ]
    where
    patch name notes = CUtil.pitched_drum_patch notes $
        Instrument.patch $ Instrument.instrument name [] pb_range
    code = MidiInst.note_generators call_code
        <> MidiInst.cmd (CUtil.insert_call char_to_call)
    call_code = concat
        [ CUtil.drum_calls Nothing
            [Drums.Note call attrs char 1
                | (char, call, attrs) <- mridangam_keymap]
        , DUtil.multiple_calls
            [(call, subcalls) | (call, subcalls, _) <- mridangam_both]
        , DUtil.double_calls
            [(call, subcall) | (call, subcall, _) <- mridangam_double]
        ]
    char_to_call = Map.fromList $ concat
        [ [(char, call) | (char, call, _) <- mridangam_keymap]
        , [(char, call) | (call, _, Just char) <- mridangam_both]
        , [(char, call) | (call, _, Just char) <- mridangam_double]
        ]

mridangam_double :: [(TrackLang.CallId, TrackLang.CallId, Maybe Char)]
mridangam_double =
    [ (TrackLang.Symbol $ u call <> u call, call, lookup call keys)
    | (_, call, _) <- mridangam_keymap
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
mridangam_both :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
mridangam_both =
    [(call, subcalls, lookup call keys) | (call, subcalls) <- pairs]
    where
    keys = [("do", 'c'), ("ko", 'v'), ("to", 'b')]
    pairs =
        [ (TrackLang.Symbol $ u rcall <> u lcall, [rcall, lcall])
        | (_, lcall, _) <- mridangam_left
        , (_, rcall, _) <- mridangam_right
        , Text.length (u lcall) == 1
        , Text.length (u rcall) == 1
        ]
    u = TrackLang.unsym

mridangam_keymap, mridangam_left, mridangam_right
    :: [(Char, TrackLang.CallId, Score.Attributes)]
mridangam_left =
    [ ('z', "+", tha)
    , ('x', "o", thom)
    ]
mridangam_right =
    [ ('q', "k", ki)
    , ('w', "t", ta)
    , ('e', "n", nam)
    , ('r', "d", din)
    , ('t', "m", dheem)
    , ('y', "u", arai)
    , ('u', "v", muru)
    ]
mridangam_keymap = mridangam_left ++ mridangam_right

make_mridangam_notes :: [(Score.Attributes, CUtil.KeyswitchRange)]
    -> CUtil.PitchedNotes
make_mridangam_notes keymap = do
    (char, call, attrs) <- mridangam_keymap
    let Just ks_range = lookup attrs keymap
    let note = Drums.Note call attrs char 1
    return (note, ks_range)

{- | Layout:

    > 0         10        20        30        40        50        60        70        80        90        100       110       120    127
    > 01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    > c-2         c-1         c0          c1          c2          c3          c4          c5          c6          c7          c8     g8
    >         XXXXtha -------|thom ------|ta --------|ki --------|nam -------|din -------|arai ------|dheem -----|meetu -----|
-}
mridangam_notes :: CUtil.PitchedNotes
mridangam_notes = make_mridangam_notes $
    CUtil.make_keymap Key2.g_2 Key2.c_1 12 NN.c4
    [ [tha]
    , [thom, thom <> Attrs.low, thom <> Attrs.open]
    , [ta], [ki], [nam], [din]
    , [arai, muru]
    , [dheem, dheem <> Attrs.staccato]
    , [meetu]
    ]

old_mridangam_notes :: CUtil.PitchedNotes
old_mridangam_notes = make_mridangam_notes $ map make
    -- left
    [ (tha, (Key.g_1, Key.e0))
    , (thom, (Key.g0, Key.e1))
    , (thom <> Attrs.staccato, (Key.g1, Key.e2))
    -- right
    , (ki, (Key.g2, Key.e3))
    , (ta, (Key.g3, Key.e4))
    , (nam, (Key.g4, Key.e5))
    , (din, (Key.g5, Key.e6))
    , (din <> Attrs.v2, (Key.g6, Key.e7))
    , (dheem, (Key.g7, Key.e8))
    , (arai, (Key.g8, Key.e9))
    , (muru, (Key.g9, Key.g9))
    ]
    where make (attrs, (low, high)) = (attrs, (Nothing, low, high, NN.e3))

-- * attrs

tha = attr "tha"
thom = attr "thom"
ki = attr "ki"
ta = attr "ta"
nam = attr "nam"
din = attr "din"
dheem = attr "dheem"
arai = attr "arai"
muru = attr "muru"
meetu = attr "meetu"
