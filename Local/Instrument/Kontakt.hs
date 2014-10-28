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

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Highlight as Highlight
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import Derive.Score (attr)
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
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
misc_patches = concat
    [ library, mcgill, balalaika, anthology_wind, sonic_couture, sc_bali, misc
    ]

library :: [MidiInst.Patch]
library = map MidiInst.with_empty_code
    [ patch "choir" [(1, "vowel")]
    ]

-- | From the McGill sample library.
mcgill :: [MidiInst.Patch]
mcgill = map MidiInst.with_empty_code
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
    [ with_code $
        Instrument.attribute_map #= Instrument.simple_keyswitches ks $
        Instrument.patch $ (Instrument.hold_keyswitch #= True) $
        Instrument.instrument "balalaika" controls pb_range
    ]
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

-- | Bela D Anthology Spiritual Wind
-- Change volume to cc 2.
-- Change b3 and c3 to be normal keyswitches instead of toggles.
anthology_wind :: [MidiInst.Patch]
anthology_wind = map MidiInst.with_empty_code
    [ MidiInst.pressure $
        Instrument.attribute_map #= Instrument.simple_keyswitches dizi_ks $
        patch "dizi" [(CC.mod, Controls.vib)]
    ]
    where
    -- blow and overblow as keyswitches instead of on/off
    dizi_ks =
        [ (mempty, Key2.c2)
        , (ornament <> Attrs.v1, Key2.cs2)
        , (Attrs.staccato, Key2.d2)
        , (ornament <> Attrs.v2, Key2.ds2)
        , (Attrs.staccato <> blow, Key.e2) -- unpitched attack
        , (ornament <> Attrs.v3, Key2.fs2)
        , (ornament <> Attrs.v4, Key2.gs2)
        , (ornament <> Attrs.long <> Attrs.v1, Key2.as2)
        , (blow, Key2.b3) -- sustain with sharp attack
        , (Attrs.accent, Key2.c3) -- like 'blow', but softer attack
        , (ornament <> Attrs.long <> Attrs.v2, Key2.cs3)
        ]
    -- f2 slide 1 up / down
    -- g2 slide 2 up / down
    -- a2 slide 2 down
    ornament = Score.attr "o"
    blow = Score.attr "blow"

-- * sonic couture

sonic_couture :: [MidiInst.Patch]
sonic_couture =
    [ MidiInst.with_empty_code $ patch "ebow"
        [(1, "harm"), (21, Controls.lpf), (22, Controls.q), (23, Controls.hpf)]
    , guzheng
    ]

guzheng :: MidiInst.Patch
guzheng = MidiInst.with_code code $ MidiInst.range bottom top $
    Instrument.instrument_#Instrument.maybe_decay #= Just 5 $
    Instrument.attribute_map #= Instrument.simple_keyswitches ks $
    patch "guzheng" [(23, Controls.lpf), (24, Controls.q),
        (27, Controls.hpf)]
    where
    code = MidiInst.note_generators [("左", DUtil.attrs_note Attrs.left)]
        <> MidiInst.note_transformers [("standard-strings", standard_strings)]
        <> MidiInst.note_calls (MidiInst.null_call c_highlight_strings)
    standard_strings = DUtil.make_call0t "standard-strings"
        ("Set " <> ShowVal.doc_val Environ.open_strings
            <> " to standard pitches: " <> ShowVal.show_val strings)
        $ \_ deriver -> Derive.with_val Environ.open_strings
            (map Pitches.nn_pitch strings) deriver
    ks =
        [ (Attrs.harm, Key2.as5)
        , (Attrs.left, Key2.b5) -- left hand, no pick
        , (mempty, Key2.c6) -- right hand, picked
        ]
    strings = take (4*5 + 1) $ -- 4 octaves + 1, so D to D
        concat $ zipWith (\nns oct -> map (oct+) nns) (repeat notes) octaves
        where
        notes = [NN.d2, NN.e2, NN.fs2, NN.a2, NN.b2]
        octaves = map fromIntegral [0, 12 ..]
    -- Let's say the top string can bend a minor third.
    (bottom, top) = (head strings, last strings + 3)

c_highlight_strings :: Derive.Generator Derive.Note
c_highlight_strings = Note.transformed_note
    ("Highlight any notes whose initial pitch either is or isn't in "
    <> ShowVal.doc_val Environ.open_strings <> ".") mempty $ \args deriver -> do
        start <- Args.real_start args
        Highlight.out_of_range $
            Highlight.open_strings start Highlight.warn_non_open deriver

sc_bali :: [MidiInst.Patch]
sc_bali =
    [ MidiInst.with_empty_code $ with_doc $
        Instrument.attribute_map #= Instrument.simple_keyswitches gangsa_ks $
        patch "sc-gangsa12"
    , CUtil.simple_drum Nothing gong_notes $ with_doc $ patch "sc-gong"
    ]
    where
    patch name = Instrument.patch $ Instrument.instrument name [] (-2, 2)
    with_doc = Instrument.text #= "Sonic Couture's Balinese gamelan sample set."
    gangsa_ks = [(Attrs.mute, Key2.cs1), (mempty, Key2.c1)]
    gong_notes = map make
        [ ("O", gong <> wadon, Key2.b1, 'z')
        , ("o", gong <> lanang, Key2.c2, 'x')
        , ("p", kempur, Key2.a2, 'q')
        , ("m", kemong, Key2.a3, 'w')
        ]
        where make (call, attrs, key, char) = (Drums.note call attrs char, key)

gong = Score.attr "gong"
kemong = Score.attr "kemong"
kempur = Score.attr "kempur"
wadon = Score.attr "wadon"
lanang = Score.attr "lanang"

misc :: [MidiInst.Patch]
misc = [MidiInst.with_code Reaktor.resonant_filter $ patch "filtered" []]

-- * hang

hang_patches :: [MidiInst.Patch]
hang_patches = map (MidiInst.with_code hang_code)
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
    [ (Attrs.center,  Key.c2,     Just "",   Just 'Z')
    , (Attrs.edge,    Key.cs2,    Just "旁", Just 'X')
    , (Attrs.slap,    Key.d2,     Just "打", Just 'C')
    , (Attrs.middle,  Key.ds2,    Just "中", Just 'V')
    , (Attrs.knuckle, Key.e2,     Just "指", Just 'B')
    , (mempty,        Key.c2,     Nothing,   Nothing)
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
wayang_patches = map (MidiInst.with_code (code <> with_weak))
    [ set_tuning Environ.umbang $ scale Wayang.umbang $ wayang "wayang-umbang"
    , set_tuning Environ.isep $ scale Wayang.isep $ wayang "wayang-isep"
    , Instrument.text #= "Tuned to 12TET." $ wayang "wayang12"
    ] ++ map (MidiInst.with_code (Bali.pasang_code <> with_weak))
    [ wayang "wayang"
    , set_range Wayang.pemade_bottom Wayang.pemade_top $ wayang "wayang-p"
    , set_range Wayang.kantilan_bottom Wayang.kantilan_top $ wayang "wayang-k"
    ]
    where
    code = MidiInst.postproc (Gangsa.mute_postproc (Attrs.mute <> Attrs.loose))
    with_weak = MidiInst.note_calls null_call
    null_call = MidiInst.null_call $ DUtil.zero_duration "note"
        "This a normal note with non-zero duration, but when the duration is\
        \ zero, it uses the `weak` call."
        (Sub.inverting weak_call)
        (Sub.inverting $ Note.default_note Note.use_attributes)
    weak_call args =
        Gender.weak (Sig.control "strength" 0.5) (Args.set_duration dur args)
        where dur = Args.next args - Args.start args
    wayang = Instrument.set_flag Instrument.ConstantPitch
        . (Instrument.instrument_#Instrument.maybe_decay #= Just 0)
        . (Instrument.attribute_map #= wayang_keymap) . flip patch []
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
    LInst.add "p" "kontakt/wayang-p" "" []
    LInst.add_environ "p" Gangsa.inst_polos (Score.Instrument "p-umbang")
    LInst.add_environ "p" Gangsa.inst_sangsih (Score.Instrument "p-isep")
    LInst.add "k" "kontakt/wayang-k" "" []
    LInst.add_environ "k" Gangsa.inst_polos (Score.Instrument "k-umbang")
    LInst.add_environ "k" Gangsa.inst_sangsih (Score.Instrument "k-isep")
    LInst.add "p-isep" "kontakt/wayang-isep" dev [0]
    LInst.add "p-umbang" "kontakt/wayang-umbang" dev [1]
    LInst.add "k-isep" "kontakt/wayang-isep" dev [2]
    LInst.add "k-umbang" "kontakt/wayang-umbang" dev [3]

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
        [ CUtil.drum_calls Nothing mridangam_keymap
        , DUtil.multiple_calls
            [(call, subcalls) | (call, subcalls, _) <- mridangam_both]
        ]
    char_to_call = Map.fromList $ concat
        [ [(Drums.note_char n, Drums.note_name n) | n <- mridangam_keymap]
        , [(char, call) | (call, _, Just char) <- mridangam_both]
        ]

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
mridangam_both :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
mridangam_both =
    [(call, subcalls, lookup call keys) | (call, subcalls) <- pairs]
    where
    keys = [("do", 'c'), ("ko", 'v'), ("to", 'b')]
    pairs =
        [ (TrackLang.Symbol $ u rcall <> u lcall, [rcall, lcall])
        | lcall <- map Drums.note_name mridangam_left
        , rcall <- map Drums.note_name mridangam_right
        , Text.length (u lcall) == 1
        , Text.length (u rcall) == 1
        ]
    u = TrackLang.unsym

mridangam_keymap, mridangam_left, mridangam_right :: [Drums.Note]
mridangam_left = map (\(c, n, a, d) -> Drums.Note n a c d)
    [ ('z', "+", tha, 1)
    , ('x', "o", thom, 1)
    , ('s', ".", thom, 0.5)
    ]
mridangam_right = map (\(c, n, a, d) -> Drums.Note n a c d)
    [ ('q', "k", ki, 1)
    , ('w', "t", ta, 1)
    , ('e', "n", nam, 1)
    , ('r', "d", din, 1)
    , ('5', "v", muru, 1)
    , ('t', "u", arai, 1)
    , ('y', "m", dheem, 1)
    ]
mridangam_keymap = mridangam_left ++ mridangam_right


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

make_mridangam_notes :: [(Score.Attributes, CUtil.KeyswitchRange)]
    -> CUtil.PitchedNotes
make_mridangam_notes keymap = do
    note <- mridangam_keymap
    let Just ks_range = lookup (Drums.note_attrs note) keymap
    return (note, ks_range)

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
