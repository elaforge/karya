-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Midi.CC as CC
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiConfig as MidiConfig
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Keymap as Keymap
import qualified Cmd.Repl.Util as Repl.Util

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
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
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
import qualified Local.Instrument.KontaktUtil as KontaktUtil
import qualified Local.Instrument.Reaktor as Reaktor

import Global


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
guzheng = MidiInst.with_code code $ MidiInst.range range $
    Instrument.instrument_#Instrument.maybe_decay #= Just 5 $
    Instrument.attribute_map #= Instrument.simple_keyswitches ks $
    patch "guzheng" [(23, Controls.lpf), (24, Controls.q), (27, Controls.hpf)]
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
    range = (head strings, last strings + 3)

c_highlight_strings :: Derive.Generator Derive.Note
c_highlight_strings = Note.transformed_note
    ("Highlight any notes whose initial pitch either is or isn't in "
    <> ShowVal.doc_val Environ.open_strings <> ".") mempty $ \args deriver -> do
        start <- Args.real_start args
        Highlight.out_of_range $
            Highlight.open_strings start Highlight.warn_non_open deriver

sc_bali :: [MidiInst.Patch]
sc_bali = map (first add_doc) $
    CUtil.simple_drum Nothing gong_notes (sc_patch "gong")
    : CUtil.simple_drum Nothing kempli_kajar_notes (sc_patch "kempli")
    : concat
    [ gangsa (range_of Legong.jegog) "jegog"
    , gangsa (range_of Legong.calung) "calung"
    , gangsa (range_of Legong.penyacah) "penyacah"
    , gangsa Legong.ugal_range "ugal"
    , gangsa (range_of Legong.pemade) "pemade"
    , gangsa (range_of Legong.kantilan) "kantilan"
    ] ++ map MidiInst.with_empty_code
    [ reyong_ks $ ranged_patch Legong.reyong_range "reyong"
    , ranged_patch Legong.trompong_range "trompong"
    ]
    where
    gangsa range name =
        [ MidiInst.with_code Bali.pasang_code $
            ranged_patch range (name <> "-pasang")
        , MidiInst.with_empty_code $ gangsa_ks $ ranged_patch range name
        ]
    range_of = BaliScales.scale_range
    ranged_patch range = MidiInst.range range . sc_patch
    sc_patch name = Instrument.set_flag Instrument.ConstantPitch $
        Instrument.patch $ Instrument.instrument ("sc-" <> name) [] (-2, 2)
    add_doc = Instrument.text
        %= ("Sonic Couture's Balinese gamelan sample set. " <>)
    gangsa_ks = Instrument.attribute_map #= Instrument.simple_keyswitches
        [(Attrs.mute, Key2.cs1), (mempty, Key2.c1)]
    reyong_ks = Instrument.attribute_map #= Instrument.simple_keyswitches
        [(Score.attr "cek", Key2.cs1), (mempty, Key2.c1)]
    gong_notes =
        [ (n 'z' "O" (gong <> wadon),   Key2.b1)
        , (n 'x' "o" (gong <> lanang),  Key2.c2)
        , (n 'q' "p" kempur,            Key2.a2)
        , (n 'w' "m" kemong,            Key2.a3)
        ]
        where n = Drums.note
    kempli_kajar_notes =
        [ (n 'z' "+"    kempli,                 Key2.d3)
        , (n 'a' "`O+`" (kempli <> open),       Key2.ds3)
        , (n 'x' "+1"   (kempli <> Attrs.v1),   Key2.f3)
        , (n 'c' "+2"   (kempli <> Attrs.v2),   Key2.g3)
        , (n 'v' "+3"   (kempli <> Attrs.v3),   Key2.a3)
        , (n 'b' "b"    bebende,                Key2.d4)
        , (n 'g' "B"    (bebende <> open),      Key2.ds4)
        -- TODO make sure these names are the same as the corresponding kendang
        , (n 'q' "o"    kajar,                  Key2.f4)
        , (n 'w' "T"    (kajar <> Attrs.rim <> open), Key2.fs4)
        -- The Sonic Couture kajar doesn't have this.
        , (n 'e' "P"    (kajar <> Attrs.rim),   Key2.g4)
        -- Soniccouture also has a low kajar variant.
        ]
        where n = Drums.note
    open = Attrs.open

gong = Score.attr "gong"
kemong = Score.attr "kemong"
kempur = Score.attr "kempur"
bebende = Score.attr "bebende"
wadon = Score.attr "wadon"
lanang = Score.attr "lanang"
kempli = Score.attr "kempli"
kajar = Score.attr "kajar"

misc :: [MidiInst.Patch]
misc = [MidiInst.with_code Reaktor.resonant_filter $ patch "filtered" []]

config_kebyar :: Text -> MidiConfig.Config
config_kebyar dev_ = make_config $ concat
    [ pasang "jegog"
    , pasang "calung"
    , pasang "penyacah"
    , pasang "pemade"
    , pasang "kantilan"
    , [ umbang_patch "ugal" "ugal"
      , isep_patch "reyong" "reyong"
      , umbang_patch "trompong" "trompong"
      , patch "gong"
      , patch "kempli"
      ]
    ]
    where
    -- Expects (name, patch, gets_chan, environ, scale).
    make_config = MidiConfig.config . snd . List.mapAccumL allocate 0
        where
        allocate chan (alias, inst, gets_chan, environ, scale) =
            (if gets_chan then chan+1 else chan, (alias, inst, config))
            where
            config = Instrument.cscale #= scale $
                Instrument.cenviron #= RestrictedEnviron.make environ $
                Instrument.config1 dev chan
    dev = Midi.write_device dev_

    -- Actually pemade and kantilan have an umbang isep pair for both polos and
    -- sangsih.
    pasang name =
        [ (name, sc_patch name <> "-pasang", False, polos_sangsih name, Nothing)
        , umbang_patch (name <> "-p") name
        , isep_patch (name <> "-s") name
        ]
    sc_patch name = "kontakt/sc-" <> name
    polos_sangsih name =
        [ (Gangsa.inst_polos, (to_val $ inst $ name <> "-p"))
        , (Gangsa.inst_sangsih, (to_val $ inst $ name <> "-s"))
        ]
    tuning val = [(Environ.tuning, to_val val)]
    to_val :: RestrictedEnviron.ToVal a => a -> RestrictedEnviron.Val
    to_val = RestrictedEnviron.to_val
    inst = Repl.Util.instrument
    umbang_patch name patch =
        (name, sc_patch patch, True, tuning Environ.umbang, Just umbang)
    isep_patch name patch =
        (name, sc_patch patch, True, tuning Environ.isep, Just isep)
    patch name = (name, sc_patch name, True, [], Nothing)
    umbang = extended_legong_scale "umbang" Legong.umbang
    isep = extended_legong_scale "isep" Legong.isep

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
    [ set_tuning Environ.umbang $ scale "umbang" Wayang.umbang $
        wayang "wayang-umbang"
    , set_tuning Environ.isep $ scale "isep" Wayang.isep $ wayang "wayang-isep"
    , Instrument.text #= "Tuned to 12TET." $ wayang "wayang12"
    ] ++ map (MidiInst.with_code (Bali.pasang_code <> with_weak))
    [ wayang "wayang"
    , MidiInst.range (BaliScales.scale_range Wayang.pemade) $
        wayang "wayang-pemade"
    , MidiInst.range (BaliScales.scale_range Wayang.kantilan) $
        wayang "wayang-kantilan"
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
    scale tuning nns = (Instrument.text #= doc)
        . (Instrument.scale #= Just (wayang_scale tuning nns))
        where
        doc = "These set the scale and tuning automatically, and expect the\
            \ patch to be tuned to the instrument's natural scale."
    set_tuning tuning = MidiInst.default_scale Wayang.scale_id
        . MidiInst.environ Environ.tuning tuning

-- | Set up a gender wayang quartet.
--
-- There are two pasang instruments, which then rely on the kotekan calls to
-- split into inst-polos and inst-sangsih.  This uses the traditional setup
-- with polos on umbang.
config_wayang :: Text -> MidiConfig.Config
config_wayang dev_ = MidiConfig.config
    [ ("p", "kontakt/wayang-pemade",
        pasang "p-umbang" "p-isep" $ Instrument.config [])
    , ("k", "kontakt/wayang-kantilan",
        pasang "k-umbang" "k-isep" $ Instrument.config [])
    , ("p-isep", "kontakt/wayang-isep", Instrument.config1 dev 0)
    , ("p-umbang", "kontakt/wayang-umbang", Instrument.config1 dev 1)
    , ("k-isep", "kontakt/wayang-isep", Instrument.config1 dev 2)
    , ("k-umbang", "kontakt/wayang-umbang", Instrument.config1 dev 3)
    ]
    where
    pasang polos sangsih = MidiConfig.environ Gangsa.inst_polos (inst polos)
        . MidiConfig.environ Gangsa.inst_sangsih (inst sangsih)
    dev = Midi.write_device dev_
    inst = Score.Instrument

wayang_scale :: Text -> [Pitch.NoteNumber] -> Instrument.PatchScale
wayang_scale tuning nns = Instrument.make_patch_scale ("wayang " <> tuning) $
    zip (wayang_keys False) nns

-- | A PatchScale for the extended wayang scale, that goes down to i1.
extended_wayang_scale :: Text -> [Pitch.NoteNumber] -> Instrument.PatchScale
extended_wayang_scale tuning nns =
    Instrument.make_patch_scale ("wayang " <> tuning) $
        zip (wayang_keys True) (Wayang.extend nns)

extended_legong_scale :: Text -> [Pitch.NoteNumber] -> Instrument.PatchScale
extended_legong_scale tuning nns =
    Instrument.make_patch_scale ("legong " <> tuning) $
        zip legong_keys (Legong.extend nns)

legong_keys :: [Midi.Key]
legong_keys = trim $ concatMap keys [3..]
    where
    trim = take (5*7 + 1)
    keys oct = map (Midi.to_key (oct * 12) +) -- i o e e# u a a#
        [Key.c_1, Key.d_1, Key.e_1, Key.f_1, Key.g_1, Key.a_1, Key.b_1]

-- | If extended is True, emit i1 on up.  Otherwise, give pemade to kantilan
-- range.
wayang_keys :: Bool -> [Midi.Key]
wayang_keys extended = trim $ concatMap keys [2..]
    where
    trim
        | extended = take (7*5 + 1)
        | otherwise = take (3*5) . drop (1 + 3*5)
    keys oct = map (Midi.to_key (oct * 12) +) -- i o e u a
        [Key2.e_2, Key2.f_2, Key2.a_2, Key2.b_2, Key2.c_1]

-- | Debugging helper to see if scale degrees line up.
annotate_scale :: [String] -> [Pitch.NoteNumber] -> [Midi.Key]
    -> [((Midi.Key, Int), (Pitch.Octave, String), Pitch.NoteNumber)]
annotate_scale notes nns keys = zip3 keynum_keys oct_notes nns
    where
    keynum_keys = zip keys (map Midi.from_key keys)
    oct_notes = [(oct, note) | oct <- [1..], note <- notes]

annot_legong = annotate_scale notes (Legong.extend Legong.umbang) legong_keys
    where notes = ["i", "o", "e", "e#", "u", "a", "a#"]

annot_wayang_extended =
    annotate_scale notes (Wayang.extend Wayang.umbang) (wayang_keys True)
    where notes = map (:"") "ioeua"

annot_wayang = annotate_scale notes Wayang.umbang (wayang_keys False)
    where notes = map (:"") "ioeua"

wayang_keymap :: Instrument.AttributeMap
wayang_keymap = Instrument.AttributeMap
    [ (Attrs.mute <> Attrs.loose, [Instrument.Keyswitch Key2.a_2], keymap)
    , (Attrs.mute, [Instrument.Keyswitch Key2.b_2], keymap)
    ]
    where
    keymap = Just $
        Instrument.PitchedKeymap Key2.c_1 Key2.b2 (Midi.from_key Key2.c3)

-- * retuned patch

retuned_patch :: Pitch.ScaleId -> Text -> Instrument.PatchScale
    -> Instrument.Patch -> Instrument.Patch
retuned_patch scale_id tuning patch_scale =
    MidiInst.default_scale scale_id . MidiInst.environ Environ.tuning tuning
    . (Instrument.text #= doc) . (Instrument.scale #= Just patch_scale)
    where
    doc = "The instrument is expected to tune to the scale using the\
        \ generated KSP."

-- | Write KSP to retune a 12TET patch.
write_wayang_ksp :: IO ()
write_wayang_ksp = mapM_ (uncurry KontaktUtil.write)
    [ ("wayang-umbang.ksp",
        KontaktUtil.tuning_ksp $ extended_wayang_scale "umbang" Wayang.umbang)
    , ("wayang-isep.ksp",
        KontaktUtil.tuning_ksp $ extended_wayang_scale "isep" Wayang.isep)
    , ("legong-umbang.ksp",
        KontaktUtil.tuning_ksp $ extended_legong_scale "umbang" Legong.umbang)
    , ("legong-isep.ksp",
        KontaktUtil.tuning_ksp $ extended_legong_scale "isep" Legong.isep)
    ]

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

-- | The convention is symbols for thoppi, and letters for valantalai.  Also,
-- vowels for open sounds, consonants for closed ones.  Soft strokes look like
-- simpler version of their equivalent loud strokes.
mridangam_left, mridangam_right :: [Drums.Note]
mridangam_stops :: [(Drums.Group, [Drums.Group])]
(mridangam_left, mridangam_right, mridangam_stops) = (left, right, stops)
    where
    left = concat $
        [ group t_closed
            [ n 'a' "-" tha 0.5
            , n 'z' "+" tha 1
            ]
        , group t_open
            [ n 'x' "o" thom 1
            , n 's' "." thom 0.5
            ]
        ]
    right = concat $
        [ group v_closed
            [ n '1' "l" ki 0.5 -- TODO ki<>soft once I have the separate sample
            , n 'q' "k" ki 1
            , n 'w' "t" ta 1
            ]
        , group v_sadam
            [ n 'e' "n" nam 1
            , n 'r' "d" din 1
            ]
        , group v_chapu
            [ n '5' "v" muru 1
            , n 't' "u" arai 1
            ]
        , group v_dheem [n 'y' "i" dheem 1]
        ]

    stops =
        [ (t_closed, [t_open])
        , (v_closed, [v_sadam, v_chapu, v_dheem])
        , (v_sadam, [v_chapu, v_dheem])
        , (v_chapu, [v_dheem])
        ]
    v_closed = "v-closed"
    v_sadam = "v-sadam"
    v_chapu = "v-chapu"
    v_dheem = "v-dheem"
    t_closed = "t-closed"
    t_open = "t-open"
    group name = map $ \n -> n { Drums.note_group = name }
    n = Drums.note_dyn

mridangam_keymap :: [Drums.Note]
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

write_mridangam_ksp :: IO ()
write_mridangam_ksp = mapM_ (uncurry KontaktUtil.write)
    [ ("mridangam.ksp", KontaktUtil.drum_mute_ksp "mridangam"
        mridangam_notes mridangam_stops)
    , ("mridangam-old.ksp", KontaktUtil.drum_mute_ksp "mridangam"
        old_mridangam_notes mridangam_stops)
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
