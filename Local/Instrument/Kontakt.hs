-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Data.List as List

import qualified Midi.CC as CC
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.StateConfig as StateConfig
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Keymap as Keymap

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Articulation as Articulation
import qualified Derive.Call.Prelude.Highlight as Highlight
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.PSignal as PSignal
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN

import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Local.Instrument.Kontakt.KendangBali as KendangBali
import qualified Local.Instrument.Kontakt.KendangSunda as KendangSunda
import qualified Local.Instrument.Kontakt.Mridangam as Mridangam
import qualified Local.Instrument.Kontakt.Pakhawaj as Pakhawaj
import qualified Local.Instrument.Kontakt.Reyong as Reyong
import qualified Local.Instrument.Kontakt.Wayang as Wayang
import qualified Local.Instrument.Reaktor as Reaktor

import Global


synth :: MidiInst.Synth
synth = MidiInst.synth synth_name "Native Instrument Kontakt" patches

synth_name :: InstTypes.SynthName
synth_name = "kontakt"

patches :: [MidiInst.Patch]
patches =
    MidiInst.default_patch pb_range [] : concat
    [ misc_patches
    , hang_patches
    , KendangBali.patches, KendangSunda.patches
    , Mridangam.patches, Pakhawaj.patches, Reyong.patches, Wayang.patches
    ]

patch :: InstTypes.Name -> [(Midi.Control, Score.Control)] -> MidiInst.Patch
patch = MidiInst.named_patch pb_range

-- One pitch bend modulator can only do +-12, but if you put two on you get
-- +-24.
pb_range :: Patch.PbRange
pb_range = (-24, 24)

-- * misc

misc_patches :: [MidiInst.Patch]
misc_patches = concat
    [ library, mcgill, balalaika, anthology_wind, sonic_couture, sc_bali, misc
    ]

library :: [MidiInst.Patch]
library =
    [ patch "choir" [(1, "vowel")]
    ]

-- | From the McGill sample library.
mcgill :: [MidiInst.Patch]
mcgill =
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
    [ MidiInst.code #= code $
        MidiInst.attribute_map #= Patch.single_keyswitches ks $
        MidiInst.make_patch $
        Patch.set_flag Patch.HoldKeyswitch $
        Patch.control_map #= Control.control_map controls $
        Patch.patch pb_range "balalaika"
    ]
    where
    code = MidiInst.note_generators [("(", Articulation.c_attr_legato)]
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
anthology_wind =
    [ MidiInst.pressure $
        MidiInst.attribute_map #= Patch.single_keyswitches dizi_ks $
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
    [ patch "ebow"
        [(1, "harm"), (21, Controls.lpf), (22, Controls.q), (23, Controls.hpf)]
    , guzheng
    ]

guzheng :: MidiInst.Patch
guzheng = MidiInst.code #= code $ MidiInst.nn_range range $
    MidiInst.decay #= Just 5 $
    MidiInst.attribute_map #= Patch.single_keyswitches ks $
    patch "guzheng" [(23, Controls.lpf), (24, Controls.q), (27, Controls.hpf)]
    where
    code = MidiInst.note_generators [("左", DUtil.attrs_note Attrs.left)]
        <> MidiInst.note_transformers [("standard-strings", standard_strings)]
        <> MidiInst.note_calls (MidiInst.null_call c_highlight_strings)
    standard_strings = DUtil.transformer0 "standard-strings"
        ("Set " <> ShowVal.doc EnvKey.open_strings
            <> " to standard pitches: " <> ShowVal.show_val strings)
        $ \_ deriver -> Derive.with_val EnvKey.open_strings
            (map PSignal.nn_pitch strings) deriver
    ks =
        [ (Attrs.harm, Key2.as5)
        , (Attrs.left, Key2.b5) -- left hand, no pick
        , (mempty, Key2.c6) -- right hand, picked
        ]
    strings = take (4*5 + 1) $ -- 4 octaves + 1, so D to D
        concatMap ((\nns oct -> map (oct+) nns) notes) octaves
        where
        notes = [NN.d2, NN.e2, NN.fs2, NN.a2, NN.b2]
        octaves = map fromIntegral [0, 12 ..]
    -- Let's say the top string can bend a minor third.
    range = (head strings, last strings + 3)

c_highlight_strings :: Derive.Generator Derive.Note
c_highlight_strings = Note.transformed_note
    ("Highlight any notes whose initial pitch either is or isn't in "
    <> ShowVal.doc EnvKey.open_strings <> ".") mempty $ \args deriver -> do
        start <- Args.real_start args
        Highlight.out_of_range $
            Highlight.open_strings start Highlight.warn_non_open deriver

sc_bali :: [MidiInst.Patch]
sc_bali = map add_doc $
    CUtil.simple_drum Nothing gong_notes (sc_patch "gong")
    : CUtil.simple_drum Nothing kempli_kajar_notes (sc_patch "kempli")
    : concat
    [ gangsa (range_of Legong.jegog) "jegog"
    , gangsa (range_of Legong.calung) "calung"
    , gangsa (range_of Legong.penyacah) "penyacah"
    , gangsa Legong.ugal_range "ugal"
    , gangsa (range_of Legong.pemade) "pemade"
    , gangsa (range_of Legong.kantilan) "kantilan"
    ] ++
    [ reyong_ks $ ranged_patch Legong.reyong_range "reyong"
    , ranged_patch Legong.trompong_range "trompong"
    ]
    where
    gangsa range name =
        [ MidiInst.code #= Bali.pasang_code $
            ranged_patch range (name <> "-pasang")
        , gangsa_ks $ ranged_patch range name
        ]
    range_of = BaliScales.scale_range
    ranged_patch range = MidiInst.range range . sc_patch
    sc_patch name =
        MidiInst.patch %= Patch.set_flag Patch.ConstantPitch $
        MidiInst.named_patch (-2, 2) ("sc-" <> name) []
    add_doc = MidiInst.doc
        %= ("Sonic Couture's Balinese gamelan sample set. " <>)
    gangsa_ks = MidiInst.attribute_map #= Patch.single_keyswitches
        [(Attrs.mute, Key2.cs1), (mempty, Key2.c1)]
    reyong_ks = MidiInst.attribute_map #= Patch.single_keyswitches
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
misc = [MidiInst.code #= Reaktor.resonant_filter $ patch "filtered" []]

kebyar_allocations :: Text -> StateConfig.Allocations
kebyar_allocations dev_ = make_config $ concat
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
    -- (inst, qualified, gets_chan, environ, scale)
    make_config :: [(Text, Text, Bool,
            [(BaseTypes.Key, RestrictedEnviron.Val)], Maybe Patch.Scale)]
        -> StateConfig.Allocations
    make_config = MidiInst.allocations . snd . List.mapAccumL allocate 0
        where
        allocate chan (inst, qualified, gets_chan, environ, scale) =
            ( next_chan
            , (inst, qualified, set_config, backend)
            )
            where
            next_chan = if gets_chan then chan+1 else chan
            backend
                | gets_chan = StateConfig.Midi $
                    Patch.cscale #= scale $ Patch.config1 dev chan
                -- Pasang instruments don't get an allocation.  Otherwise they
                -- don't have the right tuning.
                | otherwise = StateConfig.Dummy
            set_config = Common.cenviron #= RestrictedEnviron.make environ
    dev = Midi.write_device dev_

    -- Actually pemade and kantilan have an umbang isep pair for both polos and
    -- sangsih, but since I don't have that many sample sets I have
    -- a mini-ensemble with only one pair of each gangsa.
    pasang name =
        [ (name, sc_qualified name <> "-pasang", False, polos_sangsih name,
            Nothing)
        , umbang_patch (name <> "-p") name
        , isep_patch (name <> "-s") name
        ]
    sc_qualified name = synth_name <> "/sc-" <> name
    polos_sangsih name =
        [ (Gangsa.inst_polos, to_val $ make_inst $ name <> "-p")
        , (Gangsa.inst_sangsih, to_val $ make_inst $ name <> "-s")
        ]
    to_val :: RestrictedEnviron.ToVal a => a -> RestrictedEnviron.Val
    to_val = RestrictedEnviron.to_val
    make_inst = Score.instrument
    umbang_patch name patch =
        ( name, sc_qualified patch, True
        , tuning BaliScales.Umbang
        , Just $ Legong.complete_instrument_scale BaliScales.Umbang
        )
    isep_patch name patch =
        ( name, sc_qualified patch, True
        , tuning BaliScales.Isep
        , Just $ Legong.complete_instrument_scale BaliScales.Isep
        )
    tuning val = [(EnvKey.tuning, to_val val)]
    patch name = (name, sc_qualified name, True, [], Nothing)

-- * hang

hang_patches :: [MidiInst.Patch]
hang_patches = map (MidiInst.code #= hang_code)
    [ MidiInst.attribute_map #= Patch.single_keyswitches hang_ks $
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
hang_strokes :: [(Score.Attributes, Midi.Key, Maybe BaseTypes.CallId,
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
