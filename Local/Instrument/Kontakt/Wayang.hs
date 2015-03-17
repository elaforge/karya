-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Gender wayang patches.
module Local.Instrument.Kontakt.Wayang where
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Cmd.Instrument.MidiConfig as MidiConfig
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Sub as Sub
import qualified Derive.Environ as Environ
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Call.Note as Note
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Local.Instrument.Kontakt.Util as Util
import Global


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
patches :: [MidiInst.Patch]
patches = map (MidiInst.with_code (code <> with_weak))
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
    wayang name = Instrument.set_flag Instrument.ConstantPitch
        $ (Instrument.instrument_#Instrument.maybe_decay #= Just 0)
        $ (Instrument.attribute_map #= wayang_keymap)
        $ Instrument.patch $ Instrument.instrument name [] pb_range
    scale tuning nns = (Instrument.text #= doc)
        . (Instrument.scale #= Just (wayang_scale tuning nns))
        where
        doc = "These set the scale and tuning automatically, and expect the\
            \ patch to be tuned to the instrument's natural scale."
    set_tuning tuning = MidiInst.default_scale Wayang.scale_id
        . MidiInst.environ Environ.tuning tuning

pb_range :: Instrument.PbRange
pb_range = (-24, 24)

-- | Set up a gender wayang quartet.
--
-- There are two pasang instruments, which then rely on the kotekan calls to
-- split into inst-polos and inst-sangsih.  This uses the traditional setup
-- with polos on umbang.
config :: Text -> MidiConfig.Config
config dev_ = MidiConfig.config
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
write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("wayang-umbang.ksp",
        Util.tuning_ksp $ extended_wayang_scale "umbang" Wayang.umbang)
    , ("wayang-isep.ksp",
        Util.tuning_ksp $ extended_wayang_scale "isep" Wayang.isep)
    , ("legong-umbang.ksp",
        Util.tuning_ksp $ extended_legong_scale "umbang" Legong.umbang)
    , ("legong-isep.ksp",
        Util.tuning_ksp $ extended_legong_scale "isep" Legong.isep)
    ]
