-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Gender wayang patches.
module Local.Instrument.Kontakt.Wayang where
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiConfig as MidiConfig
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.Bali as Bali
import qualified Derive.Instrument.DUtil as DUtil
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
    [ set_tuning EnvKey.umbang $ scale BaliScales.Umbang $
        patch "wayang-umbang"
    , set_tuning EnvKey.isep $ scale BaliScales.Isep $ patch "wayang-isep"
    , Instrument.text #= "Tuned to 12TET." $ patch "wayang12"
    ] ++ map (MidiInst.with_code (Bali.pasang_code <> with_weak))
    [ patch "wayang"
    , MidiInst.range (BaliScales.scale_range Wayang.pemade) $
        patch "wayang-pemade"
    , MidiInst.range (BaliScales.scale_range Wayang.kantilan) $
        patch "wayang-kantilan"
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
    patch name = Instrument.set_flag Instrument.ConstantPitch $
        (Instrument.instrument_#Instrument.maybe_decay #= Just 0) $
        (Instrument.attribute_map #= attribute_map) $
        MidiInst.patch (-24, 24) name []
    scale tuning = (Instrument.text #= doc)
        . (Instrument.scale #= Just (Wayang.patch_scale False tuning))
        where
        doc = "These set the scale and tuning automatically, and expect the\
            \ patch to be tuned to the instrument's natural scale."
    set_tuning tuning = MidiInst.default_scale Wayang.scale_id
        . MidiInst.environ EnvKey.tuning tuning

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

attribute_map :: Instrument.AttributeMap
attribute_map = Instrument.AttributeMap
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
    MidiInst.default_scale scale_id . MidiInst.environ EnvKey.tuning tuning
    . (Instrument.text #= doc) . (Instrument.scale #= Just patch_scale)
    where
    doc = "The instrument is expected to tune to the scale using the\
        \ generated KSP."

-- | Write KSP to retune a 12TET patch.
write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("wayang-umbang.ksp", ksp $ Wayang.patch_scale True BaliScales.Umbang)
    , ("wayang-isep.ksp",   ksp $ Wayang.patch_scale True BaliScales.Isep)
    , ("legong-umbang.ksp", ksp $ Legong.patch_scale BaliScales.Umbang)
    , ("legong-isep.ksp",   ksp $ Legong.patch_scale BaliScales.Isep)
    ]
    where ksp = Util.tuning_ksp
