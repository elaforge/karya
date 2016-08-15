-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Gender wayang patches.
module Local.Instrument.Kontakt.Wayang where
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Ui.StateConfig as StateConfig
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Call.Bali.Gender as Gender
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
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
patches = map (MidiInst.code #= code <> with_weak)
    [ set_scale BaliScales.Umbang $ patch "wayang-umbang"
    , set_scale BaliScales.Isep $ patch "wayang-isep"
    , MidiInst.doc #= "Tuned to 12TET." $ patch "wayang12"
    ] ++ map (MidiInst.code #= Bali.pasang_code <> with_weak)
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
    patch name = set_params $ MidiInst.named_patch (-24, 24) name []
    set_params = (MidiInst.patch %=) $
        Patch.add_flag Patch.ConstantPitch . (Patch.decay #= Just 0)
        . (Patch.attribute_map #= attribute_map)
    set_scale tuning =
        (MidiInst.patch#Patch.scale
            #= Just (Wayang.instrument_scale False tuning))
        . MidiInst.default_scale Wayang.scale_id
        . MidiInst.environ EnvKey.tuning tuning

-- | Set up a gender wayang quartet.
--
-- There are two pasang instruments, which then rely on the kotekan calls to
-- split into inst-polos and inst-sangsih.  This uses the traditional setup
-- with polos on umbang.
allocations :: Text -> StateConfig.Allocations
allocations dev_ = MidiInst.allocations
    [ ("p", "kontakt/wayang-pemade", pasang "p-umbang" "p-isep",
        StateConfig.Dummy)
    , ("k", "kontakt/wayang-kantilan", pasang "k-umbang" "k-isep",
        StateConfig.Dummy)
    , ("p-isep", "kontakt/wayang-isep", id, midi_channel 0)
    , ("p-umbang", "kontakt/wayang-umbang", id, midi_channel 1)
    , ("k-isep", "kontakt/wayang-isep", id, midi_channel 2)
    , ("k-umbang", "kontakt/wayang-umbang", id, midi_channel 3)
    ]
    where
    midi_channel = StateConfig.Midi . Patch.config1 dev
    pasang polos sangsih = Common.add_environ Gangsa.inst_polos (inst polos)
        . Common.add_environ Gangsa.inst_sangsih (inst sangsih)
    dev = Midi.write_device dev_
    inst = Score.Instrument

attribute_map :: Patch.AttributeMap
attribute_map = Common.attribute_map
    [ (Attrs.mute <> Attrs.loose, ([Patch.Keyswitch Key2.a_2], keymap))
    , (Attrs.mute,                ([Patch.Keyswitch Key2.b_2], keymap))
    , (mempty, ([], Just $ Patch.PitchedKeymap Key2.c3 Key2.c7 Key2.c3))
    ]
    where keymap = Just $ Patch.PitchedKeymap Key2.c_1 Key2.b2 Key2.c3
