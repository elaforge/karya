-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Patches for my reyong samples.
module Local.Instrument.Kontakt.Reyong where
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Selisir as Selisir

import qualified Perform.Midi.Patch as Patch
import Global


patches :: [MidiInst.Patch]
patches =
    [ set_scale $ patch "reyong"
    , patch "reyong12"
    ]
    where
    patch name = MidiInst.code #= Bali.zero_dur_mute $ set_params $
        MidiInst.named_patch (-24, 24) name []
    set_params = MidiInst.patch
        %= MidiInst.add_flags [Patch.ConstantPitch, Patch.UseFinalNoteOff]
            . (Patch.defaults#Patch.decay #= Just 0)
            . (Patch.attribute_map #= attribute_map)
    tuning = BaliScales.Umbang -- TODO verify how mine are tuned
    set_scale =
        (MidiInst.patch#Patch.defaults#Patch.scale #= Just instrument_scale)
        . MidiInst.default_scale Selisir.scale_id
        . MidiInst.environ EnvKey.tuning tuning
    -- Trompong starts at 3a, trompong + reyong has 15 keys.
    instrument_scale =
        Selisir.instrument_scale (take 15 . drop 4) Selisir.laras_rambat tuning

attribute_map :: Patch.AttributeMap
attribute_map = Patch.keyswitches $ map at
    [ (Reyong.cek <> Attrs.open, 4)
    , (Reyong.cek, 3)
    , (Attrs.mute <> Attrs.open, 2)
    , (Attrs.mute, 1)
    , (mempty, 0)
    ]
    where at = second ((:[]) . Patch.Aftertouch)
