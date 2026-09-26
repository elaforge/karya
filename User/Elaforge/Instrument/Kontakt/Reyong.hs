-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Patches for my reyong samples.
module User.Elaforge.Instrument.Kontakt.Reyong where
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Reyong as Reyong
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Selisir as Selisir

import qualified Perform.Midi.Patch as Patch
import Global


patches :: [MidiInst.Patch]
patches =
    [ setScale $ patch "reyong"
    , patch "reyong12"
    ]
    where
    patch name = MidiInst.code #= code $ setParams $
        MidiInst.namedPatch (-24, 24) name []
    code = Bali.zeroDurMute 1 <> MidiInst.nullCall DUtil.constantPitch
    setParams = MidiInst.patch
        %= MidiInst.addFlags [Patch.UseFinalNoteOff]
            . (Patch.defaults#Patch.decay #= Just 0)
            . (Patch.attributeMap #= attributeMap)
    tuning = BaliScales.Umbang -- TODO verify how mine are tuned
    setScale =
        (MidiInst.patch#Patch.defaults#Patch.scale #= Just instrumentScale)
        . MidiInst.environ EnvKey.tuning tuning
    -- Trompong starts at 3a, trompong + reyong has 15 keys.
    instrumentScale =
        Selisir.instrumentScale (take 15 . drop 4) Selisir.larasRambat tuning

attributeMap :: Patch.AttributeMap
attributeMap = Patch.keyswitches $ map at
    [ (Reyong.cek <> Attrs.open, 4)
    , (Reyong.cek, 3)
    , (Attrs.mute <> Attrs.open, 2)
    , (Attrs.mute, 1)
    , (mempty, 0)
    ]
    where at = second ((:[]) . Patch.Aftertouch)
