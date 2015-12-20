-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Reyong patches.
module Local.Instrument.Kontakt.Reyong where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import Global


patches :: [MidiInst.Patch]
patches =
    [ (set_scale $ patch "reyong", code)
    , (patch "reyong12", code)
    ]
    where
    code = MidiInst.postproc postproc
    patch name = Instrument.set_flag Instrument.ConstantPitch $
        (Instrument.instrument_#Instrument.maybe_decay #= Just 0) $
        (Instrument.attribute_map #= attribute_map) $
        MidiInst.patch (-24, 24) name []
    tuning = BaliScales.Umbang -- TODO verify
    set_scale = (Instrument.scale #= Just patch_scale)
        . MidiInst.default_scale Legong.scale_id
        . MidiInst.environ EnvKey.tuning tuning
    -- Trompong starts at 2a, trompong + reyong has 15 keys.
    patch_scale =
        Legong.patch_scale (take 15 . drop (5+4) . Legong.strip_pemero) tuning

postproc :: Score.Event -> Score.Event
postproc event = event

attribute_map :: Instrument.AttributeMap
attribute_map = Instrument.single_keyswitches
    [ (cek <> Attrs.closed, 12)
    , (cek <> Attrs.open, 13)
    , (Attrs.mute <> Attrs.closed, 14)
    , (Attrs.mute <> Attrs.open, 15)
    , (mempty, 11)
    ]

cek :: Score.Attributes
cek = Score.attr "cek"
