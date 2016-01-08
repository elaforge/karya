-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Patches for my reyong samples.
module Local.Instrument.Kontakt.Reyong where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.Controls as Controls
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Signal as Signal
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
    tuning = BaliScales.Umbang -- TODO verify how mine are tuned
    set_scale = (Instrument.scale #= Just patch_scale)
        . MidiInst.default_scale Legong.scale_id
        . MidiInst.environ EnvKey.tuning tuning
    -- Trompong starts at 2a, trompong + reyong has 15 keys.
    patch_scale =
        Legong.patch_scale (take 15 . drop (5+4) . Legong.strip_pemero) tuning

-- | Take the damp value from note off time and put it as a constant
-- aftertouch.  The KSP will then sample the value at note on time and use it
-- at note off time.  Otherwise, given two repeated notes with the same pitch,
-- even polyphonic aftertouch will interfere in the same way that a normal
-- control would.
--
-- This kind of giant hassle to just get note offs reliable is why MIDI is such
-- garbage.
postproc :: Score.Event -> Score.Event
postproc event =
    Score.set_control Controls.aftertouch
        (Score.untyped (Signal.constant rescaled)) event
    where
    damp = maybe 0 (Signal.at (Score.event_end event) . Score.typed_val) $
        Map.lookup Reyong.damp_control $
        Score.event_untransformed_controls event
    rescaled = Num.normalize 0 127 $ Num.scale 0 damp_max damp

-- | Maximum value I can use for damp aftertouch.  Values above this are
-- keyswitches.
damp_max :: Signal.Y
damp_max = 122

attribute_map :: Instrument.AttributeMap
attribute_map = Instrument.keyswitches
    [ (Reyong.cek <> Attrs.open, [at 123])
    , (Reyong.cek, [at 124])
    , (Attrs.mute <> Attrs.open, [at 125])
    , (Attrs.mute, [at 126])
    , (damp_closed, [at 127, Instrument.Keyswitch 13])
    , (mempty, [at 127, Instrument.Keyswitch 12])
    ]
    where at = Instrument.Aftertouch

-- | Note releases use +mute+closed.  +mute+open is the default.
damp_closed :: Score.Attributes
damp_closed = Score.attr "damp-closed"
