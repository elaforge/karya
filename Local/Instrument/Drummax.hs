module Local.Instrument.Drummax where
import Midi.Key
import Derive.Attrs
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control
import qualified Instrument.MidiDb as MidiDb

load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return (inst, MidiDb.wildcard_patch_map patch_template)

patch_template = Instrument.patch $
    Instrument.set_keymap keymap $
    (Instrument.instrument synth_name "" Nothing Control.empty_map (-24, 24))

-- The octave numbers on the drummax are one greater than the standard usage.
-- This is for "Acoustic 1 FG".  I'll have to come up with a standard mapping
-- later.
keymap =
    [ (bd, c2)
    , (snare, d2)

    , (hh, f2+1)
    , (ride, d3+1)
    , (crash, c3+1)
    , (tom @+ high, c3)
    , (tom @+ middle, b2)
    , (tom @+ low, g2)
    ]

synth_name = "dmx"
inst = Instrument.synth synth_name "dmx" controls
controls = []
