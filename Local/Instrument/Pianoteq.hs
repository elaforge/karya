module Local.Instrument.Pianoteq where
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control
import qualified Instrument.MidiDb as MidiDb

load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return (pianoteq, MidiDb.wildcard_patch_map patch_template)

patch_template = Instrument.patch
    (Instrument.instrument synth_name "" Nothing Control.empty_map (-24, 24))

synth_name = "ptq"
pianoteq = Instrument.synth synth_name "ptq" controls

controls =
    [ (64, "sustain-pedal") -- TODO should be a general control
    , (67, "soft-pedal")
    , (66, "sost-pedal")
    , (69, "harmonic-pedal")
    -- whole bunch more
    ]
