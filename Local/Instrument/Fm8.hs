{- | Instrument db for the native instruments FM8 software synth.
-}
module Local.Instrument.Fm8 where
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller
import qualified Instrument.MidiDb as MidiDb

load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return (fm8, MidiDb.wildcard_patch_map patch_template)

patch_template = Instrument.patch
    (Instrument.instrument synth_name "" Nothing Controller.empty_map (-96, 96))

synth_name = "fm8"
fm8 = Instrument.synth synth_name "fm8" fm8_controllers

fm8_controllers =
    [ (4, "fm8 controller 1"), (11, "fm8 controller 2")
    , (16, "morph x"), (17, "morph y")
    ]
