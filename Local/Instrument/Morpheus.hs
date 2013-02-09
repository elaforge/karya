-- | E-mu morpheus module.
module Local.Instrument.Morpheus where
import System.FilePath ((</>))

import Util.Control
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Parse as Parse
import qualified App.MidiInst as MidiInst


synth_name :: FilePath
synth_name = "morph"

load :: FilePath -> IO [MidiInst.SynthDesc]
load = MidiInst.load_db (const MidiInst.empty_code) synth_name

make_db :: FilePath -> IO ()
make_db dir = do
    patches <- Parse.patch_file (dir </> synth_name)
    patches <- return $ map
        (Instrument.instrument_#Instrument.pitch_bend_range #= (-12, 12))
        patches
    MidiInst.save_patches synth patches synth_name dir

synth :: Instrument.Synth
synth = Instrument.synth synth_name "E-mu Morpheus" synth_controls

synth_controls :: [(Midi.Control, String)]
synth_controls =
    -- Definitions depend on the preset.
    [ (1, "a"), (2, "b"), (3, "c"), (4, "d")
    , (64, "switch1"), (65, "switch2"), (66, "switch3")
    ]
