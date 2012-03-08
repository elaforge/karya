-- | Native Instruments' FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "fm8" (-96, 96) controls

controls =
    [ (4, "fm8-control-1"), (11, "fm8-control-2")
    , (16, "morph-x"), (17, "morph-y")
    ]
