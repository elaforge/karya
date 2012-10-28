-- | Image-Line's Morphine softsynth.
module Local.Instrument.Morphine where
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "morphine" (-12, 12) controls

controls =
    [
    ]
