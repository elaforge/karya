-- | Native Instrument's Reaktor softsynth.
module Local.Instrument.Reaktor where
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $ MidiInst.softsynth "reak" (-36, 36) []
