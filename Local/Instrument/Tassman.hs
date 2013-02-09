-- | Applied Accoustic's Tassman softsynth.
module Local.Instrument.Tassman where
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "tass" "Applied Accoustics Tassman" (-24, 24) controls

controls =
    [
    ]
