-- | Native Instrument's Reaktor softsynth.
module Local.Instrument.Reaktor where
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "reak" (Just "loop1") (-36, 36) [] controls id

controls =
    [
    ]
