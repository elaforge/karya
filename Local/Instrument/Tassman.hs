-- | Applied Accoustic's Tassman softsynth.
module Local.Instrument.Tassman where
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "tass" (Just "loop1") (-24, 24) [] controls id

controls =
    [
    ]
