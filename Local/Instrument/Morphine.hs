-- | Image-Line's Morphine softsynth.
module Local.Instrument.Morphine where
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "morph" (Just "loop1") (-12, 12) [] controls id

controls =
    [
    ]
