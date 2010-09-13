-- | Native Instruments' FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "fm8" (Just "fm8") (-96, 96) [] controls id

controls =
    [ (4, "fm8-control-1"), (11, "fm8-control-2")
    , (16, "morph-x"), (17, "morph-y")
    ]
