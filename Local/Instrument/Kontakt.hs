-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "kkt" (Just "kontakt") (-96, 96) patches [] id

patches =
    [ inst "hang1" hang_keyswitches
    , inst "hang2" hang_keyswitches
    ]

inst name ks = Instrument.set_keyswitches ks $
    Instrument.patch $ Instrument.instrument name [] (-96, 96)

hang_keyswitches =
    [ ("center", 36)
    , ("edge", 37), ("slap", 38), ("mid", 39), ("knuckle", 40)
    , ("", 36)
    ]
