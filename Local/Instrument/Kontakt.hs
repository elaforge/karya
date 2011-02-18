-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import Derive.Attrs
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "kkt" (Just "loop1") (-12, 12) patches [] id

patches =
    [ inst "hang1" hang_keymap
    , inst "hang2" hang_keymap
    ]

inst name ks = Instrument.set_note_calls hang_calls $
    Instrument.set_keyswitches ks $
    Instrument.patch $ Instrument.instrument name [] (-12, 12)

hang_keymap =
    [ (center, 36)
    , (edge, 37), (slap, 38), (middle, 39), (knuckle, 40)
    , (no_attrs, 36)
    ]

hang_calls = ["Derive.Instrument.Drums.hang"]
