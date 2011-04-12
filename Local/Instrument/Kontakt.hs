-- | Native Instruments' Kontakt sampler.
--
-- Unfortunately the instruments here have to be hardcoded unless I want to
-- figure out how to parse .nki files or something.
module Local.Instrument.Kontakt where
import Derive.Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Instrument.Drums as Drums

import qualified Perform.Midi.Instrument as Instrument

import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "kkt" (Just "loop1") (-12, 12) [])
        { MidiInst.extra_patches = MidiInst.with_code hang_code patches
        }

patches =
    [ inst "hang1" hang_keymap
    , inst "hang2" hang_keymap
    ]

inst name ks = Instrument.set_keyswitches ks $
    Instrument.patch $ Instrument.instrument name [] (-12, 12)

hang_keymap =
    [ (center, 36)
    , (edge, 37), (slap, 38), (middle, 39), (knuckle, 40)
    , (no_attrs, 36)
    ]

hang_code = MidiInst.empty_code
    { MidiInst.note_calls = [Derive.make_lookup Drums.hang]
    }
