-- | Image-Line's Drumaxx softsynth.
module Local.Instrument.Drumaxx where
import qualified Derive.Derive as Derive
import qualified Derive.Instrument.Drums as Drums
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst

import Midi.Key
import Derive.Attrs


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "dmx" (Just "dmx") (-24, 24) [])
        { MidiInst.modify_patch =
            Instrument.set_triggered . Instrument.set_keymap keymap
        , MidiInst.code = MidiInst.empty_code
            { MidiInst.note_calls = [Derive.make_lookup Drums.traps]
            }
        }

-- The octave numbers on the drummax are one greater than the standard usage.
-- This is for "Acoustic 1 FG".  I'll have to come up with a standard mapping
-- later.
keymap =
    [ (bd, c2)
    , (snare, d2)

    , (hh, f2+1)
    , (ride, d3+1)
    , (crash, c3+1)
    , (tom @+ high, c3)
    , (tom @+ middle, b2)
    , (tom @+ low, g2)
    ]
