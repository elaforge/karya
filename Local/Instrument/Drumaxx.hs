-- | Image-Line's Drumaxx softsynth.
module Local.Instrument.Drumaxx where
import Midi.Key
import Derive.Attrs
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $ MidiDb.softsynth "dmx" (Just "dmx") (-24, 24) [] []
    (Instrument.set_note_calls note_calls . Instrument.set_triggered
        . Instrument.set_keymap keymap)

note_calls = ["Derive.Instrument.Drums.note"]

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
