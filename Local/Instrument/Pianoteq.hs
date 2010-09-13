-- | Modartt's amazing Pianoteq softsynth.
module Local.Instrument.Pianoteq where
import qualified Instrument.MidiDb as MidiDb


load :: FilePath -> IO MidiDb.SynthDesc
load _dir = return $
    MidiDb.softsynth "ptq" (Just "ptq") (-24, 24) [] controls id

controls =
    [ (64, "sustain-pedal") -- TODO should be a general control
    , (67, "soft-pedal")
    , (66, "sost-pedal")
    , (69, "harmonic-pedal")
    -- whole bunch more
    ]
