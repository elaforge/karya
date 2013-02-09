-- | Modartt's amazing Pianoteq softsynth.
module Local.Instrument.Pianoteq where
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "ptq" "Modartt Pianoteq" (-24, 24) controls

controls =
    [ (64, "sustain-pedal") -- TODO should be a general control
    , (67, "soft-pedal")
    , (66, "sost-pedal")
    , (69, "harmonic-pedal")
    -- whole bunch more
    ]
