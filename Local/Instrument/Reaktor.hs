-- | Native Instruments' Reaktor softsynth.
module Local.Instrument.Reaktor where
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make
    (MidiInst.softsynth "reak" "Native Instruments Reaktor" pb_range [])
    { MidiInst.extra_patches = MidiInst.with_empty_code patches }

pb_range = (-36, 36)

patches :: [Instrument.Patch]
patches =
    [ MidiInst.pressure $ MidiInst.patch pb_range "fm1" [(4, "depth")]
    ]
