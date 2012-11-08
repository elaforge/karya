-- | Native Instruments' FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "fm8" pb_range controls)
        { MidiInst.extra_patches = MidiInst.with_empty_code patches }

pb_range = (-96, 96)

controls :: [(Midi.Control, String)]
controls =
    [ (4, "fm8-control-1"), (11, "fm8-control-2")
    , (16, "morph-x"), (17, "morph-y")
    ]

patches :: [Instrument.Patch]
patches =
    [ Instrument.set_decay 0 $ Instrument.set_flag Instrument.Pressure $
        Instrument.patch $ Instrument.instrument "trumpet" [] pb_range
    ]
