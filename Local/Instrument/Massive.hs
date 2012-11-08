-- | Native Instruments' Massive softsynth.
module Local.Instrument.Massive where
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "msv" (-24, 24) controls)
        { MidiInst.extra_patches = MidiInst.with_empty_code patches }

controls :: [(Midi.Control, String)]
controls =
    [ (1, "macro1")
    ] ++ [(18 + n, "macro" ++ show n) | n <- [2..8]]

patches :: [Instrument.Patch]
patches = []
