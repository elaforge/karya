{- | Instrument db for the Yamaha VL1-m.
-}
module Local.Instrument.Vl1m where
import System.FilePath ((</>))

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Parse as Parse


load :: FilePath -> IO MidiDb.SynthDesc
load dir = do
    parsed <- Parse.patches (dir </> "vl1")
    patches <- case parsed of
        Left err -> error ("parse patches: " ++ show err)
        Right patches -> return patches
    return $ (vl1, MidiDb.patch_map patches)

vl1 = Instrument.synth "vl1" "vl1 dev" vl1_controllers

vl1_controllers =
    [
    (2, "breath")
    ]

-- TODO sysex parser
