{- | Instrument db for the Korg Z1 keyboard.
-}
module Local.Instrument.Z1 where
import System.FilePath ((</>))

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Db as Db
import qualified Instrument.Parse as Parse


load :: FilePath -> IO Db.SynthDesc
load dir = do
    parsed <- Parse.patches (dir </> "z1")
    patches <- case parsed of
        Left err -> error ("parse patches: " ++ show err)
        Right patches -> return patches
    return $ (z1, Db.patch_map patches)

z1 = Instrument.synth "z1" "Z1 midi device" z1_controllers

z1_controllers =
    [
    -- The PE controls are the "performance expression" knobs whose effect
    -- depends on the instrument.
    (13, "pe 1"), (20, "pe 2"), (21, "pe 3"), (22, "pe 4"), (23, "pe 5")
    , (16, "pad x"), (17, "pad y")
    , (65, "z1 port sw")
    -- General purpose on/off switches.
    , (80, "z1 sw 1"), (81, "z1 sw 2")

    -- Various filter cutoff etc.
    ]

-- TODO sysex parser
