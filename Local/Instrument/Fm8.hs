{- | Instrument db for the native instruments FM8 software synth.
-}
module Local.Instrument.Fm8 where
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Db as Db

load :: FilePath -> IO Db.SynthDesc
load _dir = return $ (fm8, Db.InventPatch)

fm8 = Instrument.synth "fm8" "IAC Driver Bus 1/CoreMIDI" fm8_controllers

fm8_controllers =
    [ (4, "fm8 controller 1"), (11, "fm8 controller 2")
    , (16, "morph x"), (17, "morph y")
    ]
