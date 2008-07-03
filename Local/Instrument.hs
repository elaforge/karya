module Local.Instrument where
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb

import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Z1 as Z1


load :: FilePath -> IO Db.Db
load dir = do
    synth_maps <- mapM ($dir) [Fm8.load, Z1.load]
    return $ Db.db (MidiDb.midi_db synth_maps)
