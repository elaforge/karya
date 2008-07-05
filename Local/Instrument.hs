module Local.Instrument where
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb

import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Z1 as Z1
import qualified Local.Instrument.Vl1m as Vl1m

import qualified Instrument.Serialize as Serialize
import qualified Instrument.Search as Search

import qualified App.Config as Config


load :: FilePath -> IO Db.Db
load app_dir = do
    let dir = app_dir </> Config.instrument_dir
    synth_maps <- mapM ($dir) [Fm8.load, Z1.load, Vl1m.load]
    saved <- Serialize.unserialize
        (app_dir </> Config.instrument_db_cache)
    (midi_db, index) <- case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db: " ++ show err
            return (MidiDb.empty, Search.empty_index)
        Right (Serialize.SavedDb _ midi_db index) -> return (midi_db, index)
    let merged = MidiDb.merge (MidiDb.midi_db synth_maps) midi_db
    return $ Db.db merged index

-- | Load instruments that take longer, and should be cached by make_db.
load_slow :: FilePath -> IO MidiDb.MidiDb
load_slow dir = do
    synth_maps <- mapM ($dir) [Z1.load_slow, Vl1m.load_slow]
    return $ MidiDb.midi_db synth_maps
