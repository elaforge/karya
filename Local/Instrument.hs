module Local.Instrument where
import Control.Monad
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb

import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Z1 as Z1
import qualified Local.Instrument.Vl1m as Vl1m
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Pianoteq as Pianoteq

import qualified Derive.Score as Score
import qualified Instrument.Serialize as Serialize
import qualified Instrument.Search as Search

import qualified App.Config as Config


load :: FilePath -> IO Db.Db
load app_dir = do
    let dir = app_dir </> Config.instrument_dir
    synth_maps <- mapM ($dir) [Fm8.load, Z1.load, Vl1m.load, Kontakt.load,
        Pianoteq.load]
    -- (midi_db, index) <- load_db app_dir
    let (midi_db, index) = (MidiDb.midi_db [], Search.empty_index)
    let (merged, overlaps) = MidiDb.merge (MidiDb.midi_db synth_maps) midi_db
    unless (null overlaps) $
        Log.warn $ "overlapping instruments discarded while merging: "
            ++ show (map (\(Score.Instrument inst) -> inst) overlaps)
    return $ Db.db merged index

load_db app_dir = do
    saved <- Serialize.unserialize
        (app_dir </> Config.instrument_db_cache)
    case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db: " ++ show err
            return (MidiDb.empty, Search.empty_index)
        Right (Serialize.SavedDb _ midi_db index) -> return (midi_db, index)

-- | Load instruments that take longer, and should be cached by make_db.
load_slow :: FilePath -> IO MidiDb.MidiDb
load_slow dir = do
    synth_maps <- mapM ($dir) [Z1.load_slow, Vl1m.load_slow]
    return $ MidiDb.midi_db synth_maps
