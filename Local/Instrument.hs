-- | Load the instrument db.
--
-- TODO the 'load' and 'load_slow' calls should be automatically generated from
-- the contents of the Local/Instrument/ dir.
module Local.Instrument where
import Control.Monad
import System.FilePath ((</>))

import qualified Util.Log as Log

import qualified Local.Instrument.Drumaxx as Drumaxx
import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Pianoteq as Pianoteq
import qualified Local.Instrument.Vl1m as Vl1m
import qualified Local.Instrument.Z1 as Z1

import qualified Derive.Score as Score
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize as Serialize
import qualified Instrument.Search as Search

import qualified App.Config as Config


load :: FilePath -> IO Db.Db
load app_dir = do
    -- (loaded_synth_descs, index) <- load_db app_dir
    let (loaded_synth_descs, index) = ([], Search.empty_index)
    let (loaded, loaded_warns) = MidiDb.midi_db loaded_synth_descs
    forM_ loaded_warns $ \msg -> Log.warn $ "loaded inst db: " ++ msg

    let dir = app_dir </> Config.instrument_dir
    synth_descs <- mapM ($dir)
        [ Drumaxx.load, Fm8.load, Kontakt.load, Pianoteq.load, Vl1m.load
        , Z1.load
        ]
    let (builtin, buildin_warns) = MidiDb.midi_db synth_descs
    forM_ buildin_warns $ \msg -> Log.warn $ "builtin inst db: " ++ msg
    let (merged, overlaps) = MidiDb.merge builtin loaded
    unless (null overlaps) $
        Log.warn $ "overlapping instruments discarded while merging: "
            ++ show (map (\(Score.Instrument inst) -> inst) overlaps)
    return $ Db.db merged index

load_db :: FilePath -> IO ([MidiDb.SynthDesc], Search.Index)
load_db app_dir = do
    saved <- Serialize.unserialize
        (app_dir </> Config.instrument_db_cache)
    case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db: " ++ show err
            return ([], Search.empty_index)
        Right (Serialize.SavedDb _ (MidiDb.SerializableMidiDb synth_descs)
                index) ->
            return (synth_descs, index)

-- | Load instruments that take longer, and should be cached by make_db.
load_slow :: FilePath -> IO [MidiDb.SynthDesc]
load_slow dir = mapM ($dir) [Z1.load_slow, Vl1m.load_slow]
