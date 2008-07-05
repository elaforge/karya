import System.FilePath ((</>))

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize as Serialize
import qualified Instrument.Search as Search

import qualified Local.Instrument
import qualified App.Config as Config


main :: IO ()
main = do
    app_dir <- Config.get_app_dir
    let fn = app_dir </> Config.instrument_db_cache
    midi_db <- Local.Instrument.load_slow (app_dir </> Config.instrument_dir)
    Serialize.serialize fn midi_db (Search.make_index midi_db)
    putStrLn $ "Wrote " ++ show (MidiDb.size midi_db) ++ " records to "
        ++ show fn ++ "."
