import System.FilePath ((</>))
import Control.Monad
import qualified Util.Log as Log

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize as Serialize
import qualified Instrument.Search as Search

import qualified Local.Instrument
import qualified App.Config as Config


main :: IO ()
main = do
    app_dir <- Config.get_app_dir
    let fn = app_dir </> Config.instrument_db_cache
    synth_descs <- Local.Instrument.load_slow
        (app_dir </> Config.instrument_dir)

    let (midi_db, warns) = MidiDb.midi_db synth_descs
    forM_ warns $ \msg -> Log.warn msg

    -- It's sort of bogus how 'make_index' wants a MidiDb instead of
    -- [SynthDesc], but oh well.
    Serialize.serialize fn (MidiDb.SerializableMidiDb synth_descs)
        (Search.make_index midi_db)
    putStrLn $ "Wrote " ++ show (MidiDb.size midi_db) ++ " records to "
        ++ show fn ++ "."
