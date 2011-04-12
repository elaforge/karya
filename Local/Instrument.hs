-- | Load the instrument db.  This collects together all the local instrument
-- definitions.
--
-- TODO the 'load' and 'make_dbs' calls should be automatically generated from
-- the contents of the Local/Instrument/ dir.
module Local.Instrument where
import Control.Monad
import System.FilePath ((</>))

import Util.Control
import qualified Util.Log as Log

import qualified Local.Instrument.Drumaxx as Drumaxx
import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Morphine as Morphine
import qualified Local.Instrument.Pianoteq as Pianoteq
import qualified Local.Instrument.Reaktor as Reaktor
import qualified Local.Instrument.Tassman as Tassman
import qualified Local.Instrument.Vl1m as Vl1m
import qualified Local.Instrument.Z1 as Z1

import qualified Cmd.Cmd as Cmd
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config


load :: FilePath -> IO Cmd.InstrumentDb
load app_dir = do
    synth_descs <- concat <$> mapM ($ app_dir </> Config.instrument_dir)
        [ Drumaxx.load, Fm8.load, Kontakt.load, Morphine.load, Pianoteq.load
        , Reaktor.load, Tassman.load, Vl1m.load, Z1.load
        ]
    let (midi_db, warns) = MidiDb.midi_db synth_descs
    forM_ warns $ \msg -> Log.warn $ "inst db: " ++ msg
    return $ Db.db midi_db

make_dbs :: FilePath -> IO ()
make_dbs dir = mapM_ ($ dir </> Config.instrument_dir)
    [Vl1m.make_db, Z1.make_db]
