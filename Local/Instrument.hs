-- | Load the instrument db.  This collects together all the local instrument
-- definitions.
--
-- TODO the 'load' and 'make_dbs' calls should be automatically generated from
-- the contents of the Local/Instrument/ dir.
module Local.Instrument where
import System.FilePath ((</>))

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Cmd.Cmd as Cmd
import qualified Instrument.Db as Db
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Parse as Parse

import qualified Local.Instrument.Drumaxx as Drumaxx
import qualified Local.Instrument.Fm8 as Fm8
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Massive as Massive
import qualified Local.Instrument.Morpheus as Morpheus
import qualified Local.Instrument.Morphine as Morphine
import qualified Local.Instrument.Pianoteq as Pianoteq
import qualified Local.Instrument.Reaktor as Reaktor
import qualified Local.Instrument.Spicy as Spicy
import qualified Local.Instrument.Tassman as Tassman
import qualified Local.Instrument.Vl1 as Vl1
import qualified Local.Instrument.Vsl as Vsl
import qualified Local.Instrument.Z1 as Z1

import qualified App.Config as Config


-- | Load functions for each synthesizer.
synths :: [FilePath -> IO [Cmd.SynthDesc]]
synths =
    [ Drumaxx.load, Fm8.load, Kontakt.load, Massive.load, Morpheus.load
    , Morphine.load, Pianoteq.load, Reaktor.load, Spicy.load, Tassman.load
    , Vl1.load, Vsl.load, Z1.load
    ]

-- | make_db functions for each synthesizer that needs more elaborate setup,
-- i.e. loading patches from sysexes.
dbs :: [(String, FilePath -> IO ())]
dbs =
    [ (Morpheus.synth_name, Morpheus.make_db)
    , (Vl1.synth_name, Vl1.make_db)
    , (Z1.synth_name, Z1.make_db)
    ]


load :: FilePath -> IO Cmd.InstrumentDb
load app_dir = do
    synth_descs <- concatMapM
        ($ Config.make_path app_dir Config.instrument_dir) synths
    let annot_fn = Config.make_path app_dir Config.local_dir
            </> "instrument_annotations"
    annots <- Parse.parse_annotations annot_fn >>= \x -> case x of
        -- The parsec error already includes the filename.
        Left err -> Log.warn err >> return mempty
        Right annots -> return annots
    let (midi_db, warns) = MidiDb.midi_db synth_descs
    forM_ warns $ \msg -> Log.warn $ "inst db: " ++ msg
    (midi_db, not_found) <- return $ MidiDb.annotate annots midi_db
    unless (null not_found) $
        Log.warn $ "annotated instruments not found: "
            ++ Pretty.pretty not_found
    return $ Db.db midi_db

make_dbs :: FilePath -> IO ()
make_dbs app_dir = mapM_ ($ Config.make_path app_dir Config.instrument_dir)
    [Morpheus.make_db, Vl1.make_db, Z1.make_db]

make_named_dbs :: [String] -> FilePath -> IO ()
make_named_dbs names app_dir =
    mapM_ ($ Config.make_path app_dir Config.instrument_dir)
        [make | (name, make) <- dbs, name `elem` names]
