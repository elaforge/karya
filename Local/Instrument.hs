-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Load the instrument db.  This collects together all the local instrument
    definitions.

    The convention is that each synthesizer has a module in Local\/Instrument\/,
    and each one exports a @load :: 'Load'@, and possibly @make_db :: 'MakeDb'@.

    "Instrument.MakeDb" is used to run the @make_db@s.
-}
module Local.Instrument (Load, MakeDb, load, make_dbs, make_named_dbs) where
import qualified Data.List as List
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
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
import Global


-- | By convention, instrument definition modules export a function called
-- @load@, with this signature.  The FilePath is the 'Config.instrument_dir',
-- could hold cached instruments, as created by 'MakeDb'.
type Load = FilePath -> IO (Maybe MidiInst.Synth)

-- | Some synths may require a more expensive load, e.g. they could parse
-- a directory full of sysex dumps.  These expose a @make_db@ function with
-- this type.  As with 'Load', the FilePath is 'Config.instrument_dir'.  The
-- function is expected to do its work and save the results in the instrument
-- dir
--
-- You should use 'Cmd.Instrument.MidiInst.save_db', which will put the file
-- into 'Config.instrument_cache_dir' with the same name as the synth.
type MakeDb = FilePath -> IO ()

-- | Load functions for each synthesizer.
all_loads :: [Load]
all_loads =
    [ Drumaxx.load, Fm8.load, Kontakt.load, Massive.load, Morpheus.load
    , Morphine.load, Pianoteq.load, Reaktor.load, Spicy.load, Tassman.load
    , Vl1.load, Vsl.load, Z1.load
    ]

-- | make_db functions for each synthesizer that needs more elaborate setup,
-- i.e. loading patches from sysexes.
all_make_dbs :: [(String, MakeDb)]
all_make_dbs =
    [ (Morpheus.synth_name, Morpheus.make_db)
    , (Vl1.synth_name, Vl1.make_db)
    , (Z1.synth_name, Z1.make_db)
    ]

load :: FilePath -> IO Cmd.InstrumentDb
load app_dir = do
    synths <- mapMaybeM ($ Config.make_path app_dir Config.instrument_dir)
        all_loads
    let annot_fn = Config.make_path app_dir Config.local_dir
            </> "instrument_annotations"
    annots <- Parse.parse_annotations annot_fn >>= \x -> case x of
        -- The parsec error already includes the filename.
        Left err -> Log.warn (txt err) >> return mempty
        Right annots -> return annots
    let (midi_db, warns) = MidiDb.midi_db synths
    forM_ warns $ \msg -> Log.warn $ "inst db: " <> msg
    (midi_db, not_found) <- return $ MidiDb.annotate annots midi_db
    unless (null not_found) $
        Log.warn $ "annotated instruments not found: " <> pretty not_found
    return $ Db.db midi_db

-- | Call all 'MakeDb' functions.
make_dbs :: FilePath -> IO ()
make_dbs app_dir = mapM_ ($ Config.make_path app_dir Config.instrument_dir)
    [Morpheus.make_db, Vl1.make_db, Z1.make_db]

-- | Call 'MakeDb' functions only for the given synths.
make_named_dbs :: [String] -> FilePath -> IO ()
make_named_dbs names app_dir
    | null unmatched =
        mapM_ ($ Config.make_path app_dir Config.instrument_dir)
            [make | (name, make) <- all_make_dbs, name `elem` names]
    | otherwise = errorIO $ "unmatched dbs: " ++ show unmatched
        ++ ", understood dbs: " ++ show (map fst all_make_dbs)
    where unmatched = names List.\\ map fst all_make_dbs
