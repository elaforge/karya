-- | Utilities for the instrument definitions in Local/Instrument.
module App.MidiInst (
    make
    , SynthDesc
    , Softsynth(..), softsynth
    , Code(..), empty_code, with_code, with_empty_code

    -- * db
    , save_db, save_patches, load_db
) where
import System.FilePath ((</>), (<.>))
import Util.Control
import qualified Util.Log as Log

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import Cmd.Cmd (SynthDesc)

import qualified Derive.Derive as Derive

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize as Serialize

import qualified App.Config as Config


-- | The arguments for the 'softsynth' function in record form, for default
-- parameters.
data Softsynth = Softsynth {
    name :: Instrument.SynthName
    , device :: Maybe String
    , pb_range :: Control.PbRange
    , controls :: [(Midi.Control, String)]
    , extra_patches :: [(Instrument.Patch, Code)]
    , modify_patch :: Instrument.Patch -> Instrument.Patch
    , code :: Code
    }

-- | Utility to construct a soft synth.  Soft synths are assumed to have
-- their own internal patch management, and thus have only a single wildcard
-- patch, which can be modified if necessary by a passed in function.  In case
-- some patches are special, you can also pass named patches in to be merged.
softsynth :: Instrument.SynthName -> Maybe String -> Control.PbRange
    -> [(Midi.Control, String)] -> Softsynth
softsynth name device pb_range controls =
    Softsynth name device pb_range controls [] id empty_code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    note_calls :: [Derive.LookupCall Derive.NoteCall]
    , val_calls :: [Derive.LookupCall Derive.ValCall]
    , cmds :: [Cmd.Cmd]
    }

empty_code :: Code
empty_code = Code [] [] []

with_code :: Code -> [Instrument.Patch] -> [(Instrument.Patch, Code)]
with_code code = map (\p -> (p, code))

with_empty_code :: [Instrument.Patch] -> [(Instrument.Patch, Code)]
with_empty_code = with_code empty_code

make :: Softsynth -> [SynthDesc]
make (Softsynth name device pb_range controls extra_patches modify_patch code) =
    [(synth, pmap <> extra)]
    where
    (extra, _) = MidiDb.patch_map (map (second make_code) extra_patches)
    (synth, wildcard_patch) =
        Instrument.make_softsynth name device pb_range controls
    pmap = MidiDb.wildcard_patch_map
        (modify_patch wildcard_patch, make_code code)

make_code :: Code -> Cmd.InstrumentCode
make_code (Code note val cmds) =
    Cmd.InstrumentCode (Derive.InstrumentCalls note val) cmds


-- * db

-- | Some instruments want to load their patches in elaborate slow ways, like
-- parsing a directory full of sysexes.  These patches can export a @make_db@
-- function, which will do the slow parts and save the results in a cache file.
-- The @load@ function will simply read the cache file, if present.
save_db :: [MidiDb.SynthDesc code] -> FilePath -> FilePath -> IO ()
save_db synths db_name app_dir = Serialize.serialize
    (app_dir </> Config.instrument_cache_dir </> db_name <.> "db") synths

-- | Specialized version of 'save_db' that takes a list of Patches.
save_patches :: Instrument.Synth -> [Instrument.Patch] -> FilePath -> FilePath
    -> IO ()
save_patches synth patches db_name app_dir = do
    -- SynthDesc is expected to have a 'code' parameter, even though
    -- 'serialize' is about to strip it off.  So put on a fake one.  It seems
    -- bogus, but otherwise I'd need two versions of 'MidiDb.patch_map' and
    -- 'logged_synths'.
    sdesc <- MidiDb.logged_synths synth (map (\p -> (p, ())) patches)
    save_db [sdesc] db_name app_dir

load_db :: (Instrument.Patch -> Code) -> FilePath -> FilePath -> IO [SynthDesc]
load_db code_for db_name app_dir = do
    saved <- Serialize.unserialize (make_code . code_for)
        (app_dir </> Config.instrument_cache_dir </> db_name <.> "db")
    case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db: " ++ err
            return []
        Right (_time, synths) -> return synths
