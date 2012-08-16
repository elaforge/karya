-- | Utilities for the instrument definitions in Local/Instrument.
module App.MidiInst (
    SynthDesc
    , Softsynth(..), softsynth
    , Patch
    , make
    -- * code
    , Code(..), empty_code, with_code, with_empty_code
    , default_scale

    -- * db
    , save_db, save_patches, load_db
) where
import qualified Data.Map as Map
import System.FilePath ((</>), (<.>))

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import Cmd.Cmd (SynthDesc)
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize as Serialize
import qualified App.Config as Config


-- | The arguments for the 'softsynth' function in record form, for default
-- parameters.
data Softsynth = Softsynth {
    name :: Instrument.SynthName
    , pb_range :: Control.PbRange
    , controls :: [(Midi.Control, String)]
    , extra_patches :: [Patch]
    , modify_patch :: Instrument.Patch -> Instrument.Patch
    , code :: Code
    }

type Patch = (Instrument.Patch, Code)

-- | Utility to construct a soft synth.  Soft synths are assumed to have
-- their own internal patch management, and thus have only a single wildcard
-- patch, which can be modified if necessary by a passed in function.  In case
-- some patches are special, you can also pass named patches in to be merged.
softsynth :: Instrument.SynthName -> Control.PbRange
    -> [(Midi.Control, String)] -> Softsynth
softsynth name pb_range controls =
    Softsynth name pb_range controls [] id empty_code

make :: Softsynth -> [SynthDesc]
make (Softsynth name pb_range controls extra_patches modify_patch code)
    = [(synth, pmap <> extra)]
    where
    (extra, _) = MidiDb.patch_map (map (second make_code) extra_patches)
    (synth, wildcard_patch) =
        Instrument.make_softsynth name pb_range controls
    pmap = MidiDb.wildcard_patch_map
        (modify_patch wildcard_patch, make_code code)

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    note_calls :: [Derive.LookupCall Derive.NoteCall]
    , val_calls :: [Derive.LookupCall Derive.ValCall]
    , environ :: TrackLang.Environ
    , cmds :: [Cmd.Cmd]
    }

empty_code :: Code
empty_code = Code [] [] mempty []

with_code :: Code -> [Instrument.Patch] -> [Patch]
with_code code = map (\p -> (p, code))

with_empty_code :: [Instrument.Patch] -> [Patch]
with_empty_code = with_code empty_code

default_scale :: Pitch.ScaleId -> Code -> Code
default_scale scale_id code = code
    { environ = Map.singleton (TrackLang.v_scale) (TrackLang.VScaleId scale_id)
    }

make_code :: Code -> Cmd.InstrumentCode
make_code (Code note val environ cmds) =
    Cmd.InstrumentCode (Derive.InstrumentCalls note val) environ cmds


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
    let file = app_dir </> Config.instrument_cache_dir </> db_name <.> "db"
    saved <- Serialize.unserialize (make_code . code_for) file
    case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db " ++ show file ++ ": "
                ++ Seq.strip err
            return []
        Right (_time, synths) -> return synths
