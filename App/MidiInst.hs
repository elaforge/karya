-- | Utilities for the instrument definitions in Local/Instrument.
module App.MidiInst (
    SynthDesc
    , Softsynth(..), softsynth
    , Patch
    , make
    -- * code
    , Code(..), empty_code, with_code, with_empty_code
    , environ, note_calls, null_call
    , cmd
    , default_scale

    -- * making patches
    , patch, pressure

    -- * db
    , save_db, save_patches, load_db
) where
import qualified Data.Monoid as Monoid
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
    -- | Add explicit non-wildcard patches.
    , extra_patches :: [Patch]
    -- | Configure the wildcard patch.
    , modify_wildcard :: Instrument.Patch -> Instrument.Patch
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
make (Softsynth name pb_range controls extra_patches modify_wildcard code)
    = [(synth, pmap <> extra)]
    where
    (extra, _) = MidiDb.patch_map (map (second make_code) extra_patches)
    (synth, wildcard_patch) =
        Instrument.make_softsynth name pb_range controls
    pmap = MidiDb.wildcard_patch_map
        (modify_wildcard wildcard_patch, make_code code)

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    code_note_calls :: [Derive.LookupCall Derive.NoteCall]
    , code_val_calls :: [Derive.LookupCall Derive.ValCall]
    , code_environ :: TrackLang.Environ
    , code_cmds :: [Cmd.Cmd]
    }

instance Monoid.Monoid Code where
    mempty = Code [] [] mempty []
    mappend (Code a1 b1 c1 d1) (Code a2 b2 c2 d2) =
        Code (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2)

empty_code :: Code
empty_code = Code [] [] mempty []

with_code :: Code -> [Instrument.Patch] -> [Patch]
with_code code = map (\p -> (p, code))

with_empty_code :: [Instrument.Patch] -> [Patch]
with_empty_code = with_code empty_code

make_code :: Code -> Cmd.InstrumentCode
make_code (Code note val environ cmds) =
    Cmd.InstrumentCode (Derive.InstrumentCalls note val) environ cmds

-- ** code constructors

-- | Add the given calls to the note track scope.
note_calls :: [(String, Derive.NoteCall)] -> Code
note_calls calls = mempty
    { code_note_calls = [Derive.map_lookup (Derive.make_calls calls)] }

-- | Add the given call as the null note call to the note track.  This also
-- binds @n@, since @n@ is supposed to be the \"named\" way to call \"\".
null_call :: Derive.NoteCall -> Code
null_call call = note_calls [("", call), ("n", call)]

cmd :: Cmd.Cmd -> Code
cmd c = mempty { code_cmds = [c] }

-- | The instrument will also set the given environ when it comes into scope.
environ :: (TrackLang.Typecheck a) => TrackLang.ValName -> a -> Code
environ name val = mempty
    { code_environ = TrackLang.make_environ [(name, TrackLang.to_val val)] }

-- | The instrument will set the given scale when it comes into scope.
default_scale :: Pitch.ScaleId -> Code
default_scale = environ TrackLang.v_scale

-- * making patches

-- | Make a patch, with a few parameters that tend to be unique per patch.
patch :: Control.PbRange -> Instrument.InstrumentName
    -> [(Midi.Control, String)] -> Instrument.Patch
patch pb_range name controls =
    Instrument.patch (Instrument.instrument name controls pb_range)

-- | Set a patch to pressure control.
pressure :: Instrument.Patch -> Instrument.Patch
pressure = Instrument.set_decay 0 . Instrument.set_flag Instrument.Pressure


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
