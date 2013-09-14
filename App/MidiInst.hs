-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the instrument definitions in Local/Instrument.
module App.MidiInst (
    SynthDesc
    , Softsynth(..), softsynth
    , Patch
    , make
    -- * code
    , Code(..), empty_code, with_code, with_empty_code
    , environ
    , NoteCall, generator, transformer, both, note_calls
    , note_generators, note_transformers, null_call
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
import qualified Derive.Call.Make as Make
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score
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
    , synth_doc :: Text
    , pb_range :: Control.PbRange
    , controls :: [(Midi.Control, Score.Control)]
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
softsynth :: Instrument.SynthName -> Text -> Control.PbRange
    -> [(Midi.Control, Score.Control)] -> Softsynth
softsynth name doc pb_range controls =
    Softsynth name doc pb_range controls [] id empty_code

make :: Softsynth -> [SynthDesc]
make (Softsynth name doc pb_range controls extra_patches modify_wildcard code)
    = [(synth, pmap <> extra)]
    where
    (extra, _) = MidiDb.patch_map (map (second make_code) extra_patches)
    (synth, wildcard_patch) =
        Instrument.make_softsynth name doc pb_range controls
    pmap = MidiDb.wildcard_patch_map
        (modify_wildcard wildcard_patch, make_code code)

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    code_note_generators :: [Derive.LookupCall (Derive.Generator Derive.Note)]
    , code_note_transformers ::
        [Derive.LookupCall (Derive.Transformer Derive.Note)]
    , code_val_calls :: [Derive.LookupCall Derive.ValCall]
    , code_environ :: TrackLang.Environ
    , code_cmds :: [Cmd.Cmd]
    }

instance Monoid.Monoid Code where
    mempty = Code [] [] [] mempty []
    mappend (Code a1 b1 c1 d1 e1) (Code a2 b2 c2 d2 e2) =
        Code (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2) (e1<>e2)

empty_code :: Code
empty_code = mempty

with_code :: Code -> [Instrument.Patch] -> [Patch]
with_code code = map (\p -> (p, code))

with_empty_code :: [Instrument.Patch] -> [Patch]
with_empty_code = with_code empty_code

make_code :: Code -> Cmd.InstrumentCode
make_code (Code generator transformer val environ cmds) =
    Cmd.InstrumentCode (Derive.InstrumentCalls generator transformer val)
        environ cmds

-- ** code constructors

-- | Bundle together generators and transformers.  The rationale is described
-- in 'Derive.CallMaps'.
data Call d =
    Generator Text (Derive.Generator d)
    | Transformer Text (Derive.Transformer d)
    | Both Text (Derive.Generator d) (Derive.Transformer d)

type NoteCall = Call Derive.Note

generator :: Text -> Derive.Generator d -> Call d
generator = Generator

transformer :: Text -> Derive.Transformer d -> Call d
transformer = Transformer

both :: Text -> Make.Calls d -> Call d
both name (g, t) = Both name g t

note_calls :: [Call Derive.Note] -> Code
note_calls calls =
    note_generators ([(name, c) | Generator name c <- calls]
        ++ [(name, c) | Both name c _ <- calls])
    <> note_transformers ([(name, c) | Transformer name c <- calls]
        ++ [(name, c) | Both name _ c <- calls])

-- | Add the given calls to the note track scope.
note_generators :: [(Text, Derive.Generator Derive.Note)] -> Code
note_generators calls = mempty
    { code_note_generators = [Derive.map_lookup (Derive.make_calls calls)] }

-- | Add the given calls to the note track scope.
note_transformers :: [(Text, Derive.Transformer Derive.Note)] -> Code
note_transformers calls = mempty
    { code_note_transformers = [Derive.map_lookup (Derive.make_calls calls)] }

-- | Add the given call as the null note call to the note track.  This also
-- binds @n@, since @n@ is supposed to be the \"named\" way to call \"\".
null_call :: Derive.Generator Derive.Note -> [NoteCall]
null_call call = [generator "" call, generator "n" call]

cmd :: Cmd.Cmd -> Code
cmd c = mempty { code_cmds = [c] }

-- | The instrument will also set the given environ when it comes into scope.
environ :: (TrackLang.Typecheck a) => TrackLang.ValName -> a -> Code
environ name val = mempty
    { code_environ = TrackLang.make_environ [(name, TrackLang.to_val val)] }

-- | The instrument will set the given scale when it comes into scope.
default_scale :: Pitch.ScaleId -> Code
default_scale = environ Environ.scale . TrackLang.scale_id_to_sym

-- * making patches

-- | Make a patch, with a few parameters that tend to be unique per patch.
patch :: Control.PbRange -> Instrument.InstrumentName
    -> [(Midi.Control, Score.Control)] -> Instrument.Patch
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
save_db synths db_name app_dir =
    Serialize.serialize (db_path app_dir db_name) synths

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
    let fname = db_path app_dir db_name
    saved <- Serialize.unserialize (make_code . code_for) fname
    case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db " ++ show fname ++ ": "
                ++ Seq.strip err
            return []
        Right (_time, synths) -> return synths

db_path :: FilePath -> FilePath -> FilePath
db_path app_dir name =
    Config.make_path app_dir Config.instrument_cache_dir </> name <.> "db"
