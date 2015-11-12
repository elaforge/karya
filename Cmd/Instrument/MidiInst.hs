-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the instrument definitions in "Local.Instrument".
module Cmd.Instrument.MidiInst (
    Synth, Patch
    , with_patches
    -- * code
    , Code(..), empty_code, with_code, with_empty_code
    , Call
    , generator, transformer, both, note_calls
    , note_generators, note_transformers, null_call
    , postproc, cmd

    -- * making patches
    , patch, pressure
    -- ** environ
    , environ, default_scale, range, nn_range

    -- * db
    , save_synth, load_synth
) where
import qualified Data.Text as Text
import System.FilePath ((</>), (<.>))

import qualified Util.Log as Log
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Make as Make
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Serialize
import qualified App.Config as Config
import Global


type Synth = Instrument.Synth Cmd.InstrumentCode
type Patch = (Instrument.Patch, Code)

with_patches :: [(Instrument.Patch, Code)] -> Instrument.Synth a -> Synth
with_patches patches synth = synth { Instrument.synth_patches = verified }
    where
    -- Since these patches are likely defined by hand, I assume there are no
    -- name collisions to be logged.
    (verified, _logs) = MidiDb.verify_patches $ map (second make_code) patches

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    code_note_generators :: [Derive.LookupCall (Derive.Generator Derive.Note)]
    , code_note_transformers ::
        [Derive.LookupCall (Derive.Transformer Derive.Note)]
    , code_val_calls :: [Derive.LookupCall Derive.ValCall]
    , code_postproc :: Cmd.InstrumentPostproc
    , code_cmds :: [Cmd.Cmd]
    }

instance Monoid Code where
    mempty = Code [] [] mempty id []
    mappend (Code g1 t1 v1 post1 cmds1) (Code g2 t2 v2 post2 cmds2) =
        Code (g1<>g2) (t1<>t2) (v1<>v2) (post1 . post2) (cmds1<>cmds2)

empty_code :: Code
empty_code = mempty

with_code :: Code -> Instrument.Patch -> Patch
with_code code patch = (patch, code)

with_empty_code :: Instrument.Patch -> Patch
with_empty_code = with_code empty_code

make_code :: Code -> Cmd.InstrumentCode
make_code (Code generator transformer val postproc cmds) = Cmd.InstrumentCode
    { Cmd.inst_calls = Derive.InstrumentCalls generator transformer val
    , Cmd.inst_postproc = postproc
    , Cmd.inst_cmds = cmds
    }

-- ** code constructors

-- | Bundle together generators and transformers.  The rationale is described
-- in 'Derive.CallMaps'.
data Call d =
    Generator TrackLang.CallId (Derive.Generator d)
    | Transformer TrackLang.CallId (Derive.Transformer d)
    | Both TrackLang.CallId (Derive.Generator d) (Derive.Transformer d)

generator :: TrackLang.CallId -> Derive.Generator d -> Call d
generator = Generator

transformer :: TrackLang.CallId -> Derive.Transformer d -> Call d
transformer = Transformer

both :: TrackLang.CallId -> Make.Calls d -> Call d
both name (g, t) = Both name g t

-- | Add the given call as the null note call to the note track.  This also
-- binds @n@, since @n@ is supposed to be the \"named\" way to call \"\".
null_call :: Derive.Generator Derive.Note -> [Call Derive.Note]
null_call call = [generator "" call, generator "n" call]

note_calls :: [Call Derive.Note] -> Code
note_calls calls =
    note_generators ([(name, c) | Generator name c <- calls]
        ++ [(name, c) | Both name c _ <- calls])
    <> note_transformers ([(name, c) | Transformer name c <- calls]
        ++ [(name, c) | Both name _ c <- calls])

-- | Add the given calls to the note track scope.
note_generators :: [(TrackLang.CallId, Derive.Generator Derive.Note)] -> Code
note_generators calls = mempty { code_note_generators = Derive.call_map calls }

-- | Add the given calls to the note track scope.
note_transformers :: [(TrackLang.CallId, Derive.Transformer Derive.Note)]
    -> Code
note_transformers calls =
    mempty { code_note_transformers = Derive.call_map calls }

postproc :: Cmd.InstrumentPostproc -> Code
postproc post = mempty { code_postproc = post }

cmd :: Cmd.Cmd -> Code
cmd c = mempty { code_cmds = [c] }

-- * making patches

-- | Make a patch, with a few parameters that tend to be unique per patch.
-- Controls come last because they are often a long list.
patch :: Control.PbRange -> Instrument.InstrumentName
    -> [(Midi.Control, Score.Control)] -> Instrument.Patch
patch pb_range name controls =
    Instrument.patch (Instrument.instrument pb_range name controls)

-- | Set a patch to pressure control.
pressure :: Instrument.Patch -> Instrument.Patch
pressure = Instrument.set_decay 0 . Instrument.set_flag Instrument.Pressure

-- ** environ

-- | The instrument will also set the given environ when it comes into scope.
environ :: RestrictedEnviron.ToVal a => Env.Key -> a -> Instrument.Patch
    -> Instrument.Patch
environ name val = Instrument.environ
    %= (RestrictedEnviron.make [(name, RestrictedEnviron.to_val val)] <>)

-- | The instrument will set the given scale when it comes into scope.
default_scale :: Pitch.ScaleId -> Instrument.Patch -> Instrument.Patch
default_scale = environ EnvKey.scale . TrackLang.scale_id_to_sym

-- | Set instrument range.
range :: Scale.Range -> Instrument.Patch -> Instrument.Patch
range range = environ EnvKey.instrument_bottom (Scale.range_bottom range)
    . environ EnvKey.instrument_top (Scale.range_top range)

nn_range :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Instrument.Patch
    -> Instrument.Patch
nn_range (bottom, top) = environ EnvKey.instrument_bottom bottom
    . environ EnvKey.instrument_top top


-- * db

-- | Some instruments want to load their patches in elaborate slow ways, like
-- parsing a directory full of sysexes.  These patches can export a @make_db@
-- function, which will do the slow parts and save the results in a cache file.
-- The @load@ function will simply read the cache file, if present.
--
-- The Synth and its patches are passed separately.  This is because there
-- may well be patches with duplicate names, and so I need to run
-- 'MidiDb.verify_patches' before saving.
save_synth :: FilePath -> Instrument.Synth a -> [Instrument.Patch]
    -> IO ()
save_synth app_dir synth patches = do
    let (verified, logs) = MidiDb.verify_patches (map (, ()) patches)
    let synth_name = Instrument.synth_name synth
    mapM_ (Log.notice . (("synth " <> synth_name <> ": ") <>)) logs
    Instrument.Serialize.serialize (db_path app_dir (untxt synth_name)) $
        Instrument.modify_code (const ()) $
        synth { Instrument.synth_patches = verified }

load_synth :: (Instrument.Patch -> Code) -> FilePath -> FilePath
    -> IO (Maybe Synth)
load_synth code_for db_name app_dir = do
    let fname = db_path app_dir db_name
    saved <- Instrument.Serialize.unserialize (make_code . code_for) fname
    case saved of
        Left err -> do
            Log.warn $ "Error loading instrument db " <> showt fname <> ": "
                <> Text.strip err
            return Nothing
        Right (_time, synth) -> return (Just synth)

db_path :: FilePath -> FilePath -> FilePath
db_path app_dir name =
    Config.make_path app_dir Config.instrument_cache_dir </> name <.> "db"
