-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the instrument definitions in "Local.Instrument".
module Cmd.Instrument.MidiInst (
    Synth, synth
    , make_inst
    -- * code
    , make_code
    , Code(..), Call
    , generator, transformer, both, note_calls
    , note_generators, note_transformers, val_calls
    , null_call, null_calls
    , postproc, cmd, thru

    -- * Patch
    , Patch(..), patch, common
    , make_patch, patch_from_pair, named_patch, default_patch
    -- ** modify
    , code, doc, attribute_map, decay, synth_controls
    , add_flag, add_flags, pressure, add_common_flag, triggered
    , control_defaults
    -- ** environ
    , environ, default_scale, range, nn_range
    -- ** per-allocation
    , inst_range
    -- * allocations
    , allocations, config, config1

    -- * types
    , Load, MakeDb
) where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty

import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.REnv as REnv
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Symbols as Symbols

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


type Synth = Inst.SynthDecl Cmd.InstrumentCode

synth :: InstT.SynthName -> Text -> [Patch] -> Synth
synth name doc patches =
    Inst.SynthDecl name doc (zip (map name_of patches) (map make_inst patches))
    where name_of = (patch#Patch.name #$)

make_inst :: Patch -> Inst.Inst Cmd.InstrumentCode
make_inst (Patch patch common) = Inst.Inst
    { inst_backend = Inst.Midi patch
    , inst_common = common
        { Common.common_code = make_code (Common.common_code common) }
    }

make_code :: Code -> Cmd.InstrumentCode
make_code (Code library postproc cmds thru) = Cmd.InstrumentCode
    { inst_calls = compile_library library
    , inst_postproc = postproc
    , inst_cmds = cmds
    , inst_thru = thru
    }

-- | InstrumentCalls doesn't have modules, so just pull everything out of every
-- module.
compile_library :: Library.Library -> Derive.InstrumentCalls
compile_library = convert . fst . Library.compile
    where
    convert (Derive.Scopes gen trans track val) = Derive.Scopes
        { scopes_generator = extract $ Derive.scope_note gen
        , scopes_transformer = extract $ Derive.scope_note trans
        , scopes_track = extract $ Derive.scope_note track
        , scopes_val = extract val
        }
    -- TODO this doesn't warn about shadowed entries, but I'd have to extend
    -- 'synth' to be in LogMonad.
    extract :: Map Module.Module (Derive.CallMap a) -> Derive.CallMap a
    extract = mconcat . Map.elems

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    code_library :: !Library.Library
    , code_postproc :: !Cmd.InstrumentPostproc
    , code_cmds :: ![Cmd.HandlerId]
    , code_thru :: !(Maybe Cmd.ThruFunction)
    }

instance Pretty Code where
    format (Code library _postproc cmds thru) =
        Pretty.record "Code"
            [ ("library", Pretty.format library)
            , ("cmds", Pretty.format $ length cmds)
            , ("thru", Pretty.format thru)
            ]

instance Semigroup Code where
    (<>)    (Code lib1 post1 cmds1 thru1)
            (Code lib2 post2 cmds2 thru2) =
        Code (lib1<>lib2) (merge post1 post2) (cmds1<>cmds2) (thru1<|>thru2)
instance Monoid Code where
    mempty = Code mempty (,[]) [] Nothing
    mappend = (<>)

merge :: (b -> (c, [log])) -> (a -> (b, [log])) -> (a -> (c, [log]))
merge f1 f2 = (\(b, logs) -> (logs++) <$> f1 b) . f2

-- ** code constructors

-- | Bundle together generators and transformers.  The rationale is described
-- in 'Derive.CallMaps'.
data Call d =
    Generator Expr.Symbol (Derive.Generator d)
    | Transformer Expr.Symbol (Derive.Transformer d)
    | Both Expr.Symbol (Derive.Generator d) (Derive.Transformer d)

generator :: Expr.Symbol -> Derive.Generator d -> Call d
generator = Generator

transformer :: Expr.Symbol -> Derive.Transformer d -> Call d
transformer = Transformer

both :: Expr.Symbol -> Library.Calls d -> Call d
both name calls =
    Both name (Library.generator calls) (Library.transformer calls)

-- | Add the given call as the null note call to the note track.  This also
-- binds 'Symbols.default_note', since that's supposed to be the \"named\" way
-- to call \"\".
null_call :: Derive.Generator Derive.Note -> Code
null_call = note_calls . null_calls

null_calls :: Derive.Generator Derive.Note -> [Call Derive.Note]
null_calls call =
    [ generator Symbols.null_note call
    , generator Symbols.default_note call
    ]

note_calls :: [Call Derive.Note] -> Code
note_calls calls =
    note_generators ([(name, c) | Generator name c <- calls]
        ++ [(name, c) | Both name c _ <- calls])
    <> note_transformers ([(name, c) | Transformer name c <- calls]
        ++ [(name, c) | Both name _ c <- calls])

-- | Add the given calls to the note track scope.
note_generators :: [(Expr.Symbol, Derive.Generator Derive.Note)] -> Code
note_generators calls = mempty { code_library = Library.generators calls }

-- | Add the given calls to the note track scope.
note_transformers :: [(Expr.Symbol, Derive.Transformer Derive.Note)] -> Code
note_transformers calls = mempty { code_library = Library.transformers calls }

val_calls :: [(Expr.Symbol, Derive.ValCall)] -> Code
val_calls calls = mempty { code_library = Library.vals calls }

postproc :: Cmd.InstrumentPostproc -> Code
postproc post = mempty { code_postproc = post }

cmd :: Cmd.HandlerId -> Code
cmd c = mempty { code_cmds = [c] }

thru :: Cmd.ThruFunction -> Code
thru f = mempty { code_thru = Just f }

-- * Patch

data Patch = Patch {
    patch_patch :: Patch.Patch
    , patch_common :: Common.Common Code
    }

patch = Lens.lens patch_patch (\f r -> r { patch_patch = f (patch_patch r) })
common = Lens.lens patch_common
    (\f r -> r { patch_common = f (patch_common r) })

instance Pretty Patch where
    format (Patch patch common) = Pretty.record "Patch"
        [ ("patch", Pretty.format patch)
        , ("common", Pretty.format common)
        ]

make_patch :: Patch.Patch -> Patch
make_patch p = Patch
    { patch_patch = p
    , patch_common = Common.common mempty
    }

-- | Convert patches as emitted by 'Patch.Sysex.Patch'.
patch_from_pair :: (Patch.Patch, Common.Common ()) -> Patch
patch_from_pair (patch, common) = (make_patch patch)
    { patch_common = common { Common.common_code = mempty } }

-- | Make a patch, with a few parameters that tend to be unique per patch.
-- Controls come last because they are often a long list.
--
-- TODO I don't love the name, but 'patch' is already taken by the lens.
named_patch :: Control.PbRange -> InstT.Name
    -> [(Midi.Control, ScoreT.Control)] -> Patch
named_patch pb_range name controls =
    make_patch $ (Patch.patch pb_range name)
        { Patch.patch_control_map = Control.control_map controls }

-- | Make a default patch for the synth.
default_patch :: Control.PbRange -> [(Midi.Control, ScoreT.Control)] -> Patch
default_patch pb_range controls = Patch
    { patch_patch = (Patch.patch pb_range Patch.default_name)
        { Patch.patch_control_map = Control.control_map controls }
    , patch_common = Common.common mempty
    }

-- ** modify

code :: Lens Patch Code
code = common # Common.code

doc :: Lens Patch Doc.Doc
doc = common # Common.doc

attribute_map :: Lens Patch Patch.AttributeMap
attribute_map = patch # Patch.attribute_map

decay :: Lens Patch (Maybe RealTime)
decay = patch # Patch.defaults # Patch.decay

-- | Annotate all the patches with some global controls.
synth_controls :: [(Midi.Control, ScoreT.Control)] -> [Patch] -> [Patch]
synth_controls controls = map $
    patch # Patch.control_map %= (Control.control_map controls <>)

add_flag :: Patch.Flag -> Patch.Patch -> Patch.Patch
add_flag flag =
    Patch.defaults#Patch.flags %= Just . Patch.add_flag flag . fromMaybe mempty

add_flags :: [Patch.Flag] -> Patch.Patch -> Patch.Patch
add_flags flags = Patch.defaults#Patch.flags
    %= Just . Set.union (Set.fromList flags) . fromMaybe mempty

-- | Set a patch to pressure control.
pressure :: Patch -> Patch
pressure = patch %= (Patch.defaults#Patch.decay #= Just 0)
    . add_flag Patch.Pressure

add_common_flag :: Common.Flag -> Patch -> Patch
add_common_flag flag = common#Common.flags %= Set.insert flag

triggered :: Patch -> Patch
triggered = add_common_flag Common.Triggered

control_defaults :: [(ScoreT.Control, Signal.Y)] -> Patch -> Patch
control_defaults controls =
    patch#Patch.defaults#Patch.control_defaults #= Just (Map.fromList controls)

-- ** environ

-- | The instrument will also set the given environ when it comes into scope.
environ :: REnv.ToVal a => Env.Key -> a -> Patch -> Patch
environ name val = common %= Common.add_environ name val

-- | The instrument will set the given scale when it comes into scope.
default_scale :: Pitch.ScaleId -> Patch -> Patch
default_scale = environ EnvKey.scale . Expr.scale_id_to_str

-- | Set instrument range.
range :: Scale.Range -> Patch -> Patch
range range = environ EnvKey.instrument_bottom (Scale.range_bottom range)
    . environ EnvKey.instrument_top (Scale.range_top range)

nn_range :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Patch -> Patch
nn_range (bottom, top) = environ EnvKey.instrument_bottom bottom
    . environ EnvKey.instrument_top top

-- ** per-allocation

-- | Like 'range', but set it in the allocation, not the patch.
inst_range :: Scale.Range -> Common.Config -> Common.Config
inst_range range =
    Common.add_cenviron EnvKey.instrument_bottom (Scale.range_bottom range)
    . Common.add_cenviron EnvKey.instrument_top (Scale.range_top range)

-- * Allocations

allocations ::
    [(ScoreT.Instrument, Text,
        Common.Config -> Common.Config, UiConfig.Backend)]
    -- ^ (inst, qualified, set_config, backend)
    -> UiConfig.Allocations
allocations = UiConfig.make_allocations . map make
    where
    make (name, qualified, set_config, backend) =
        ( name
        , UiConfig.Allocation
            { alloc_qualified = InstT.parse_qualified qualified
            , alloc_config = set_config Common.empty_config
            , alloc_backend = backend
            }
        )

-- | Create an incomplete Config.  It's incomplete because it doesn't have
-- the Settings from 'Patch.patch_defaults', so it'll need to have those
-- applied when it gets applied to 'UiConfig.state_allocations'.
config :: [Patch.Addr] -> Patch.Config
config = Patch.config . map (, Nothing)

-- | Specialize 'config' for a single Addr.
config1 :: Midi.WriteDevice -> Midi.Channel -> Patch.Config
config1 dev chan = config [(dev, chan)]


-- * types

-- | Instrument definition modules that need to load from disk export a
-- function called @load@, with this signature.  Use the AppDir to find
-- 'Config.instrument_dir'.
type Load = Path.AppDir -> IO (Maybe Synth)


{- | Some synths may require a more expensive load, e.g. they could parse
    a directory full of sysex dumps.  These expose a @make_db@ function with
    this type.  As with 'Load', the FilePath is 'Config.instrument_dir'.  The
    function is expected to do its work and save the results in the instrument
    dir

    You should use 'Cmd.Instrument.MidiInst.save_synth', which will put the
    file into 'Config.instrument_cache_dir' with the same name as the synth.
-}
type MakeDb = Path.AppDir -> IO ()
