-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the instrument definitions in "Local.Instrument".
module Cmd.Instrument.MidiInst (
    Synth, synth
    , extractWarns
    , makeInst
    -- * code
    , makeCode
    , Code(..), Call
    , generator, transformer, both, noteCalls
    , noteGenerators, noteTransformers, valCalls
    , nullCall, nullCalls
    , postproc, cmd, thru

    -- * Patch
    , Patch(..), patch, common
    , makePatch, patchFromPair, namedPatch, defaultPatch
    -- ** modify
    , dummy, code, doc, attributeMap, decay, synthControls
    , addFlag, addFlags, pressure, addCommonFlag, triggered
    , controlDefaults
    -- ** environ
    , environ, range, nnRange
    -- ** per-allocation
    , instRange
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
synth name doc patches = Inst.SynthDecl
    { synthdName = name
    , synthdDoc = doc
    , synthdPatches
    , synthdWarns
    }
    where
    nameOf = (patch#Patch.name #$)
    (synthdPatches, synthdWarns) = extractWarns $
        zip (map nameOf patches) $ map makeInst patches

extractWarns :: [(Text, (code, [Library.Shadowed]))]
    -> ([(Text, code)], [Text])
extractWarns insts =
    ( zip names codes
    , concat
        [ map (((name <> ": ") <>) . Library.showShadowed) shadowed
        | (name, shadowed) <- zip names shadoweds
        ]
    )
    where
    (names, (codes, shadoweds)) = unzip <$> unzip insts

makeInst :: Patch -> (Inst.Inst Cmd.InstrumentCode, [Library.Shadowed])
makeInst (Patch patch dummy common) = (, shadowed) $ Inst.Inst
    { instBackend = case dummy of
        Nothing -> Inst.Midi patch
        Just msg -> Inst.Dummy msg
    , instCommon = common { Common.commonCode = code }
    }
    where (code, shadowed) = makeCode (Common.commonCode common)

makeCode :: Code -> (Cmd.InstrumentCode, [Library.Shadowed])
makeCode (Code library instPostproc instCmds instThru) =
    ( Cmd.InstrumentCode { instCalls, instPostproc, instCmds, instThru }
    , shadowed
    )
    where (instCalls, shadowed) = compileLibrary library

-- | InstrumentCalls doesn't have modules, so just pull everything out of every
-- module.
compileLibrary :: Library.Library
    -> (Derive.InstrumentCalls, [Library.Shadowed])
compileLibrary = first convert . Library.compile
    where
    convert (Derive.Scopes gen trans track val) = Derive.Scopes
        { scopesGenerator = extract $ Derive.scopeNote gen
        , scopesTransformer = extract $ Derive.scopeNote trans
        , scopesTrack = extract $ Derive.scopeNote track
        , scopesVal = extract val
        }
    -- TODO this doesn't warn about shadowed entries, but I'd have to extend
    -- 'synth' to be in LogMonad.
    extract :: Map Module.Module (Derive.CallMap a) -> Derive.CallMap a
    extract = mconcat . Map.elems

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code
    { codeLibrary :: !Library.Library
    , codePostproc :: !Cmd.InstrumentPostproc
    , codeCmds :: ![Cmd.HandlerId]
    , codeThru :: !(Maybe Cmd.ThruFunction)
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
data Call d
    = Generator Expr.Symbol (Derive.Generator d)
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
nullCall :: Derive.Generator Derive.Note -> Code
nullCall = noteCalls . nullCalls

nullCalls :: Derive.Generator Derive.Note -> [Call Derive.Note]
nullCalls call =
    [ generator Symbols.nullNote call
    , generator Symbols.defaultNote call
    ]

noteCalls :: [Call Derive.Note] -> Code
noteCalls calls =
    noteGenerators ([(name, c) | Generator name c <- calls]
        ++ [(name, c) | Both name c _ <- calls])
    <> noteTransformers ([(name, c) | Transformer name c <- calls]
        ++ [(name, c) | Both name _ c <- calls])

-- | Add the given calls to the note track scope.
noteGenerators :: [(Expr.Symbol, Derive.Generator Derive.Note)] -> Code
noteGenerators calls = mempty { codeLibrary = Library.generators calls }

-- | Add the given calls to the note track scope.
noteTransformers :: [(Expr.Symbol, Derive.Transformer Derive.Note)] -> Code
noteTransformers calls = mempty { codeLibrary = Library.transformers calls }

valCalls :: [(Expr.Symbol, Derive.ValCall)] -> Code
valCalls calls = mempty { codeLibrary = Library.vals calls }

postproc :: Cmd.InstrumentPostproc -> Code
postproc post = mempty { codePostproc = post }

cmd :: Cmd.HandlerId -> Code
cmd c = mempty { codeCmds = [c] }

thru :: Cmd.ThruFunction -> Code
thru f = mempty { codeThru = Just f }

-- * Patch

data Patch = Patch
    { patchPatch :: Patch.Patch
    , patchDummy :: Maybe Text
    , patchCommon :: Common.Common Code
    }

patch = Lens.lens patchPatch (\r a -> r { patchPatch = a })
common = Lens.lens patchCommon (\r a -> r { patchCommon = a })

instance Pretty Patch where
    format (Patch patch dummy common) = Pretty.record "Patch"
        [ ("patch", Pretty.format patch)
        , ("dummy", Pretty.format dummy)
        , ("common", Pretty.format common)
        ]

makePatch :: Patch.Patch -> Patch
makePatch p = Patch
    { patchPatch = p
    , patchDummy = Nothing
    , patchCommon = Common.common mempty
    }

-- | Convert patches as emitted by 'Patch.Sysex.Patch'.
patchFromPair :: (Patch.Patch, Common.Common ()) -> Patch
patchFromPair (patch, common) = (makePatch patch)
    { patchCommon = common { Common.commonCode = mempty } }

-- | Make a patch, with a few parameters that tend to be unique per patch.
-- Controls come last because they are often a long list.
--
-- TODO I don't love the name, but 'patch' is already taken by the lens.
namedPatch :: Control.PbRange -> InstT.Name
    -> [(Midi.Control, ScoreT.Control)] -> Patch
namedPatch pbRange name controls =
    makePatch $ (Patch.patch pbRange name)
        { Patch.patchControlMap = Control.controlMap controls }

-- | Make a default patch for the synth.
defaultPatch :: Control.PbRange -> [(Midi.Control, ScoreT.Control)] -> Patch
defaultPatch pbRange controls =
    makePatch $ (Patch.patch pbRange Patch.defaultName)
        { Patch.patchControlMap = Control.controlMap controls }

-- ** modify

dummy :: Text -> Patch -> Patch
dummy msg patch = patch { patchDummy = Just msg }

code :: Lens Patch Code
code = common # Common.code

doc :: Lens Patch Doc.Doc
doc = common # Common.doc

attributeMap :: Lens Patch Patch.AttributeMap
attributeMap = patch # Patch.attributeMap

decay :: Lens Patch (Maybe RealTime)
decay = patch # Patch.defaults # Patch.decay

-- | Annotate all the patches with some global controls.
synthControls :: [(Midi.Control, ScoreT.Control)] -> [Patch] -> [Patch]
synthControls controls = map $
    patch # Patch.controlMap %= (Control.controlMap controls <>)

addFlag :: Patch.Flag -> Patch.Patch -> Patch.Patch
addFlag flag =
    Patch.defaults#Patch.flags %= Just . Patch.addFlag flag . fromMaybe mempty

addFlags :: [Patch.Flag] -> Patch.Patch -> Patch.Patch
addFlags flags = Patch.defaults#Patch.flags
    %= Just . Set.union (Set.fromList flags) . fromMaybe mempty

-- | Set a patch to pressure control.
pressure :: Patch -> Patch
pressure = patch %= (Patch.defaults#Patch.decay #= Just 0)
    . addFlag Patch.Pressure

addCommonFlag :: Common.Flag -> Patch -> Patch
addCommonFlag flag = common#Common.flags %= Set.insert flag

triggered :: Patch -> Patch
triggered = addCommonFlag Common.Triggered

controlDefaults :: [(ScoreT.Control, Signal.Y)] -> Patch -> Patch
controlDefaults controls =
    patch#Patch.defaults#Patch.controlDefaults #= Just (Map.fromList controls)

-- ** environ

-- | The instrument will also set the given environ when it comes into scope.
environ :: REnv.ToVal a => Env.Key -> a -> Patch -> Patch
environ name val = common %= Common.addEnviron name val

-- | Set instrument range.
range :: Scale.Range -> Patch -> Patch
range range = environ EnvKey.instrumentBottom (Scale.rangeBottom range)
    . environ EnvKey.instrumentTop (Scale.rangeTop range)

nnRange :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Patch -> Patch
nnRange (bottom, top) = environ EnvKey.instrumentBottom bottom
    . environ EnvKey.instrumentTop top

-- ** per-allocation

-- | Like 'range', but set it in the allocation, not the patch.
instRange :: Scale.Range -> Common.Config -> Common.Config
instRange range =
    Common.addCenviron EnvKey.instrumentBottom (Scale.rangeBottom range)
    . Common.addCenviron EnvKey.instrumentTop (Scale.rangeTop range)

-- * Allocations

allocations ::
    [(ScoreT.Instrument, InstT.Qualified,
        Common.Config -> Common.Config, UiConfig.Backend)]
    -- ^ (inst, qualified, set_config, backend)
    -> UiConfig.Allocations
allocations = UiConfig.makeAllocations . map make
    where
    make (name, qualified, setConfig, backend) =
        ( name
        , UiConfig.Allocation
            { allocQualified = qualified
            , allocConfig = setConfig Common.emptyConfig
            , allocBackend = backend
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
