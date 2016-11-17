-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities for the instrument definitions in "Local.Instrument".
module Cmd.Instrument.MidiInst (
    Synth, synth
    -- * code
    , Code(..), Call
    , generator, transformer, both, note_calls
    , note_generators, note_transformers, null_call
    , postproc, cmd, thru

    -- * Patch
    , Patch(..), patch, common
    , make_patch, patch_from_pair, named_patch, default_patch
    -- ** modify
    , code, doc, attribute_map, decay, synth_controls
    , add_flag, pressure
    -- ** environ
    , environ, default_scale, range, nn_range
    -- * allocations
    , allocations, config, config1
    , merge_defaults

    -- * db
    , save_synth, load_synth
    , check_names, generate_names, clean_name
) where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time

import System.FilePath ((</>), (<.>))

import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.StateConfig as StateConfig
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Make as Make
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Serialize
import qualified Instrument.Tag as Tag

import qualified App.Config as Config
import Global
import Types


type Synth = Inst.SynthDecl Cmd.InstrumentCode

synth :: InstTypes.SynthName -> Text -> [Patch] -> Synth
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
make_code (Code generator transformer val postproc cmds thru) =
    Cmd.InstrumentCode
        { inst_calls = Derive.InstrumentCalls generator transformer val
        , inst_postproc = postproc
        , inst_cmds = cmds
        , inst_thru = thru
        }

-- * code

-- | A version of 'Cmd.InstrumentCode' that's more convenient for record update
-- syntax.
data Code = Code {
    code_note_generators :: [Derive.LookupCall (Derive.Generator Derive.Note)]
    , code_note_transformers ::
        [Derive.LookupCall (Derive.Transformer Derive.Note)]
    , code_val_calls :: [Derive.LookupCall Derive.ValCall]
    , code_postproc :: Cmd.InstrumentPostproc
    , code_cmds :: [Msg.Msg -> Cmd.CmdId Cmd.Status]
    , code_thru :: Maybe Cmd.ThruFunction
    }

instance Pretty.Pretty Code where
    format (Code note_gens note_trans val_calls _postproc cmds thru) =
        Pretty.record "Code"
            [ ("note_generators", Pretty.format $ length note_gens)
            , ("note_transformers", Pretty.format $ length note_trans)
            , ("val_calls", Pretty.format $ length val_calls)
            , ("cmds", Pretty.format $ length cmds)
            , ("thru", Pretty.format thru)
            ]

instance Monoid Code where
    mempty = Code [] [] mempty id [] Nothing
    mappend (Code g1 t1 v1 post1 cmds1 thru1)
            (Code g2 t2 v2 post2 cmds2 thru2) =
        Code (g1<>g2) (t1<>t2) (v1<>v2) (post1 . post2) (cmds1<>cmds2)
            (thru1<|>thru2)

-- ** code constructors

-- | Bundle together generators and transformers.  The rationale is described
-- in 'Derive.CallMaps'.
data Call d =
    Generator BaseTypes.CallId (Derive.Generator d)
    | Transformer BaseTypes.CallId (Derive.Transformer d)
    | Both BaseTypes.CallId (Derive.Generator d) (Derive.Transformer d)

generator :: BaseTypes.CallId -> Derive.Generator d -> Call d
generator = Generator

transformer :: BaseTypes.CallId -> Derive.Transformer d -> Call d
transformer = Transformer

both :: BaseTypes.CallId -> Make.Calls d -> Call d
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
note_generators :: [(BaseTypes.CallId, Derive.Generator Derive.Note)] -> Code
note_generators calls = mempty { code_note_generators = Derive.call_map calls }

-- | Add the given calls to the note track scope.
note_transformers :: [(BaseTypes.CallId, Derive.Transformer Derive.Note)]
    -> Code
note_transformers calls =
    mempty { code_note_transformers = Derive.call_map calls }

postproc :: Cmd.InstrumentPostproc -> Code
postproc post = mempty { code_postproc = post }

cmd :: (Msg.Msg -> Cmd.CmdId Cmd.Status) -> Code
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

instance Pretty.Pretty Patch where
    format (Patch patch common) = Pretty.record "Patch"
        [ ("patch", Pretty.format patch)
        , ("common", Pretty.format common)
        ]

-- | Make a patch.  Get the name from Patch.patch_name.
-- TODO: pass the name independently
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
named_patch :: Control.PbRange -> InstTypes.Name
    -> [(Midi.Control, Score.Control)] -> Patch
named_patch pb_range name controls =
    make_patch $ (Patch.patch pb_range name)
        { Patch.patch_control_map = Control.control_map controls }

-- | Make a default patch for the synth.
default_patch :: Control.PbRange -> [(Midi.Control, Score.Control)] -> Patch
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
synth_controls :: [(Midi.Control, Score.Control)] -> [Patch] -> [Patch]
synth_controls controls = map $
    patch # Patch.control_map %= (Control.control_map controls <>)

add_flag :: Patch.Flag -> Patch.Patch -> Patch.Patch
add_flag flag = Patch.defaults#Patch.flags %= Patch.add_flag flag

-- | Set a patch to pressure control.
pressure :: Patch -> Patch
pressure = patch %= (Patch.defaults#Patch.decay #= Just 0)
    . add_flag Patch.Pressure

-- ** environ

-- | The instrument will also set the given environ when it comes into scope.
environ :: RestrictedEnviron.ToVal a => Env.Key -> a -> Patch -> Patch
environ name val = common#Common.environ
    %= (RestrictedEnviron.make [(name, RestrictedEnviron.to_val val)] <>)

-- | The instrument will set the given scale when it comes into scope.
default_scale :: Pitch.ScaleId -> Patch -> Patch
default_scale = environ EnvKey.scale . BaseTypes.scale_id_to_sym

-- | Set instrument range.
range :: Scale.Range -> Patch -> Patch
range range = environ EnvKey.instrument_bottom (Scale.range_bottom range)
    . environ EnvKey.instrument_top (Scale.range_top range)

nn_range :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Patch -> Patch
nn_range (bottom, top) = environ EnvKey.instrument_bottom bottom
    . environ EnvKey.instrument_top top

-- * Allocations

allocations ::
    [(Text, Text, Common.Config -> Common.Config, StateConfig.Backend)]
    -- ^ (inst, qualified, set_config, backend)
    -> StateConfig.Allocations
allocations = StateConfig.make_allocations . map make
    where
    make (name, qualified, set_config, backend) =
        ( Score.Instrument name
        , StateConfig.Allocation (InstTypes.parse_qualified qualified)
            (set_config Common.empty_config) backend
        )

-- | Create an incomplete Config.  It's incomplete because it doesn't have
-- the Settings from 'Patch.patch_defaults', so it'll need to have those
-- applied when it gets applied to 'StateConfig.state_allocations'.
config :: [Patch.Addr] -> Patch.Config
config = Patch.config mempty . map (, Nothing)

-- | Specialize 'config' for a single Addr.
config1 :: Midi.WriteDevice -> Midi.Channel -> Patch.Config
config1 dev chan = config [(dev, chan)]

-- | Merge an incomplete allocation with defaults from its instrument.
merge_defaults :: Cmd.Inst -> StateConfig.Allocation
    -> Either Text StateConfig.Allocation
merge_defaults inst alloc = case (Inst.inst_midi inst, backend) of
    (Just patch, StateConfig.Midi config) -> Right $ alloc
        { StateConfig.alloc_backend =
            StateConfig.Midi (Patch.merge_defaults patch config)
        }
    (Just _, StateConfig.Dummy) -> Right alloc
    (Just _, StateConfig.Im) ->
        Left $ pretty inst <> ": can't merge defaults from Midi to Im"
    (Nothing, _) -> Left $ pretty inst
        <> ": can't merge defaults for a non-Midi inst"
    where backend = StateConfig.alloc_backend alloc


-- * db

-- | Some instruments want to load their patches in elaborate slow ways, like
-- parsing a directory full of sysexes.  These patches can export a @make_db@
-- function, which will do the slow parts and save the results in a cache file.
-- The @load@ function will simply read the cache file, if present.
save_synth :: FilePath -> InstTypes.SynthName -> [Patch] -> IO ()
save_synth app_dir synth_name patches = do
    -- Assume these are loaded from files, so I'll need to generate valid
    -- names.
    let (patch_map, logs) = generate_names patches
    mapM_ (Log.notice . (("synth " <> synth_name <> ": ") <>)) logs
    now <- Time.getCurrentTime
    Instrument.Serialize.serialize (db_path app_dir (untxt synth_name)) $
        Instrument.Serialize.InstrumentDb now (strip_code <$> patch_map)
    where
    strip_code :: Patch -> (Patch.Patch, Common.Common ())
    strip_code (Patch patch common) =
        (patch, common { Common.common_code = () })

load_synth :: (Patch.Patch -> Code) -> InstTypes.SynthName -> Text
    -> FilePath -> IO (Maybe Synth)
load_synth get_code synth_name doc app_dir = do
    let fname = db_path app_dir (untxt synth_name)
    Instrument.Serialize.unserialize fname >>= \x -> case x of
        Left err -> do
            Log.warn $ "Error loading instrument db " <> showt fname <> ": "
                <> Text.strip (pretty err)
            return Nothing
        Right (Instrument.Serialize.InstrumentDb _time patch_map) ->
            return $ Just $ Inst.SynthDecl synth_name doc
                (map (second make) (Map.toList patch_map))
    where
    make (patch, common) = make_inst $ Patch patch $
        common { Common.common_code = get_code patch }

db_path :: FilePath -> FilePath -> FilePath
db_path app_dir name =
    Config.make_path app_dir Config.instrument_cache_dir </> name <.> "db"


-- * generate_names

-- | Like 'generate_names', but don't drop or rename duplicates, just report
-- them as errors.
check_names :: [Patch] -> (Map.Map InstTypes.Name Patch, [InstTypes.Name])
check_names = second (map fst) . Util.Map.unique
    . Seq.key_on (Patch.patch_name . patch_patch)

-- | 'Patch.inst_name' is the name as it appears on the synth, so it's not
-- guaranteed to be unique.  Also, due to loading from sysexes, there may be
-- duplicate patches.  Generate valid names for the patches, drop duplicates,
-- and disambiguate names that wind up the same.
generate_names :: [Patch] -> (Map.Map InstTypes.Name Patch, [Text])
generate_names = -- This only touches the 'patch_patch' field.
    run . (concatMapM split <=< mapM drop_dup_initialization)
        . Seq.keyed_group_sort (clean_name . inst_name)
    where
    run = first Map.fromList . Identity.runIdentity . Logger.run
    -- If the name and initialization is the same, they are likely duplicates.
    drop_dup_initialization :: (InstTypes.Name, [Patch])
        -> Logger (InstTypes.Name, [Patch])
    drop_dup_initialization (name, patches) = do
        let (unique, dups) = Seq.partition_dups
                (Patch.patch_initialize . patch_patch) patches
        forM_ dups $ \(patch, dups) ->
            log ("dropped patches with the same initialization as "
                <> details patch) dups
        return (name, unique)
    -- The remaining patches are probably different and just happened to get
    -- the same name, so number them to disambiguate.
    split :: (InstTypes.Name, [Patch]) -> Logger [(InstTypes.Name, Patch)]
    split (name, patches@(_:_:_)) = do
        let named = zip (map ((name<>) . showt) [1..]) patches
        log ("split into " <> Text.intercalate ", " (map fst named)) patches
        return named
    split (name, patches) = return $ map (name,) patches

    log _ [] = return ()
    log msg patches = Logger.log $ msg <> ": "
        <> Text.intercalate ", " (map details patches)
    details patch =
        inst_name patch <> " (" <> fromMaybe "" (filename patch) <> ")"
    inst_name = Patch.patch_name . patch_patch
    filename = lookup Tag.file . Common.common_tags . patch_common

type Logger a = Logger.LoggerT Text Identity.Identity a

-- | People like to put wacky characters in their names, but it makes them
-- hard to type.
clean_name :: Text -> InstTypes.Name
clean_name =
    Text.dropWhileEnd (=='-') . Text.dropWhile (=='-')
        . strip_dups
        . Text.filter (`elem` Score.instrument_valid_chars) . Text.map replace
        . Text.toLower
    where
    strip_dups = Text.intercalate "-" . filter (not . Text.null)
        . Text.split (=='-')
    replace c
        | c `elem` (" _/" :: [Char]) = '-'
        | otherwise = c
