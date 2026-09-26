-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{- | This is called Inst because I already have way too many modules named
    Instrument and I couldn't think of anything better.  TODO think of
    something better.

    The hierarchy, from general to specific goes:

    - 'InstT.Qualified' - global name of an Inst, used to instantiate
    a ScoreTypes.Instrument.

    - 'ScoreTypes.Instrument' - name an instance of an Inst within a score

    - 'Inst' - instrument encompassing all backends.

    - Patch - backend-specific instrument

    Instrument configuration is divided into static (built-in to the instrument)
    and dynamic (configured per score).

    Static configuration starts with 'Inst', and is divided into 'Backend'
    specific and 'Common.Common'.

    Dynamic configuration starts with 'Ui.StateConfig.Allocation' and is also
    divided into 'Ui.StateConfig.Backend' specific and 'Common.Config'.  When
    a new allocation is created, the 'Midi.Patch.patch_defaults' are copied
    to 'Midi.Patch.config_settings'.
-}
module Instrument.Inst (
    -- * Inst
    Inst(..), common, backend, Backend(..), backendName
    , instMidi, instAttributes
    -- * db
    , Db, Synth(..), empty, size, synthNames, synths, lookupSynth, lookup
    , SynthDecl(..), db, merge
    , annotate
) where
import           Prelude hiding (lookup)
import qualified Data.Map as Map

import qualified Util.Lens as Lens
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Tag as Tag

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Perform.Sc.Patch as Sc.Patch

import           Global


-- * Inst

data Inst code = Inst
    { instBackend :: Backend
    , instCommon :: Common.Common code
    } deriving (Show)

backend = Lens.lens instBackend (\r a -> r { instBackend = a })
common = Lens.lens instCommon (\r a -> r { instCommon = a })

instance Pretty code => Pretty (Inst code) where
    format (Inst backend common) = Pretty.record "Inst"
        [ ("backend", Pretty.format backend)
        , ("common", Pretty.format common)
        ]

data Backend
    -- | A Dummy instrument should be resolved to concrete instruments during
    -- derivation.  It includes an error msg show if that doesn't happen.
    = Dummy Text
    | Midi Midi.Patch.Patch
    | Im Im.Patch.Patch
    | Sc Sc.Patch.Patch
    deriving (Show)

instance Pretty Backend where
    format = \case
        Dummy msg -> "Dummy \"" <> Pretty.text msg <> "\""
        Midi patch -> Pretty.format patch
        Im patch -> Pretty.format patch
        Sc patch -> Pretty.format patch

backendName :: Backend -> Text
backendName = \case
    Dummy {} -> "dummy"
    Midi {} -> "midi"
    Im {} -> "im"
    Sc {} -> "sc"

instMidi :: Inst code -> Maybe Midi.Patch.Patch
instMidi inst = case instBackend inst of
    Midi inst -> Just inst
    _ -> Nothing

instAttributes :: Inst code -> [Attrs.Attributes]
instAttributes inst = case instBackend inst of
    Dummy {} -> []
    Midi patch -> Common.mappedAttributes $
        Midi.Patch.patchAttributeMap patch
    Im patch -> Common.mappedAttributes $
        Im.Patch.patchAttributeMap patch
    Sc _patch -> []

-- * Db

newtype Db code = Db (Map InstT.SynthName (Synth code))
    deriving (Show, Pretty)

data Synth code = Synth
    { synthDoc :: Text -- ^ Full name, just for documentation.
    , synthInsts :: Map InstT.Name (Inst code)
    } deriving (Show)

insts = Lens.lens synthInsts (\r a -> r { synthInsts = a })

instance Pretty code => Pretty (Synth code) where
    format (Synth _ insts) = Pretty.format insts

empty :: Db code
empty = Db mempty

-- | Number of 'Inst's in the db.
size :: Db code -> Int
size (Db db) = Num.sum $ map (Map.size . synthInsts) $ Map.elems db

synthNames :: Db code -> [InstT.SynthName]
synthNames (Db db) = Map.keys db

synths :: Db code -> [(InstT.SynthName, Synth code)]
synths (Db db) = Map.toList db

lookupSynth :: InstT.SynthName -> Db code -> Maybe (Synth code)
lookupSynth synth (Db db) = Map.lookup synth db

lookup :: InstT.Qualified -> Db code -> Maybe (Inst code)
lookup (InstT.Qualified synth name) (Db db) =
    Map.lookup name . synthInsts =<< Map.lookup synth db

type Warn = Text

-- | Unchecked synth declaration.  'db' will check it for duplicates and other
-- problems.
data SynthDecl code = SynthDecl
    { synthdName :: InstT.SynthName
    , synthdDoc :: Text
    , synthdPatches :: [(InstT.Name, Inst code)]
    , synthdWarns :: [Warn]
    } deriving (Show)

instance Pretty code => Pretty (SynthDecl code) where
    format (SynthDecl name doc insts warns) = Pretty.record "SynthDecl"
        [ ("name", Pretty.format name)
        , ("doc", Pretty.format doc)
        , ("instruments", Pretty.format insts)
        , ("warns", Pretty.format warns)
        ]

-- | Construct and validate a Db, returning any errors that occurred.
db :: [SynthDecl code] -> (Db code, [Warn])
db synthDecls = (Db db, synthErrors ++ instErrors ++ validateErrors)
    where
    (instMaps, instErrors) = second concat $ unzip $ do
        SynthDecl synth synthDoc insts warns <- synthDecls
        let (instMap, dups) = Maps.unique insts
        let errors =
                [ "duplicate inst: " <> pretty (InstT.Qualified synth name)
                | name <- map fst dups
                ]
        pure ((synth, Synth synthDoc instMap), warns ++ errors)
    (db, dups) = Maps.unique instMaps
    synthErrors = ["duplicate synth: " <> showt synth | synth <- map fst dups]
    validateErrors = concat
        [ map ((pretty (InstT.Qualified synth name) <> ": ") <>)
            (validate inst)
        | SynthDecl synth _ insts _ <- synthDecls, (name, inst) <- insts
        ]

-- | Return any errors found in the Inst.
validate :: Inst code -> [Warn]
validate inst = case instBackend inst of
    Dummy {} -> []
    Midi patch -> Common.overlappingAttributes $
        Midi.Patch.patchAttributeMap patch
    Im patch -> Common.overlappingAttributes $
        Im.Patch.patchAttributeMap patch
    Sc _patch -> []

-- | Merge the Dbs, and return any duplicate synths.
merge :: Db code -> Db code -> (Db code, [InstT.SynthName])
merge (Db db1) (Db db2) = (Db db, Map.keys dups)
    where (db, dups) = Maps.uniqueUnion db1 db2

annotate :: Map InstT.Qualified [Tag.Tag] -> Db code
    -> (Db code, [InstT.Qualified])
annotate annots db = Map.foldrWithKey modify (db, []) annots
    where
    modify qualified@(InstT.Qualified synth name) tags (db, notFound) =
        case modifyInst synth name (addTags tags) db of
            Nothing -> (db, qualified : notFound)
            Just db -> (db, notFound)
    addTags tags = common#Common.tags %= (tags++)

modifyInst :: InstT.SynthName -> InstT.Name -> (Inst code -> Inst code)
    -> Db code -> Maybe (Db code)
modifyInst synth name modify (Db db) = Db <$> do
    inst <- Map.lookup name . synthInsts =<< Map.lookup synth db
    pure $ Map.adjust (insts %= Map.insert name (modify inst)) synth db
