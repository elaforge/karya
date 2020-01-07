-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This is called Inst because I already have way too many modules named
    Instrument and I couldn't think of anything better.  TODO think of
    something better.

    The hierarchy, from general to specific goes:

    - 'InstTypes.Qualified' - global name of an Inst, used to instantiate
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
    Inst(..), common, backend, Backend(..)
    , inst_midi, inst_attributes
    -- * db
    , Db, Synth(..), empty, size, synth_names, synths, lookup_synth, lookup
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
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Tag as Tag

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Patch as Midi.Patch

import           Global


-- * Inst

data Inst code = Inst {
    inst_backend :: !Backend
    , inst_common :: !(Common.Common code)
    } deriving (Show)

backend = Lens.lens inst_backend
    (\f r -> r { inst_backend = f (inst_backend r) })
common = Lens.lens inst_common (\f r -> r { inst_common = f (inst_common r) })

instance Pretty code => Pretty (Inst code) where
    format (Inst backend common) = Pretty.record "Inst"
        [ ("backend", Pretty.format backend)
        , ("common", Pretty.format common)
        ]

data Backend = Dummy | Midi !Midi.Patch.Patch | Im Im.Patch.Patch
    deriving (Show)

instance Pretty Backend where
    format Dummy = "Dummy"
    format (Midi inst) = Pretty.format inst
    format (Im inst) = Pretty.format inst

inst_midi :: Inst code -> Maybe Midi.Patch.Patch
inst_midi inst = case inst_backend inst of
    Midi inst -> Just inst
    _ -> Nothing

inst_attributes :: Inst code -> [Attrs.Attributes]
inst_attributes inst = case inst_backend inst of
    Dummy -> []
    Midi patch -> Common.mapped_attributes $
        Midi.Patch.patch_attribute_map patch
    Im patch -> Common.mapped_attributes $
        Im.Patch.patch_attribute_map patch

-- * Db

newtype Db code = Db (Map InstTypes.SynthName (Synth code))
    deriving (Show, Pretty)

data Synth code = Synth {
    -- | Full name, just for documentation.
    synth_doc :: !Text
    , synth_insts :: !(Map InstTypes.Name (Inst code))
    } deriving (Show)

insts = Lens.lens synth_insts (\f r -> r { synth_insts = f (synth_insts r) })

instance Pretty code => Pretty (Synth code) where
    format (Synth _ insts) = Pretty.format insts

empty :: Db code
empty = Db mempty

-- | Number of 'Inst's in the db.
size :: Db code -> Int
size (Db db) = Num.sum $ map (Map.size . synth_insts) $ Map.elems db

synth_names :: Db code -> [InstTypes.SynthName]
synth_names (Db db) = Map.keys db

synths :: Db code -> [(InstTypes.SynthName, Synth code)]
synths (Db db) = Map.toList db

lookup_synth :: InstTypes.SynthName -> Db code -> Maybe (Synth code)
lookup_synth synth (Db db) = Map.lookup synth db

lookup :: InstTypes.Qualified -> Db code -> Maybe (Inst code)
lookup (InstTypes.Qualified synth name) (Db db) =
    Map.lookup name . synth_insts =<< Map.lookup synth db

-- | Unchecked synth declaration.  'db' will check it for duplicates and other
-- problems.  (name, doc, patches)
data SynthDecl code =
    SynthDecl !InstTypes.SynthName !Text ![(InstTypes.Name, Inst code)]
    deriving (Show)

instance Pretty code => Pretty (SynthDecl code) where
    format (SynthDecl name doc insts) = Pretty.record "SynthDecl"
        [ ("name", Pretty.format name)
        , ("doc", Pretty.format doc)
        , ("instruments", Pretty.format insts)
        ]

-- | Construct and validate a Db, returning any errors that occurred.
db :: [SynthDecl code] -> (Db code, [Text])
db synth_decls = (Db db, synth_errors ++ inst_errors ++ validate_errors)
    where
    (inst_maps, inst_errors) = second concat $ unzip $ do
        SynthDecl synth synth_doc insts <- synth_decls
        let (inst_map, dups) = Maps.unique insts
        let errors =
                [ "duplicate inst: " <> pretty (InstTypes.Qualified synth name)
                | name <- map fst dups
                ]
        return ((synth, Synth synth_doc inst_map), errors)
    (db, dups) = Maps.unique inst_maps
    synth_errors = ["duplicate synth: " <> showt synth | synth <- map fst dups]
    validate_errors = concat
        [ map ((pretty (InstTypes.Qualified synth name) <> ": ") <>)
            (validate inst)
        | SynthDecl synth _ insts <- synth_decls, (name, inst) <- insts
        ]

-- | Return any errors found in the Inst.
validate :: Inst code -> [Text]
validate inst = case inst_backend inst of
    Dummy -> []
    Midi patch -> Common.overlapping_attributes $
        Midi.Patch.patch_attribute_map patch
    Im patch -> Common.overlapping_attributes $
        Im.Patch.patch_attribute_map patch

-- | Merge the Dbs, and return any duplicate synths.
merge :: Db code -> Db code -> (Db code, [InstTypes.SynthName])
merge (Db db1) (Db db2) = (Db db, Map.keys dups)
    where (db, dups) = Maps.unique_union db1 db2

annotate :: Map InstTypes.Qualified [Tag.Tag] -> Db code
    -> (Db code, [InstTypes.Qualified])
annotate annots db = Map.foldrWithKey modify (db, []) annots
    where
    modify qualified@(InstTypes.Qualified synth name) tags (db, not_found) =
        case modify_inst synth name (add_tags tags) db of
            Nothing -> (db, qualified : not_found)
            Just db -> (db, not_found)
    add_tags tags = common#Common.tags %= (tags++)

modify_inst :: InstTypes.SynthName -> InstTypes.Name -> (Inst code -> Inst code)
    -> Db code -> Maybe (Db code)
modify_inst synth name modify (Db db) = Db <$> do
    inst <- Map.lookup name . synth_insts =<< Map.lookup synth db
    return $ Map.adjust (insts %= Map.insert name (modify inst)) synth db
