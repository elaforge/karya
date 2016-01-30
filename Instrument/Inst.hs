-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is called Inst because I already have way too many modules named
-- Instrument and I couldn't think of anything better.  TODO think of something
-- better.
module Instrument.Inst (
    Inst(..), Backend(..)
    , Db, SynthName, Name, empty, size, lookup
    , db, merge
    , annotate
) where
import Prelude hiding (lookup)
import qualified Data.Map as Map

import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Perform.Midi.Instrument as Midi.Instrument
import qualified Perform.Synth.Instrument as Synth.Instrument
import qualified Instrument.Common as Common
import qualified Instrument.Tag as Tag
import Global


-- * Inst

data Inst code = Inst {
    common :: !(Common.Common code)
    , backend :: !Backend
    } deriving (Show)

instance Pretty.Pretty code => Pretty.Pretty (Inst code) where
    format (Inst common backend) = Pretty.record "Inst"
        [ ("common", Pretty.format common)
        , ("backend", Pretty.format backend)
        ]

data Backend =
    Midi !Midi.Instrument.Patch | Synth Synth.Instrument.Instrument
    deriving (Show)

instance Pretty.Pretty Backend where
    format (Midi inst) = Pretty.format inst
    format (Synth inst) = Pretty.format inst

-- * Db

newtype Db code = Db (Map.Map SynthName (Map.Map Name (Inst code)))
    deriving (Show, Pretty.Pretty)

type SynthName = Text

-- | A name uniquely addresses this instrument within a synth.  It's not
-- a Score.Instrument so it doesn't have to follow its rules about valid
-- characters.
type Name = Text

empty :: Db code
empty = Db mempty

-- | Number of 'Inst's in the db.
size :: Db code -> Int
size (Db db) = sum $ map Map.size $ Map.elems db

lookup :: SynthName -> Name -> Db code -> Maybe (Inst code)
lookup synth name (Db db) = Map.lookup name =<< Map.lookup synth db

-- | Construct and validate a Db, returning any errors that occurred.
db :: [(SynthName, [(Name, Inst code)])] -> (Db code, [Text])
db synth_insts = (Db db, synth_errors ++ inst_errors ++ validate_errors)
    where
    (inst_maps, inst_errors) = second concat $ unzip $ do
        (synth, insts) <- synth_insts
        let (inst_map, dups) = Util.Map.unique insts
        let errors = ["duplicate inst: " <> qualified_name synth name
                | name <- map fst dups]
        return ((synth, inst_map), errors)
    (db, dups) = Util.Map.unique inst_maps
    synth_errors = ["duplicate synth: " <> showt synth | synth <- map fst dups]
    validate_errors = concat
        [ map ((qualified_name synth name <> ": ") <>) (validate inst)
        | (synth, insts) <- synth_insts, (name, inst) <- insts
        ]

-- | Return any errors found in the Inst.
validate :: Inst code -> [Text]
validate inst = case backend inst of
    Midi patch -> Common.overlapping_attributes $
        Midi.Instrument.patch_attribute_map patch
    Synth patch -> Common.overlapping_attributes $
        Synth.Instrument.inst_attribute_map patch

-- | Merge the Dbs, and return any duplicate synths.
merge :: Db code -> Db code -> (Db code, [SynthName])
merge (Db db1) (Db db2) = (Db db, Map.keys dups)
    where (db, dups) = Util.Map.unique_union db1 db2

annotate :: Map.Map (SynthName, Name) [Tag.Tag] -> Db code -> (Db code, [Text])
annotate annots db = Map.foldrWithKey modify (db, []) annots
    where
    modify (synth, name) tags (db, not_found) =
        case modify_inst synth name (add_tags tags) db of
            Nothing -> (db, qualified_name synth name : not_found)
            Just db -> (db, not_found)
    add_tags tags inst = inst
        { common = (common inst)
            { Common.tags = tags ++ Common.tags (common inst) }
        }

modify_inst :: SynthName -> Name -> (Inst code -> Inst code) -> Db code
    -> Maybe (Db code)
modify_inst synth name modify (Db db) = Db <$> do
    inst <- Map.lookup name =<< Map.lookup synth db
    return $ Map.adjust (Map.insert name (modify inst)) synth db

-- * util

-- | Format a qualified inst name.
qualified_name :: SynthName -> Name -> Text
qualified_name synth name = ">" <> synth <> "/" <> name
