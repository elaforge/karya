-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is called Inst because I already have way too many modules named
-- Instrument and I couldn't think of anything better.  TODO think of something
-- better.
module Instrument.Inst (
    -- * Inst
    Inst(..), common, backend, Backend(..)
    , inst_midi, inst_attributes
    -- * Qualified
    , Qualified(..), SynthName, Name, parse_qualified, show_qualified
    -- * db
    , Db, Synth(..), empty, size, synth_names, synths, lookup_synth, lookup
    , SynthDecl, db, merge
    , annotate
) where
import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lens as Lens
import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Derive.ScoreTypes as ScoreTypes
import qualified Perform.Midi.Instrument as Midi.Instrument
import qualified Perform.Im.Instrument as Im.Instrument
import qualified Instrument.Common as Common
import qualified Instrument.Tag as Tag
import Global


-- * Inst

data Inst code = Inst {
    inst_backend :: !Backend
    , inst_common :: !(Common.Common code)
    } deriving (Show)

backend = Lens.lens inst_backend
    (\f r -> r { inst_backend = f (inst_backend r) })
common = Lens.lens inst_common (\f r -> r { inst_common = f (inst_common r) })

instance Pretty.Pretty code => Pretty.Pretty (Inst code) where
    format (Inst backend common) = Pretty.record "Inst"
        [ ("backend", Pretty.format backend)
        , ("common", Pretty.format common)
        ]

data Backend = Midi !Midi.Instrument.Patch | Im Im.Instrument.Instrument
    deriving (Show)

instance Pretty.Pretty Backend where
    format (Midi inst) = Pretty.format inst
    format (Im inst) = Pretty.format inst

inst_midi :: Inst code -> Maybe Midi.Instrument.Patch
inst_midi inst = case inst_backend inst of
    Midi inst -> Just inst
    _ -> Nothing

inst_attributes :: Inst code -> [ScoreTypes.Attributes]
inst_attributes inst = case inst_backend inst of
    Midi inst -> Common.mapped_attributes $
        Midi.Instrument.patch_attribute_map inst
    Im inst -> Common.mapped_attributes $
        Im.Instrument.inst_attribute_map inst

-- * Qualified

-- | This is an instrument name qualified by synth name.  It should uniquely
-- address a single instrument.  It's different from a 'ScoreTypes.Instrument',
-- which addresses a particular instantiation of an instrument in a particular
-- score.
data Qualified = Qualified SynthName Name deriving (Show, Eq, Ord)

instance Pretty.Pretty Qualified where pretty = show_qualified

instance Serialize.Serialize Qualified where
    put (Qualified a b) = Serialize.put a >> Serialize.put b
    get = Qualified <$> Serialize.get <*> Serialize.get

-- | Short but unabbreviated lowercase name with no spaces.  This is used to
-- address instruments so it should be easy to type.
type SynthName = Text

-- | A name uniquely addresses this instrument within a synth.  It's not
-- a Score.Instrument so it doesn't have to follow its rules about valid
-- characters.
type Name = Text

parse_qualified :: Text -> Qualified
parse_qualified text = Qualified pre (Text.drop 1 post)
    where (pre, post) = Text.break (=='/') text

show_qualified :: Qualified -> Text
show_qualified (Qualified synth name) = synth <> "/" <> name

-- | Format a qualified inst name.  TODO integrate with Qualified
qualified_name :: SynthName -> Name -> Text
qualified_name synth name = ">" <> synth <> "/" <> name

-- * Db

newtype Db code = Db (Map.Map SynthName (Synth code))
    deriving (Show, Pretty.Pretty)

data Synth code = Synth {
    -- | Full name, just for documentation.
    synth_doc :: !Text
    , synth_insts :: !(Map.Map Name (Inst code))
    } deriving (Show)

insts = Lens.lens synth_insts (\f r -> r { synth_insts = f (synth_insts r) })

instance Pretty.Pretty code => Pretty.Pretty (Synth code) where
    format (Synth _ insts) = Pretty.format insts

empty :: Db code
empty = Db mempty

-- | Number of 'Inst's in the db.
size :: Db code -> Int
size (Db db) = sum $ map (Map.size . synth_insts) $ Map.elems db

synth_names :: Db code -> [SynthName]
synth_names (Db db) = Map.keys db

synths :: Db code -> [(SynthName, Synth code)]
synths (Db db) = Map.toList db

lookup_synth :: SynthName -> Db code -> Maybe (Synth code)
lookup_synth synth (Db db) = Map.lookup synth db

lookup :: Qualified -> Db code -> Maybe (Inst code)
lookup (Qualified synth name) (Db db) =
    Map.lookup name . synth_insts =<< Map.lookup synth db

-- | Unchecked synth declaration.  'db' will check it for duplicates and other
-- problems.
type SynthDecl code = (SynthName, Text, [(Name, Inst code)])

-- | Construct and validate a Db, returning any errors that occurred.
db :: [SynthDecl code] -> (Db code, [Text])
db synth_insts = (Db db, synth_errors ++ inst_errors ++ validate_errors)
    where
    (inst_maps, inst_errors) = second concat $ unzip $ do
        (synth, synth_doc, insts) <- synth_insts
        let (inst_map, dups) = Util.Map.unique insts
        let errors = ["duplicate inst: " <> qualified_name synth name
                | name <- map fst dups]
        return ((synth, Synth synth_doc inst_map), errors)
    (db, dups) = Util.Map.unique inst_maps
    synth_errors = ["duplicate synth: " <> showt synth | synth <- map fst dups]
    validate_errors = concat
        [ map ((qualified_name synth name <> ": ") <>) (validate inst)
        | (synth, _, insts) <- synth_insts, (name, inst) <- insts
        ]

-- | Return any errors found in the Inst.
validate :: Inst code -> [Text]
validate inst = case inst_backend inst of
    Midi patch -> Common.overlapping_attributes $
        Midi.Instrument.patch_attribute_map patch
    Im patch -> Common.overlapping_attributes $
        Im.Instrument.inst_attribute_map patch

-- | Merge the Dbs, and return any duplicate synths.
merge :: Db code -> Db code -> (Db code, [SynthName])
merge (Db db1) (Db db2) = (Db db, Map.keys dups)
    where (db, dups) = Util.Map.unique_union db1 db2

annotate :: Map.Map Qualified [Tag.Tag] -> Db code -> (Db code, [Text])
annotate annots db = Map.foldrWithKey modify (db, []) annots
    where
    modify (Qualified synth name) tags (db, not_found) =
        case modify_inst synth name (add_tags tags) db of
            Nothing -> (db, qualified_name synth name : not_found)
            Just db -> (db, not_found)
    add_tags tags = common#Common.tags %= (tags++)

modify_inst :: SynthName -> Name -> (Inst code -> Inst code) -> Db code
    -> Maybe (Db code)
modify_inst synth name modify (Db db) = Db <$> do
    inst <- Map.lookup name . synth_insts =<< Map.lookup synth db
    return $ Map.adjust (insts %= Map.insert name (modify inst)) synth db
