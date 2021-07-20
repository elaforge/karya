-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A simple tag-oriented query language, and an index for fast-ish searching.

    The syntax is documented by 'Query'.
-}
module Instrument.Search where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Maps as Maps
import qualified Util.Seq as Seq
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Tag as Tag

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch

import           Global


type Search = Query -> [InstTypes.Qualified]

{- | A simple tag-oriented query language.  Instruments match whose tags match
    all of the given TagKeys exactly, and whose corresponding vals have the
    queried val as a substring.  All the pairs must match, but pairs that match
    nothing won't cause the match to fail.  A tag beginning with @!@ will
    subtract its matches from the result.

    For example, a single word @tag1@ will match all instruments that have the
    given tag.  @tag1=x@ requires that tag1 has an \"x\" in it.

    @tag1=x tag2=y !bad !not=want@ requires both tags to match, the @bad@ tag
    to not be present, and the @not@ tag to not contain \"want\".
    -}
newtype Query = Query [Clause]
    deriving (Show)

-- | Clause inverted? tag val
data Clause = Clause Bool Tag.Key Tag.Value
    deriving (Show)

-- | Search the db.  The input Query is in the parsed db query language, and
-- the output is the names of matching patches, along with their backend.
--
-- An empty query matches everything.
search :: Index -> Search
search idx (Query []) = Map.keys (idx_instrument_tags idx)
search idx (Query clauses)
    | null positive = []
    | otherwise = Set.toList $
        foldl1 Set.intersection positive `Set.difference` negative
    where
    positive
        | null kvs = [Map.keysSet (idx_instrument_tags idx)]
        | otherwise = filter (not . Set.null) $ map Set.fromList $
            query_matches idx kvs
        where kvs = [(k, v) | Clause False k v <- clauses]
    negative = Set.fromList $ concat $
        query_matches idx [(k, v) | Clause True k v <- clauses]

tags_of :: Index -> InstTypes.Qualified -> Maybe [Tag.Tag]
tags_of idx inst = Map.lookup inst (idx_instrument_tags idx)

data Index = Index {
    idx_by_key :: Map Tag.Key (Map Tag.Value [InstTypes.Qualified])
    , idx_instrument_tags :: Map InstTypes.Qualified [Tag.Tag]
    } deriving (Show)

empty_index :: Index
empty_index = Index Map.empty Map.empty

-- | Merge the indices, favoring instruments from the left one.
merge_indices :: Index -> Index -> Index
merge_indices (Index keys0 inv0) (Index keys1 inv1) =
    Index (merge_keys keys0 keys1) (Map.union inv0 inv1)
    where
    merge_keys = Map.unionWith merge_vals
    merge_vals = Map.unionWith (++)

make_index :: Inst.Db code -> Index
make_index db = Index
    { idx_by_key = Map.map Maps.multimap (Maps.multimap idx)
    , idx_instrument_tags = Map.fromList inst_tags
    }
    where
    inst_tags = instrument_tags db
    idx = [(key, (val, inst)) | (inst, tags) <- inst_tags, (key, val) <- tags]

-- | The query language looks like \"a b= c=d !e=f\", which means
--
-- > Query [Clause False "a" "", Clause False "b" "", Clause False "c" "d",
-- >    Clause True "e" "f"]
--
-- TODO parse quotes for keys or vals with spaces
parse :: Text -> Query
parse = Query . map clause . Text.words
    where
    clause word
        | Just ('!', pre) <- Text.uncons pre =
            Clause True pre (Text.drop 1 post)
        | otherwise = Clause False pre (Text.drop 1 post)
        where (pre, post) = Text.break (=='=') word

-- * implementation

query_matches :: Index -> [(Tag.Key, Tag.Value)] -> [[InstTypes.Qualified]]
query_matches (Index idx _) = map with_tag
    where
    with_tag (key, val) = case Map.lookup key idx of
        Nothing -> []
        Just vals -> concatMap snd $ filter ((val `Text.isInfixOf`) . fst)
            (Map.assocs vals)

instrument_tags :: Inst.Db code -> [(InstTypes.Qualified, [Tag.Tag])]
instrument_tags = concatMap synth_tags . Inst.synths

synth_tags :: (InstTypes.SynthName, Inst.Synth code)
    -> [(InstTypes.Qualified, [Tag.Tag])]
synth_tags (synth_name, synth) = do
    (inst_name, inst) <- Map.toList (Inst.synth_insts synth)
    let tags = normalize_tags $
            common_tags synth_name inst_name (Inst.inst_common inst)
            ++ inst_tags (Inst.inst_backend inst)
    return (InstTypes.Qualified synth_name inst_name, tags)

common_tags :: InstTypes.SynthName -> InstTypes.Name -> Common.Common code
    -> [Tag.Tag]
common_tags synth_name inst_name common =
    (Tag.synth, synth_name)
    : (Tag.name, inst_name)
    : Common.common_tags common

-- | Get tags of an inst, including automatically generated tags.
inst_tags :: Inst.Backend -> [Tag.Tag]
inst_tags (Inst.Midi patch) = concat
    [ [(Tag.backend, "midi")]
    , control_tags $ Patch.patch_control_map patch
    , case Patch.patch_initialize patch of
        Patch.InitializeMidi msgs
            | any Midi.is_sysex msgs -> [(Tag.sysex, "")]
            | otherwise -> []
        _ -> []
    ]
inst_tags (Inst.Im _patch) = [(Tag.backend, "im")]
inst_tags (Inst.Sc _patch) = [(Tag.backend, "sc")]
inst_tags Inst.Dummy = []

normalize_tags :: [Tag.Tag] -> [Tag.Tag]
normalize_tags =
    Seq.drop_dups id . List.sort . map (bimap Text.toLower Text.toLower)

control_tags :: Control.ControlMap -> [Tag.Tag]
control_tags = map ((,) Tag.control . ScoreT.control_name) . Map.keys
