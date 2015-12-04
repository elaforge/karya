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

import qualified Util.Map as Map
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Tag as Tag
import Global


type Search = Query -> [Score.Instrument]

-- | A simple tag-oriented query language.  Instruments match whose tags match
-- all of the given TagKeys exactly, and whose corresponding vals have the
-- queried val as a substring.  All the pairs must match, but pairs that
-- match nothing won't cause the match to fail.  A tag beginning with @!@ will
-- subtract its matches from the result.
--
-- For example, a single word @tag1@ will match all instruments that have the
-- given tag.  @tag1=x@ requires that tag1 has an \"x\" in it.
--
-- @tag1=x tag2=y !bad !not=want@ requires both tags to match, the
-- @bad@ tag to not be present, and the @not@ tag to not contain \"want\".
newtype Query = Query [Clause]
    deriving (Show)

-- | Clause inverted? tag val
data Clause = Clause Bool Instrument.TagKey Instrument.TagVal
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

tags_of :: Index -> Score.Instrument -> Maybe [Instrument.Tag]
tags_of idx inst = Map.lookup inst (idx_instrument_tags idx)

data Index = Index {
    idx_by_key :: Map.Map Instrument.TagKey
        (Map.Map Instrument.TagVal [Score.Instrument])
    , idx_instrument_tags :: Map.Map Score.Instrument [Instrument.Tag]
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

make_index :: MidiDb.MidiDb code -> Index
make_index midi_db = Index
    { idx_by_key = Map.map Map.multimap (Map.multimap idx)
    , idx_instrument_tags = Map.fromList inst_tags
    }
    where
    inst_tags = instrument_tags midi_db
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
        | Just ('!', pre) <- Text.uncons pre = Clause True pre (Text.drop 1 post)
        | otherwise = Clause False pre (Text.drop 1 post)
        where (pre, post) = Text.break (=='=') word

-- * implementation

query_matches :: Index -> [(Instrument.TagKey, Instrument.TagVal)]
    -> [[Score.Instrument]]
query_matches (Index idx _) = map with_tag
    where
    with_tag (key, val) = case Map.lookup key idx of
        Nothing -> []
        Just vals -> concatMap snd $ filter ((val `Text.isInfixOf`) . fst)
            (Map.assocs vals)

instrument_tags :: MidiDb.MidiDb code -> [(Score.Instrument, [Instrument.Tag])]
instrument_tags (MidiDb.MidiDb synths) = concatMap synth_tags (Map.elems synths)

synth_tags :: Instrument.Synth code -> [(Score.Instrument, [Instrument.Tag])]
synth_tags synth = do
    (inst_name, (patch, _)) <- Map.toList (Instrument.synth_patches synth)
    let inst = MidiDb.instrument (Instrument.synth_name synth) inst_name
    return (inst, patch_tags synth inst_name patch)

-- | Get tags of a patch, including automatically generated tags.
patch_tags :: Instrument.Synth code -> Instrument.InstrumentName
    -> Instrument.Patch -> [Instrument.Tag]
patch_tags synth inst_name patch = normalize_tags $
    (Tag.synth, Instrument.synth_name synth)
        : (Tag.name, inst_name)
        : control_tags (Instrument.synth_control_map synth)
        ++ control_tags
            (Instrument.inst_control_map (Instrument.patch_instrument patch))
        ++ Instrument.patch_tags patch
        ++ has_sysex
    where
    has_sysex = case Instrument.patch_initialize patch of
        Instrument.InitializeMidi msgs
            | any Midi.is_sysex msgs -> [(Tag.sysex, "")]
            | otherwise -> []
        _ -> []

normalize_tags :: [Instrument.Tag] -> [Instrument.Tag]
normalize_tags =
    Seq.drop_dups id . List.sort . map (Text.toLower *** Text.toLower)

control_tags :: Control.ControlMap -> [Instrument.Tag]
control_tags = map ((,) Tag.control . Score.control_name) . Map.keys
