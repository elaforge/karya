{- | A simple tag-oriented query language, and an index for fast-ish searching.
-}
module Instrument.Search where
import qualified Control.Arrow as Arrow
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Map as Map

import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control

import qualified Instrument.MidiDb as MidiDb


type Search = Query -> [Score.Instrument]

-- | A simple tag-oriented query language.  Instruments match whose tags match
-- all of the given TagKeys exactly, and whose corresponding vals have the
-- queried val as a substring.  All the pairs must match, but pairs that
-- match nothing won't cause the match to fail.
type Query = [(Instrument.TagKey, Instrument.TagVal)]

-- | Search the db.  The input Query is in the parsed db query language, and
-- the output is the names of matching patches, along with their backend.
--
-- An empty query matches everything.
search :: Index -> Search
search idx query
    | null query = Map.keys (idx_inverted idx)
    | null matches = []
    | otherwise = Set.toList (foldl1 Set.intersection matches)
    where
    matches = filter (not . Set.null) $
        map Set.fromList (query_matches idx query)

tags_of :: Index -> Score.Instrument -> Maybe [Instrument.Tag]
tags_of idx inst = Map.lookup inst (idx_inverted idx)

data Index = Index {
    idx_by_key :: Map.Map Instrument.TagKey
        (Map.Map Instrument.TagVal [Score.Instrument])
    , idx_inverted :: Map.Map Score.Instrument [Instrument.Tag]
    } deriving (Show)

empty_index = Index Map.empty Map.empty

-- | Merge the indices, favoring instruments from the left one.
merge_indices :: Index -> Index -> Index
merge_indices (Index keys0 inv0) (Index keys1 inv1) =
    Index (merge_keys keys0 keys1) (Map.union inv0 inv1)
    where
    merge_keys = Map.unionWith merge_vals
    merge_vals = Map.unionWith (++)

make_index :: MidiDb.MidiDb -> Index
make_index midi_db =
    Index (Map.map Map.multimap (Map.multimap idx)) (Map.fromList inv_idx)
    where
    inv_idx = inverted_index midi_db
    idx = [(key, (val, inst)) | (inst, tags) <- inv_idx, (key, val) <- tags]

-- | The query language looks like \"a b= c=d\", which means
--
-- > [("a", ""), ("b", ""), ("c", "d")]
--
-- TODO parse quotes for keys or vals with spaces
-- TODO parse '-' for negative assertions
parse :: String -> Query
parse = map split . words
    where split w = let (pre, post) = break (=='=') w in (pre, drop 1 post)

-- * implementation

backend_tag, synth_tag, name_tag, control_tag :: Instrument.TagKey
backend_tag = "backend"
synth_tag = "synth"
name_tag = "name"
control_tag = "control"
-- | Indicates that the instrument has a sysex message as its midi
-- initialization, which probably means it's not built in to the synth.
sysex_tag = "sysex"

tag = Instrument.tag

query_matches :: Index -> Query -> [[Score.Instrument]]
query_matches (Index idx _) query = map with_tag query
    where
    with_tag (key, val) = case Map.lookup key idx of
        Nothing -> []
        Just vals -> concat $ map snd $ filter ((val `List.isInfixOf`) . fst)
            (Map.assocs vals)

inverted_index :: MidiDb.MidiDb -> [(Score.Instrument, [Instrument.Tag])]
inverted_index (MidiDb.MidiDb synths) = inst_tags2
    where
    all_tags = concat [synth_tags synth patches
        | (synth, patches) <- Map.elems synths]
    lc_tags = let lc = map Char.toLower in map (lc Arrow.*** lc)
    inst_of tags = do
        synth <- lookup synth_tag tags
        name <- lookup name_tag tags
        return $ MidiDb.join_inst synth name

    inst_tags1 = [(inst_of tags, tags) | tags <- map lc_tags all_tags]
    inst_tags2 = [(inst, tags) | (Just inst, tags) <- inst_tags1]

-- | Get tags for the synth, including automatically generated synth tags.
synth_tags :: Instrument.Synth -> MidiDb.PatchMap -> [[Instrument.Tag]]
synth_tags synth patches = map (stags++) (patch_tags patches)
    where
    stags = tag synth_tag (Instrument.synth_name synth)
        : control_tags (Instrument.synth_control_map synth)

-- | Get tags for the patch, including automatically generated ones.
patch_tags :: MidiDb.PatchMap -> [[Instrument.Tag]]
patch_tags (MidiDb.PatchMap patches) = map ptags (Map.assocs patches)
    where
    ptags (inst_name, (patch, _)) = Instrument.tag name_tag inst_name
            : control_tags (Instrument.inst_control_map inst)
            ++ Instrument.patch_tags patch
            ++ has_sysex
        where
        inst = Instrument.patch_instrument patch
        has_sysex = case Instrument.patch_initialize patch of
            Instrument.InitializeMidi msgs
                | any Midi.is_sysex msgs -> [Instrument.tag sysex_tag ""]
                | otherwise -> []
            _ -> []

control_tags = map (Instrument.tag control_tag) . Control.control_map_names
