{- | A simple tag-oriented query language, and an index for fast-ish searching.
-}
module Instrument.Search where
import qualified Control.Arrow as Arrow
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Data

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller

import qualified Instrument.MidiDb as MidiDb


type Search = Query -> [Score.Instrument]
type Query = [(Instrument.TagKey, Instrument.TagVal)]

-- | Search the db.  The input string is in the db query language, and the
-- output is the names of matching patches, along with their backend.
search :: Index -> Search
search idx query
    | null matches = []
    | otherwise = Set.toList (foldl1 Set.intersection matches)
    where matches = map Set.fromList (query_matches idx query)

data Index = Index
    (Map.Map Instrument.TagKey (Map.Map Instrument.TagVal [Score.Instrument]))
    deriving (Show)

make_index :: MidiDb.MidiDb -> Index
make_index midi_db = Index $ Map.map Util.Data.multimap (Util.Data.multimap idx)
    where
    inv_idx = inverted_index midi_db
    idx = [(key, (val, inst)) | (inst, tags) <- inv_idx, (key, val) <- tags]


-- * implementation

backend_tag, synth_tag, name_tag, controller_tag :: Instrument.TagKey
backend_tag = "backend"
synth_tag = "synth"
name_tag = "name"
controller_tag = "controller"

tag = Instrument.tag

any_instrument = tag name_tag ""

query_matches :: Index -> Query -> [[Score.Instrument]]
query_matches (Index idx) query = map with_tag query
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
        return $ join_inst synth name

    inst_tags1 = [(inst_of tags, tags) | tags <- map lc_tags all_tags]
    inst_tags2 = [(inst, tags) | (Just inst, tags) <- inst_tags1]

synth_tags :: Instrument.Synth -> MidiDb.SynthPatches -> [[Instrument.Tag]]
synth_tags synth patches = map (stags++) (patch_tags patches)
    where
    stags = tag synth_tag (Instrument.synth_name synth)
        : controller_tags (Instrument.synth_controller_map synth)

patch_tags :: MidiDb.SynthPatches -> [[Instrument.Tag]]
patch_tags (MidiDb.PatchTemplate patch) =
    [any_instrument : Instrument.patch_tags patch]
patch_tags (MidiDb.PatchMap patches) = map ptags (Map.elems patches)
    where
    ptags patch = Instrument.tag name_tag (Instrument.inst_name inst)
            : controller_tags (Instrument.inst_controller_map inst)
            ++ Instrument.patch_tags patch
        where
        inst = Instrument.patch_instrument patch

controller_tags =
    map (Instrument.tag controller_tag) . Controller.controller_map_names


-- * util

split_inst (Score.Instrument inst) = (pre, drop 1 post)
    where (pre, post) = break (=='/') inst
join_inst synth inst_name = Score.Instrument $ synth ++ "/" ++ inst_name
