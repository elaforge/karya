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

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Instrument.Tag as Tag

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch

import           Global


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
search :: Index -> Query -> [InstT.Qualified]
search idx (Query []) = Map.keys (idxInstrumentTags idx)
search idx (Query clauses)
    | null positive = []
    | otherwise = Set.toList $
        foldl1 Set.intersection positive `Set.difference` negative
    where
    positive
        | null kvs = [Map.keysSet (idxInstrumentTags idx)]
        | otherwise = filter (not . Set.null) $ queryMatches idx kvs
        where kvs = [(k, v) | Clause False k v <- clauses]
    negative = mconcat $ queryMatches idx [(k, v) | Clause True k v <- clauses]

tagsOf :: Index -> InstT.Qualified -> Maybe [Tag.Tag]
tagsOf idx inst = Map.lookup inst (idxInstrumentTags idx)

data Index = Index
    { idxByKey :: Map Tag.Key (Map Tag.Value [InstT.Qualified])
    , idxInstrumentTags :: Map InstT.Qualified [Tag.Tag]
    } deriving (Show)

emptyIndex :: Index
emptyIndex = Index Map.empty Map.empty

-- | Merge the indices, favoring instruments from the left one.
mergeIndices :: Index -> Index -> Index
mergeIndices (Index keys0 inv0) (Index keys1 inv1) =
    Index (mergeKeys keys0 keys1) (Map.union inv0 inv1)
    where
    mergeKeys = Map.unionWith mergeVals
    mergeVals = Map.unionWith (++)

makeIndex :: Inst.Db code -> Index
makeIndex db = Index
    { idxByKey = Map.map Maps.multimap (Maps.multimap idx)
    , idxInstrumentTags = Map.fromList instTags
    }
    where
    instTags = instrumentTags db
    idx = [(key, (val, inst)) | (inst, tags) <- instTags, (key, val) <- tags]

-- | The query language looks like \"a b= c=d !e=f\", which means
--
-- > Query
-- >    [ Clause False "" "a"
-- >    , Clause False "b" ""
-- >    , Clause False "c" "d"
-- >    , Clause True "e" "f"
-- >    ]
--
-- So a bare word is short for =word, which in turn is special cased to match
-- the qualified name.
parse :: Text -> Query
parse = Query . map clause . Text.words
    where
    clause word_
        | Text.null post = Clause inverted "" pre
        | otherwise = Clause inverted pre val
        where
        (word, inverted) = case Text.uncons word_ of
            Just ('!', word) -> (word, True)
            _ -> (word_, False)
        (pre, post) = Text.break (=='=') word
        val = Text.drop 1  post

-- * implementation

queryMatches :: Index -> [(Tag.Key, Tag.Value)] -> [Set InstT.Qualified]
queryMatches (Index idx _) = map (Set.fromList . withTag)
    where
    withTag ("", val) = case Text.splitOn "/" val of
        [synth, name] -> filter ((val `Text.isInfixOf`) . pretty) $
            withTag ("synth", synth) ++ withTag ("name", name)
        _ -> withTag ("synth", val) ++ withTag ("name", val)
    withTag (key, val) = case Map.lookup key idx of
        Nothing -> []
        Just vals -> concatMap snd $
            filter ((val `Text.isInfixOf`) . fst) (Map.assocs vals)

instrumentTags :: Inst.Db code -> [(InstT.Qualified, [Tag.Tag])]
instrumentTags = concatMap synthTags . Inst.synths

synthTags :: (InstT.SynthName, Inst.Synth code)
    -> [(InstT.Qualified, [Tag.Tag])]
synthTags (synthName, synth) = do
    (instName, inst) <- Map.toList (Inst.synthInsts synth)
    let tags = normalizeTags $
            commonTags synthName instName (Inst.instCommon inst)
            ++ instTags (Inst.instBackend inst)
    pure (InstT.Qualified synthName instName, tags)

commonTags :: InstT.SynthName -> InstT.Name -> Common.Common code -> [Tag.Tag]
commonTags synthName instName common =
    (Tag.synth, synthName)
    : (Tag.name, instName)
    : Common.commonTags common

-- | Get tags of an inst, including automatically generated tags.
instTags :: Inst.Backend -> [Tag.Tag]
instTags = \case
    Inst.Midi patch -> concat
        [ [(Tag.backend, "midi")]
        , controlTags $ Patch.patchControlMap patch
        , case Patch.patchInitialize patch of
            Patch.InitializeMidi msgs
                | any Midi.isSysex msgs -> [(Tag.sysex, "")]
                | otherwise -> []
            _ -> []
        ]
    Inst.Im {} -> [(Tag.backend, "im")]
    Inst.Sc {} -> [(Tag.backend, "sc")]
    Inst.Dummy {} -> []

normalizeTags :: [Tag.Tag] -> [Tag.Tag]
normalizeTags =
    Lists.dropDups id . List.sort . map (bimap Text.toLower Text.toLower)

controlTags :: Control.ControlMap -> [Tag.Tag]
controlTags = map ((,) Tag.control . ScoreT.controlName) . Map.keys
