{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Midi.Midi as Midi

import qualified Util.Map as Map
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument



-- * local instrument utils

-- | Utility to construct a soft synth.  Soft synths are assumed to have
-- their own internal patch management, and thus have only a single wildcard
-- patch, which can be modified if necessary by a passed in function.  In case
-- some patches are special, you can also pass named patches in to be merged.
softsynth :: Instrument.SynthName -> Maybe String -> Control.PbRange
    -> [Instrument.Patch] -> [(Midi.Control, String)]
    -> (Instrument.Patch -> Instrument.Patch) -> SynthDesc
softsynth name device pb_range patches controls set_patch =
    (synth, merge_patch_maps (wildcard_patch_map (set_patch template_patch))
        (fst (patch_map patches)))
    where
    (synth, template_patch) =
        Instrument.make_softsynth name device pb_range controls


-- * midi db

data MidiDb = MidiDb
    (Map.Map Instrument.SynthName (Instrument.Synth, PatchMap))
    deriving (Show)

-- | Merge the MidiDbs, favoring instruments in the leftmost one.
merge :: MidiDb -> MidiDb -> (MidiDb, [Score.Instrument])
merge (MidiDb db0) (MidiDb db1) =
    (MidiDb $ Map.unionWith merge_synth db0 db1, rejects)
    where
    merge_synth (synth, pmap0) (_, pmap1) = (synth, merge_patches pmap0 pmap1)
    merge_patches (PatchMap ps0) (PatchMap ps1) = PatchMap (Map.union ps0 ps1)
    rejects = concatMap find_dups (Map.zip_intersection db0 db1)
    find_dups (synth, (_, PatchMap ps0), (_, PatchMap ps1)) =
        map (join_inst synth) (Map.keys (Map.intersection ps1 ps0))

midi_db :: [SynthDesc] -> MidiDb
midi_db synth_map = MidiDb $ Map.fromList
    [ (lc (Instrument.synth_name synth), (synth, patches))
    | (synth, patches) <- synth_map]

size :: MidiDb -> Int
size (MidiDb synths) = sum $ map ssize (Map.elems synths)
    where ssize (_, PatchMap patches) = Map.size patches

empty = MidiDb Map.empty

-- ** lookup

type LookupMidiInstrument = Score.Attributes -> Score.Instrument
    -> Maybe Instrument.Instrument

-- | Once I have other backends this should move back into Db.
data Info = Info {
    info_synth :: Instrument.Synth
    , info_patch :: Instrument.Patch
    } deriving (Show)

lookup_midi :: MidiDb -> LookupMidiInstrument
lookup_midi midi_db attrs inst = case lookup_instrument midi_db inst of
    Nothing -> Nothing
    Just (Info synth patch) -> Just $ make_inst synth patch inst attrs

-- | Merge a Synth and a Patch to create an Instrument.
make_inst :: Instrument.Synth -> Instrument.Patch
    -> Score.Instrument -> Score.Attributes -> Instrument.Instrument
make_inst synth patch (Score.Instrument score_inst) attrs = inst
        { Instrument.inst_control_map = Map.union inst_cmap synth_cmap
        , Instrument.inst_score_name = score_inst
        , Instrument.inst_synth = Instrument.synth_name synth
        , Instrument.inst_keyswitch = ks
        }
    where
    inst = Instrument.patch_instrument patch
    synth_cmap = Instrument.synth_control_map synth
    inst_cmap = Instrument.inst_control_map inst
    ks = Instrument.get_keyswitch (Instrument.patch_keyswitches patch) attrs

lookup_instrument :: MidiDb -> Score.Instrument -> Maybe Info
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    return (Info synth patch)

-- * patch map

type SynthDesc = (Instrument.Synth, PatchMap)

newtype PatchMap = PatchMap (Map.Map Instrument.InstrumentName Instrument.Patch)
    deriving (Show)

-- | This patch takes whatever name you give.
wildcard_inst_name :: Instrument.InstrumentName
wildcard_inst_name = "*"

-- | Build a PatchMap to give to 'midi_db'.  Colliding patches are
-- returned.
patch_map :: [Instrument.Patch] -> (PatchMap, [(String, Instrument.Patch)])
patch_map patches = (PatchMap pmap, rejects)
    where
    (pmap, rejects) = Map.unique
        [(clean_inst_name (Instrument.inst_name
            (Instrument.patch_instrument p)), p) | p <- patches]

-- | Make the patches into a PatchMap.  This is just a version of 'patch_map'
-- that logs colliding patches and is hence in IO.
logged_patch_map :: Instrument.Synth -> [Instrument.Patch] -> IO SynthDesc
logged_patch_map synth patches = do
    let (pmap, rejects) = patch_map patches
    forM_ rejects $ \(patch_name, patch) ->
        -- Printing the text is sort of a hack, because I know it contains
        -- the original filename.
        -- TODO say who it's colliding with
        Log.warn $ "discarding overlapping patch " ++ show patch_name
            ++ " text: " ++ Seq.strip (Instrument.patch_text patch)
    return (synth, pmap)

-- | Build a PatchMap for a synth that has whatever patch you name.
wildcard_patch_map :: Instrument.Patch -> PatchMap
wildcard_patch_map patch = PatchMap $ Map.singleton wildcard_inst_name patch

merge_patch_maps (PatchMap pmap0) (PatchMap pmap1) =
    PatchMap $ Map.union pmap1 pmap0

-- ** lookup

lookup_patch :: Instrument.InstrumentName -> PatchMap -> Maybe Instrument.Patch
lookup_patch inst_name (PatchMap patches) =
    case Map.lookup inst_name patches of
        Just patch -> Just patch
        Nothing -> case Map.lookup wildcard_inst_name patches of
            Just patch -> Just $ set_patch_name inst_name patch
            Nothing -> Nothing

-- * util

-- | Since instruments are stored in the index as lower case for case
-- insensitive lookup, they should be stored as lower case here too.
--
-- TODO: having two different structures for lookup seems messy, it would be
-- nicer to put it in the index flat, but not clear how best to reconcile that
-- with PatchTemplates.
lc :: String -> String
lc = map Char.toLower

-- | People like to put wacky characters in their names, but it makes them
-- hard to type.  This affects the key under which the instrument is stored
-- and therefore lookup, but the inst_name field remains unchanged.
clean_inst_name :: String -> String
clean_inst_name = Seq.replace " " "_" . unwords . words
    . filter (`elem` valid_chars) . lc

valid_chars = ['0'..'9'] ++ ['a'..'z'] ++ " _-"


join_inst :: String -> String -> Score.Instrument
join_inst synth inst_name = Score.Instrument (synth ++ "/" ++ inst_name)

split_inst :: Score.Instrument -> (String, String)
split_inst (Score.Instrument inst) = (synth, drop 1 inst_name)
    where (synth, inst_name) = break (=='/') inst

-- | Modify the patch template to have the given name.
set_patch_name :: Instrument.InstrumentName -> Instrument.Patch
    -> Instrument.Patch
set_patch_name name patch = patch { Instrument.patch_instrument =
    (Instrument.patch_instrument patch) { Instrument.inst_name = name } }

