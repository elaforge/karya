{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Data
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument


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
    rejects = concatMap find_dups (Util.Data.zip_intersection db0 db1)
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

type LookupMidiInstrument = Score.Instrument -> Maybe Instrument.Instrument

-- | Once I have other backends this should move back into Db.
data Info = Info {
    info_synth :: Instrument.Synth
    , info_patch :: Instrument.Patch
    } deriving (Show)

lookup_midi :: MidiDb -> LookupMidiInstrument
lookup_midi midi_db inst =
    case lookup_instrument midi_db inst of
        Nothing -> Nothing
        Just ((Info synth patch), ks) -> Just $ make_inst synth inst ks patch

-- | Merge a Synth and a Patch to create an Instrument.
make_inst :: Instrument.Synth -> Score.Instrument -> Maybe Instrument.Keyswitch
    -> Instrument.Patch -> Instrument.Instrument
make_inst synth (Score.Instrument score_inst) keyswitch patch = inst
        { Instrument.inst_controller_map =
            Map.unions [inst_cmap, synth_cmap, Controller.default_controllers]
        , Instrument.inst_score_name = score_inst
        , Instrument.inst_keyswitch = keyswitch
        }
    where
    inst = Instrument.patch_instrument patch
    synth_cmap = Instrument.synth_controller_map synth
    inst_cmap = Instrument.inst_controller_map inst

lookup_instrument :: MidiDb -> Score.Instrument
    -> Maybe (Info, Maybe Instrument.Keyswitch)
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name, ks_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    let patch_ks = Instrument.patch_keyswitches patch
    let keyswitch = List.find
            (\(Instrument.Keyswitch inst_ks _) -> inst_ks == ks_name)
            patch_ks
        -- If there's a ks name or the patch as keyswitches, insist it has
        -- a match, otherwise use a ks of "" or Nothing.
    when ((not (null ks_name) || not (null patch_ks))
        && Maybe.isNothing keyswitch) Nothing
    return (Info synth patch, keyswitch)

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
    (pmap, rejects) = Util.Data.unique_map
        [(clean_inst_name (Instrument.inst_name
            (Instrument.patch_instrument p)), p) | p <- patches]

load_synth_desc :: Instrument.Synth -> [Instrument.Patch] -> IO SynthDesc
load_synth_desc synth patches = do
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

lookup_patch ::
    Instrument.InstrumentName -> PatchMap -> Maybe Instrument.Patch
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


join_inst synth inst_name = Score.Instrument (synth ++ "/" ++ inst_name)
split_inst (Score.Instrument inst) = (synth, inst_name, drop 1 keyswitch)
    where
    (synth, rest) = break (=='/') inst
    (inst_name, keyswitch) = break (=='/') (drop 1 rest)

-- | Modify the patch template to have the given name.
set_patch_name :: Instrument.InstrumentName -> Instrument.Patch
    -> Instrument.Patch
set_patch_name name patch = patch { Instrument.patch_instrument =
    (Instrument.patch_instrument patch) { Instrument.inst_name = name } }

