{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Map as Map
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


-- * midi db

-- | Pair a Patch up with its code.
--
-- The code parameter is extra data that can't be defined here to avoid
-- circular imports.  Instruments want to have Cmds in them, but the "Cmd.Cmd"
-- module must import this one.  However, it would be quite annoying to have to
-- try to move this entire module into Cmd.Cmd.
type PatchCode code = (Instrument.Patch, code)

newtype MidiDb code = MidiDb {
    midi_db_map :: Map.Map Instrument.SynthName
        (Instrument.Synth, PatchMap code)
    } deriving (Show)

-- | Merge the MidiDbs, favoring instruments in the leftmost one.
merge :: MidiDb code -> MidiDb code -> (MidiDb code, [Score.Instrument])
merge (MidiDb db1) (MidiDb db2) =
    (MidiDb (Map.unionWith merge_synth db1 db2), rejects)
    where
    merge_synth (synth, pmap1) (_, pmap2) = (synth, pmap1 <> pmap2)
    rejects = concatMap find_dups (Map.zip_intersection db1 db2)
    find_dups (synth, (_, PatchMap ps1), (_, PatchMap ps2)) =
        map (join_inst synth) (Map.keys (Map.intersection ps2 ps1))

-- | Construct and validate a MidiDb, returning any errors that occurred.
midi_db :: [SynthDesc code] -> (MidiDb code, [String])
midi_db synth_pmaps = (MidiDb db_map, validate synth_pmaps)
    where
    db_map = Map.fromList
        [ (lc (Instrument.synth_name synth), (synth, pmap))
        | (synth, pmap) <- synth_pmaps]

validate :: [SynthDesc a] -> [String]
validate synth_pmaps = concatMap check_synth synth_pmaps
    where
    check_synth (synth, PatchMap patches) =
        concatMap (check_patch synth) (map fst (Map.elems patches))
    check_patch synth patch = map (\s -> prefix ++ ": " ++ s) $
        Instrument.overlapping_keyswitches (Instrument.patch_keyswitches patch)
        where
        prefix = Pretty.pretty $ join_inst
            (Instrument.synth_name synth) (Instrument.patch_name patch)

size :: MidiDb code -> Int
size (MidiDb synths) = sum $ map ssize (Map.elems synths)
    where ssize (_, PatchMap patches) = Map.size patches

empty :: MidiDb code
empty = MidiDb Map.empty

-- ** lookup

-- | This returns the instrument, if found, and the attributes that were used
-- to find a keyswitch, if any.
--
-- The attributes are returned because keyswitch matching is by subset rather
-- than exact, so this is the only way for the caller to know which attributes
-- were used.
type LookupMidiInstrument = Score.Attributes -> Score.Instrument
    -> Maybe (Instrument.Instrument, Score.Attributes)

-- | This type is nominally the backend-independent part of the instrument.
-- Of course at the moment it's MIDI only.  Once I have other backends this
-- should move back into Db.
data Info code = Info {
    info_synth :: Instrument.Synth
    , info_patch :: Instrument.Patch
    -- | Instruments can have Cmds and deriver calls, but those types can't
    -- be referenced directly here for to avoid circular imports.  The
    -- complete definition is in 'Cmd.Cmd.MidiInfo'.
    , info_code :: code
    } deriving (Show)

lookup_midi :: MidiDb code -> LookupMidiInstrument
lookup_midi midi_db attrs inst = case lookup_instrument midi_db inst of
    Nothing -> Nothing
    Just (Info synth patch _) -> Just $ make_inst synth patch inst attrs

-- | Merge a Synth and a Patch to create an Instrument.
make_inst :: Instrument.Synth -> Instrument.Patch -> Score.Instrument
    -> Score.Attributes -> (Instrument.Instrument, Score.Attributes)
make_inst synth patch score_inst attrs = (inst
        { Instrument.inst_control_map = Map.union inst_cmap synth_cmap
        , Instrument.inst_score = score_inst
        , Instrument.inst_synth = Instrument.synth_name synth
        , Instrument.inst_keyswitch = fmap fst ks_attrs
        }, maybe Score.no_attrs snd ks_attrs)
    where
    inst = Instrument.patch_instrument patch
    synth_cmap = Instrument.synth_control_map synth
    inst_cmap = Instrument.inst_control_map inst
    ks_attrs = Instrument.get_keyswitch (Instrument.patch_keyswitches patch)
        attrs

lookup_instrument :: MidiDb code -> Score.Instrument -> Maybe (Info code)
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    (patch, code) <- lookup_patch inst_name patches
    return $ Info synth patch code

lookup_patch :: Instrument.InstrumentName -> PatchMap code
    -> Maybe (Instrument.Patch, code)
lookup_patch inst_name (PatchMap patches) =
    case Map.lookup inst_name patches of
        Just (patch, code) -> Just (patch, code)
        Nothing -> case Map.lookup wildcard_inst_name patches of
            Just (patch, code) -> Just (set_patch_name inst_name patch, code)
            Nothing -> Nothing

-- * patch map

type SynthDesc code = (Instrument.Synth, PatchMap code)

newtype PatchMap code =
    PatchMap (Map.Map Instrument.InstrumentName (PatchCode code))
    deriving (Show, Monoid.Monoid)

-- | This patch takes whatever name you give.
wildcard_inst_name :: Instrument.InstrumentName
wildcard_inst_name = "*"

-- | Build a PatchMap to give to 'midi_db'.  Colliding patches are returned.
patch_map :: [PatchCode code] -> (PatchMap code, [(String, Instrument.Patch)])
patch_map patches =
    (PatchMap pmap, [(name, patch) | (name, (patch, _)) <- rejects])
    where
    (pmap, rejects) = Map.unique
        [(name patch, (patch, code)) | (patch, code) <- patches]
    name = clean_inst_name . Instrument.inst_name . Instrument.patch_instrument

-- | Make the patches into a PatchMap.  This is just a version of
-- 'patch_map' that logs colliding patches and is hence in IO.
logged_synths :: Instrument.Synth -> [PatchCode code] -> IO (SynthDesc code)
logged_synths synth patches = do
    let (pmap, rejects) = patch_map patches
    forM_ rejects $ \(patch_name, patch) ->
        -- Printing the text is sort of a hack, because I know it contains
        -- the original filename.
        -- TODO say who it's colliding with
        Log.warn $ "discarding overlapping patch " ++ show patch_name
            ++ " text: " ++ Seq.strip (Instrument.patch_text patch)
    return (synth, pmap)

-- | Build a PatchMap for a synth that has whatever patch you name.
wildcard_patch_map :: PatchCode code -> PatchMap code
wildcard_patch_map patch = PatchMap $ Map.singleton wildcard_inst_name patch

-- ** lookup

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

score_inst :: Instrument.Synth -> Instrument.Patch -> Score.Instrument
score_inst synth patch =
    join_inst (Instrument.synth_name synth) (Instrument.patch_name patch)

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

