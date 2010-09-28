{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import qualified Midi.Midi as Midi

import qualified Util.Map as Map
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Derive as Derive
import qualified Derive.Instrument.All as Instrument.All
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument

import qualified App.Link as Link


-- * local instrument utils

-- | Utility to construct a soft synth.  Soft synths are assumed to have
-- their own internal patch management, and thus have only a single wildcard
-- patch, which can be modified if necessary by a passed in function.  In case
-- some patches are special, you can also pass named patches in to be merged.
softsynth :: Instrument.SynthName -> Maybe String -> Control.PbRange
    -> [Instrument.Patch] -> [(Midi.Control, String)]
    -> (Instrument.Patch -> Instrument.Patch) -> SynthDesc
softsynth name device pb_range patches controls set_patch =
    (synth, Monoid.mappend (wildcard_patch_map (set_patch template_patch))
        (fst (patch_map patches)))
    where
    (synth, template_patch) =
        Instrument.make_softsynth name device pb_range controls


-- * midi db

newtype MidiDb = MidiDb {
    midi_db_map :: Map.Map Instrument.SynthName
        (Instrument.Synth, PatchMap)
    } deriving (Show)

-- | An 'UnresolvedPatchMap' can be serialized, but not a 'PatchMap'.  So this
-- is a serializable of the 'MidiDb'.
newtype SerializableMidiDb = SerializableMidiDb [SynthDesc]
    deriving (Show)

-- | Merge the MidiDbs, favoring instruments in the leftmost one.
merge :: MidiDb -> MidiDb -> (MidiDb, [Score.Instrument])
merge (MidiDb db1) (MidiDb db2) =
    (MidiDb (Map.unionWith merge_synth db1 db2), rejects)
    where
    merge_synth (synth, pmap1) (_, pmap2) = (synth, Monoid.mappend pmap1 pmap2)
    rejects = concatMap find_dups (Map.zip_intersection db1 db2)
    find_dups (synth, (_, PatchMap ps1), (_, PatchMap ps2)) =
        map (join_inst synth) (Map.keys (Map.intersection ps2 ps1))

-- | Construct and validate a MidiDb, returning any errors that occurred.
midi_db :: [SynthDesc] -> (MidiDb, [String])
midi_db synth_pmaps = (MidiDb db_map, warns)
    where
    db_map = Map.fromList
        [ (lc (Instrument.synth_name synth), (synth, pmap))
        | (synth, pmap) <- resolved_synth_pmaps]
    (resolved_synth_pmaps, resolve_warns) = resolve_calls synth_pmaps
    validate_warns = validate synth_pmaps
    warns = map ("resolve "++) resolve_warns
        ++ map ("validate "++) validate_warns

resolve_calls :: [SynthDesc] -> ([(Instrument.Synth, PatchMap)], [String])
resolve_calls synth_pmaps = (resolved, concat warns)
    where
    (resolved, warns) = unzip (map resolve_pmap synth_pmaps)
    resolve_pmap :: (Instrument.Synth, UnresolvedPatchMap)
        -> ((Instrument.Synth, PatchMap), [String])
    resolve_pmap (synth, UnresolvedPatchMap pmap) =
        ((synth, PatchMap (Map.fromList resolved)), concat warns)
        where
        (resolved, warns) = unzip (map resolve (Map.assocs pmap))
        resolve (inst_name, patch) =
            ((inst_name, (patch, inst_calls)), inst_warns)
            where
            (inst_calls, warns) = resolve_patch patch
            inst_warns = map
                (\w -> Pretty.pretty (score_inst synth patch) ++ ": " ++ w)
                warns

-- | Look up the call module ID references from the patch, and return its call
-- map, along with errors for IDs which weren't found.
resolve_patch :: Instrument.Patch -> (Derive.InstrumentCalls, [String])
resolve_patch patch = (Derive.InstrumentCalls note_lookups val_lookups, warns)
    where
    (failed_notes, note_lookups) = Seq.partition_either
        [maybe (Left mod) Right (Map.lookup mod Instrument.All.note)
            | mod <- Set.toList (Instrument.patch_note_calls patch)]
    (failed_vals, val_lookups) = Seq.partition_either
        [maybe (Left mod) Right (Map.lookup mod Instrument.All.val)
            | mod <- Set.toList (Instrument.patch_val_calls patch)]
    warns = map (show_warn "note call") failed_notes
        ++ map (show_warn "val call") failed_vals
    show_warn msg (Link.ModuleId m) = msg ++ " not found: " ++ show m

validate :: [SynthDesc] -> [String]
validate synth_pmaps = concatMap check_synth synth_pmaps
    where
    check_synth (synth, UnresolvedPatchMap patches) =
        concatMap (check_patch synth) (Map.elems patches)
    check_patch synth patch = map (\s -> prefix ++ ": " ++ s) $
        Instrument.overlapping_keyswitches (Instrument.patch_keyswitches patch)
        where
        prefix = Pretty.pretty $ join_inst
            (Instrument.synth_name synth) (Instrument.patch_name patch)

size :: MidiDb -> Int
size (MidiDb synths) = sum $ map ssize (Map.elems synths)
    where ssize (_, PatchMap patches) = Map.size patches

empty :: MidiDb
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
data Info = Info {
    info_synth :: Instrument.Synth
    , info_patch :: Instrument.Patch
    , info_inst_calls :: Derive.InstrumentCalls
    }

lookup_midi :: MidiDb -> LookupMidiInstrument
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

lookup_instrument :: MidiDb -> Score.Instrument -> Maybe Info
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    (patch, inst_calls) <- lookup_patch inst_name patches
    return $ Info synth patch inst_calls

lookup_patch :: Instrument.InstrumentName -> PatchMap
    -> Maybe (Instrument.Patch, Derive.InstrumentCalls)
lookup_patch inst_name (PatchMap patches) =
    case Map.lookup inst_name patches of
        Just (patch, inst_calls) -> Just (patch, inst_calls)
        Nothing -> case Map.lookup wildcard_inst_name patches of
            Just (patch, inst_calls) ->
                Just (set_patch_name inst_name patch, inst_calls)
            Nothing -> Nothing

-- * patch map

type SynthDesc = (Instrument.Synth, UnresolvedPatchMap)

newtype UnresolvedPatchMap =
    UnresolvedPatchMap (Map.Map Instrument.InstrumentName Instrument.Patch)
    deriving (Show, Monoid.Monoid)
newtype PatchMap =
    PatchMap (Map.Map Instrument.InstrumentName
        (Instrument.Patch, Derive.InstrumentCalls))
    deriving (Show, Monoid.Monoid)

-- | This patch takes whatever name you give.
wildcard_inst_name :: Instrument.InstrumentName
wildcard_inst_name = "*"

-- | Build an UnresolvedPatchMap to give to 'midi_db'.  Colliding patches are
-- returned.
patch_map :: [Instrument.Patch]
    -> (UnresolvedPatchMap, [(String, Instrument.Patch)])
patch_map patches = (UnresolvedPatchMap pmap, rejects)
    where
    (pmap, rejects) = Map.unique
        [(clean_inst_name (Instrument.inst_name
            (Instrument.patch_instrument p)), p) | p <- patches]

-- | Make the patches into a UnresolvedPatchMap.  This is just a version of
-- 'patch_map' that logs colliding patches and is hence in IO.
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

-- | Build an UnresolvedPatchMap for a synth that has whatever patch you name.
wildcard_patch_map :: Instrument.Patch -> UnresolvedPatchMap
wildcard_patch_map patch = UnresolvedPatchMap $
    Map.singleton wildcard_inst_name patch

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

