-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import qualified Control.Monad.Identity as Identity
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import qualified System.FilePath as FilePath

import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import Global


-- * midi db

newtype MidiDb code = MidiDb {
    midi_db_map :: Map.Map Instrument.SynthName
        (Instrument.Synth, PatchMap code)
    } deriving (Show)

type SynthDesc code = (Instrument.Synth, PatchMap code)

-- | Construct and validate a MidiDb, returning any errors that occurred.
midi_db :: [SynthDesc code] -> (MidiDb code, [Text])
midi_db synth_pmaps = (MidiDb db_map, validate synth_pmaps)
    where
    db_map = Map.fromList
        [ (Text.toLower (Instrument.synth_name synth), (synth, pmap))
        | (synth, pmap) <- synth_pmaps
        ]

validate :: [SynthDesc a] -> [Text]
validate = concatMap check_synth
    where
    check_synth (synth, PatchMap patches) =
        concatMap (check_patch synth . fst) (Map.elems patches)
    check_patch synth patch = map (\s -> prefix <> ": " <> s) $
        Instrument.overlapping_attributes (Instrument.patch_attribute_map patch)
        where
        prefix = pretty $ Score.instrument
            (Instrument.synth_name synth) (Instrument.patch_name patch)

-- | Merge the MidiDbs, favoring instruments in the leftmost one.
merge :: MidiDb code -> MidiDb code -> (MidiDb code, [Score.Instrument])
merge (MidiDb db1) (MidiDb db2) =
    (MidiDb (Map.unionWith merge_synth db1 db2), rejects)
    where
    merge_synth (synth, pmap1) (_, pmap2) = (synth, pmap1 <> pmap2)
    rejects = concatMap find_dups (Map.zip_intersection db1 db2)
    find_dups (synth, (_, PatchMap ps1), (_, PatchMap ps2)) =
        map (Score.instrument synth) (Map.keys (Map.intersection ps2 ps1))

-- | Apply the given annotations to the instruments in the MidiDb, and
-- return non-existent instruments.
annotate :: Map.Map Score.Instrument [Instrument.Tag] -> MidiDb code
    -> (MidiDb code, [Score.Instrument])
annotate annots midi_db = (Map.foldrWithKey annot midi_db annots, not_found)
    where
    annot inst tags = modify_patch inst (add_tags tags)
    add_tags tags = Instrument.tags %= (tags++)
    not_found =
        filter (Maybe.isNothing . lookup_instrument midi_db) (Map.keys annots)

-- | Modify the patch with the given instrument name, if it exists.
-- If it doesn't exist but a 'wildcard_inst_name' does, a new patch will be
-- inserted.
modify_patch :: Score.Instrument
    -> (Instrument.Patch -> Instrument.Patch) -> MidiDb code -> MidiDb code
modify_patch inst modify (MidiDb synths) =
    MidiDb $ Map.adjust (second modify_pmap) synth_name synths
    where
    (synth_name, inst_name) = Score.split_inst inst
    modify_pmap (PatchMap patches)
        | Just (patch, code) <- Map.lookup inst_name patches =
            PatchMap $ Map.insert inst_name (modify patch, code) patches
        | Just (patch, code) <- Map.lookup wildcard_inst_name patches =
            PatchMap $ Map.insert inst_name
                (modify (set_patch_name inst_name patch), code) patches
        | otherwise = PatchMap patches

size :: MidiDb code -> Int
size (MidiDb synths) = sum $ map ssize (Map.elems synths)
    where ssize (_, PatchMap patches) = Map.size patches

empty :: MidiDb code
empty = MidiDb Map.empty

-- ** lookup

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

instance Pretty.Pretty code => Pretty.Pretty (Info code) where
    format (Info synth patch code) = Pretty.record "Info"
        [ ("synth", Pretty.format synth)
        , ("patch", Pretty.format patch)
        , ("code", Pretty.format code)
        ]

lookup_midi :: MidiDb code -> Score.Instrument -> Maybe Instrument.Instrument
lookup_midi midi_db inst = case lookup_instrument midi_db inst of
    Nothing -> Nothing
    Just (Info synth patch _) -> Just $ make_inst synth patch inst

-- | Merge a Synth and a Patch to create an Instrument.
make_inst :: Instrument.Synth -> Instrument.Patch -> Score.Instrument
    -> Instrument.Instrument
make_inst synth patch score_inst = inst
    { Instrument.inst_control_map = Map.union inst_cmap synth_cmap
    , Instrument.inst_score = score_inst
    , Instrument.inst_synth = Instrument.synth_name synth
    }
    where
    inst = Instrument.patch_instrument patch
    synth_cmap = Instrument.synth_control_map synth
    inst_cmap = Instrument.inst_control_map inst

lookup_instrument :: MidiDb code -> Score.Instrument -> Maybe (Info code)
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name) = Score.split_inst inst
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

newtype PatchMap code =
    PatchMap (Map.Map Instrument.InstrumentName (PatchCode code))
    deriving (Show, Monoid.Monoid)

-- | Pair a Patch up with its code.
--
-- The code parameter is extra data that can't be defined here to avoid
-- circular imports.  Instruments want to have Cmds in them, but the "Cmd.Cmd"
-- module must import this one.  However, it would be quite annoying to have
-- to move this entire module into Cmd.Cmd.
type PatchCode code = (Instrument.Patch, code)

-- | This is a name for a synth with a generic \"main\" patch.  Typically this
-- is a soft-synth that must be configured by its own UI.  Normally you'd
-- create an alias from a score-specific name to @>synth/\*@, but if you look up
-- a patch and it isn't found, the wildcard patch will be copied and returned.
-- This is sort of a shorthand for explicitly creating an alias.
-- TODO I could remove the feature if the complication outweighs the
-- convenience.
--
-- The other special behaviour that patches with the wildcard name have is that
-- 'annotate', if asked to annotate an patch that doesn't exist, will copy and
-- annotate the wildcard patch, if there is one for the relevant synth.
wildcard_inst_name :: Instrument.InstrumentName
wildcard_inst_name = "*"

-- | Build a 'PatchMap' to give to 'midi_db'.  Simplified names are generated
-- for each patch, and if names collide various heuristics are tried to
-- discard or combine them, or they are disambiguated with numbers.
patch_map :: [PatchCode code] -> (PatchMap code, [Text])
    -- ^ (PatchMap, log msgs)
patch_map patches = run $ concatMapM split =<< mapM strip_init by_name
    where
    by_name = Seq.keyed_group_on (score_instrument_name . patch_inst) patches
    patch_inst = Instrument.patch_instrument . fst
    run = first (PatchMap . Map.fromList) . Identity.runIdentity . Logger.run

    strip_init :: NamedPatch code -> Merge (NamedPatch code)
    -- If the initialization is the same, they are likely duplicates.
    strip_init (name, patches)
        | Text.null name = do
            log "dropped patches with no name" patches
            return ("", [])
        | otherwise = do
            let (unique, dups) = Seq.partition_dups
                    (Instrument.patch_initialize . fst) patches
            forM_ dups $ \(patch, dups) ->
                log ("dropped patches with the same initialization as "
                    <> details patch) dups
            return (name, unique)

    -- Remaining patches are probably different and just happened to get the
    -- same name, so number them to disambiguate.
    split :: NamedPatch code -> Merge [(Text, PatchCode code)]
    split (name, patches@(_:_:_)) = do
        let named = zip (map ((name<>) . showt) [1..]) patches
        log ("split into " <> Text.intercalate ", " (map fst named)) patches
        return named
    split (name, patches) = return $ map (name,) patches

    log _ [] = return ()
    log msg ps = Logger.log $ msg <> ": "
        <> Text.intercalate ", " (map details ps)
    details patch = Instrument.inst_name (patch_inst patch)
        <> " ("
        <> txt (FilePath.takeFileName (Instrument.patch_file (fst patch)))
        <> ")"

type NamedPatch code = (Text, [PatchCode code])
type Merge = Logger.LoggerT Text Identity.Identity

-- | Make the patches into a PatchMap.  This is just a version of
-- 'patch_map' that logs colliding patches and is hence in IO.
logged_synths :: Instrument.Synth -> [PatchCode code] -> IO (SynthDesc code)
logged_synths synth patches = do
    let (pmap, msgs) = patch_map patches
    let prefix = "synth " <> Instrument.synth_name synth <> ": "
    mapM_ (Log.notice . (prefix<>)) msgs
    return (synth, pmap)

-- | Build a PatchMap for a synth with a single wildcard patch, documented by
-- 'wildcard_inst_name'.
wildcard_patch_map :: PatchCode code -> PatchMap code
wildcard_patch_map patch = PatchMap $ Map.singleton wildcard_inst_name patch

-- * util

-- | Guess a score inst name from the instrument name.  This may not be the
-- final name because they may still be processed for uniqueness.
--
-- When instruments are being constructed, they don't have a score name.
-- That's because 'Score.Instrument's have to be globally unique so I need the
-- whole inst db to figure them out.
score_instrument_name :: Instrument.Instrument -> Instrument.InstrumentName
score_instrument_name = clean_inst_name . Instrument.inst_name

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
clean_inst_name :: Text -> Text
clean_inst_name =
    Text.dropWhileEnd (=='-') . Text.dropWhile (=='-')
        . strip_dups
        . Text.filter (`elem` Score.inst_valid_chars) . Text.map replace
        . Text.toLower
    where
    strip_dups = Text.intercalate "-" . filter (not . Text.null)
        . Text.split (=='-')
    replace c
        | c `elem` (" _/" :: [Char]) = '-'
        | otherwise = c

score_inst :: Instrument.Synth -> Instrument.Patch -> Score.Instrument
score_inst synth patch =
    Score.instrument (Instrument.synth_name synth) (Instrument.patch_name patch)

-- | Modify the patch template to have the given name.
set_patch_name :: Instrument.InstrumentName -> Instrument.Patch
    -> Instrument.Patch
set_patch_name name patch = patch { Instrument.patch_instrument =
    (Instrument.patch_instrument patch) { Instrument.inst_name = name } }
