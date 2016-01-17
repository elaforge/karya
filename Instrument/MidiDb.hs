-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The MidiDb type.  Split from Instrument.Db to avoid circular imports.
-}
module Instrument.MidiDb where
import qualified Control.Monad.Identity as Identity
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified System.FilePath as FilePath

import qualified Util.Logger as Logger
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import Global


-- * midi db

newtype MidiDb code =
    MidiDb (Map.Map Instrument.SynthName (Instrument.Synth code))
    deriving (Show)

-- | Construct and validate a MidiDb, returning any errors that occurred.
midi_db :: [Instrument.Synth code] -> (MidiDb code, [Text])
midi_db synths = (MidiDb synth_map, validate synths)
    where synth_map = Map.fromList $ Seq.key_on Instrument.synth_name synths

validate :: [Instrument.Synth code] -> [Text]
validate = concatMap check_synth
    where
    check_synth synth = concatMap (check_patch synth . fst) $
        Map.elems (Instrument.synth_patches synth)
    check_patch synth patch = map (\s -> prefix <> ": " <> s) $
        Instrument.overlapping_attributes (Instrument.patch_attribute_map patch)
        where
        prefix = pretty $ instrument
            (Instrument.synth_name synth) (Instrument.patch_name patch)

-- | Merge the MidiDbs, favoring instruments in the leftmost one.
merge :: MidiDb code -> MidiDb code -> (MidiDb code, [Score.Instrument])
merge (MidiDb synths1) (MidiDb synths2) =
    (MidiDb (Map.unionWith merge_synth synths1 synths2), rejects)
    where
    merge_synth synth1 synth2 =
        (Instrument.patches %= (Instrument.synth_patches synth2 <>)) synth1
    rejects = concatMap find_dups (Map.zip_intersection synths1 synths2)
    find_dups (synth_name, synth1, synth2) =
        map (instrument synth_name) $ Map.keys $ Map.intersection
            (Instrument.synth_patches synth1) (Instrument.synth_patches synth2)

-- | Apply the given annotations to the instruments in the MidiDb, and
-- return non-existent instruments.
annotate :: Map.Map Score.Instrument [Instrument.Tag] -> MidiDb code
    -> (MidiDb code, [Score.Instrument])
annotate annots midi_db = Map.foldrWithKey modify (midi_db, []) annots
    where
    modify inst tags (midi_db, not_found) =
        case modify_patch inst (add_tags tags) midi_db of
            Nothing -> (midi_db, inst : not_found)
            Just midi_db -> (midi_db, not_found)
    add_tags tags = Instrument.tags %= (tags++)

-- | Modify the given instrument, or Nothing if it doesn't exist.
modify_patch :: Score.Instrument
    -> (Instrument.Patch -> Instrument.Patch) -> MidiDb code
    -> Maybe (MidiDb code)
modify_patch inst modify (MidiDb synths) = do
    let (synth_name, inst_name) = Score.split_instrument inst
    synth <- Map.lookup synth_name synths
    (patch, code) <- Map.lookup inst_name (Instrument.synth_patches synth)
    -- TODO this is just
    -- synths[synth_name].patches[inst_name] = (modify patch, code)
    -- I'll bet lenses could combine the lookup and modification, failing with
    -- Nothing.
    -- Lens.map synth_name # patches # Lens.map inst_name # fst_ %= modify
    return $ MidiDb $
        Map.adjust (Instrument.add_patches [(modify patch, code)])
            synth_name synths

-- | Number of patches in the db.
size :: MidiDb code -> Int
size (MidiDb synths) =
    sum $ map (Map.size . Instrument.synth_patches) $ Map.elems synths

empty :: MidiDb code
empty = MidiDb Map.empty

-- ** lookup

-- | This type is nominally the backend-independent part of the instrument.
-- Of course at the moment it's MIDI only.  Once I have other backends this
-- should move back into Db.
data Info code = Info {
    info_synth :: Instrument.Synth code
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
    Just (Info synth patch _) -> Just $ make_instrument synth patch inst

-- | Merge a Synth and a Patch to create an Instrument.
make_instrument :: Instrument.Synth code -> Instrument.Patch -> Score.Instrument
    -> Instrument.Instrument
make_instrument synth patch score_inst = inst
    { Instrument.inst_control_map = inst_cmap <> synth_cmap
    , Instrument.inst_score = score_inst
    , Instrument.inst_synth = Instrument.synth_name synth
    }
    where
    inst = Instrument.patch_instrument patch
    synth_cmap = Instrument.synth_control_map synth
    inst_cmap = Instrument.inst_control_map inst

lookup_instrument :: MidiDb code -> Score.Instrument -> Maybe (Info code)
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name) = Score.split_instrument inst
    synth <- Map.lookup synth_name synths
    (patch, code) <- Map.lookup inst_name (Instrument.synth_patches synth)
    return $ Info synth patch code

-- * patch map

type PatchMap code = Map.Map Instrument.InstrumentName (PatchCode code)

-- | Pair a Patch up with its code.
--
-- The code parameter is extra data that can't be defined here to avoid
-- circular imports.  Instruments want to have Cmds in them, but the "Cmd.Cmd"
-- module must import this one.  However, it would be quite annoying to have
-- to move this entire module into Cmd.Cmd.
type PatchCode code = (Instrument.Patch, code)

-- | Build a 'PatchMap' to give to 'midi_db'.  Simplified names are generated
-- for each patch, and if names collide various heuristics are tried to
-- discard or combine them, or they are disambiguated with numbers.
verify_patches :: [PatchCode code] -> (PatchMap code, [Text])
verify_patches patches = run $ concatMapM split =<< mapM strip_init by_name
    where
    by_name = Seq.keyed_group_sort (score_instrument_name . patch_inst) patches
    patch_inst = Instrument.patch_instrument . fst
    run = first Map.fromList . Identity.runIdentity . Logger.run

    strip_init :: NamedPatch code -> Merge (NamedPatch code)
    -- If the initialization is the same, they are likely duplicates.
    strip_init (name, patches) = do
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

-- * util

-- | Create an Instrument with the full synth address.
instrument :: Text -> Text -> Score.Instrument
instrument synth name = Score.instrument $ synth <> "/" <> name

-- | Guess a score inst name from the instrument name.  This may not be the
-- final name because they may still be processed for uniqueness.
--
-- When instruments are being constructed, they don't have a score name.
-- That's because 'Score.Instrument's have to be globally unique so I need the
-- whole inst db to figure them out.
score_instrument_name :: Instrument.Instrument -> Instrument.InstrumentName
score_instrument_name = clean_instrument_name . Instrument.inst_name

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
clean_instrument_name :: Text -> Text
clean_instrument_name =
    Text.dropWhileEnd (=='-') . Text.dropWhile (=='-')
        . strip_dups
        . Text.filter (`elem` Score.instrument_valid_chars) . Text.map replace
        . Text.toLower
    where
    strip_dups = Text.intercalate "-" . filter (not . Text.null)
        . Text.split (=='-')
    replace c
        | c `elem` (" _/" :: [Char]) = '-'
        | otherwise = c

-- | Modify the patch template to have the given name.
set_patch_name :: Instrument.InstrumentName -> Instrument.Patch
    -> Instrument.Patch
set_patch_name name patch = patch { Instrument.patch_instrument =
    (Instrument.patch_instrument patch) { Instrument.inst_name = name } }
