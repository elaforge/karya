module Instrument.Db where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Derive.Score as Score
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller


-- * Db

data Backend = Midi deriving (Show)

-- | Static config type for the instrument db.
data Db = Db {
    inst_backend :: LookupBackend
    , inst_search :: Search

    -- DB for the midi backend
    , inst_lookup_midi :: LookupInstrument
    , inst_lookup_synth :: Score.Instrument -> Maybe Instrument.Synth
    , inst_midi_initialize :: Score.Instrument -> Maybe MakeInitialize
    }

empty = Db {
    inst_backend = const Nothing
    , inst_search = const []
    , inst_lookup_midi = const Nothing
    , inst_lookup_synth = const Nothing
    , inst_midi_initialize = const Nothing
    }

-- So Cmd.State can be showable, for debugging.
instance Show Db where
    show _ = "<instrument_db>"

type LookupInstrument = Score.Instrument -> Maybe Instrument.Instrument
-- | Search the db.  The input string is in the db query language, and the
-- output is the names of matching patches, along with their backend.
type Search = String -> [(Backend, Score.Instrument)]
type LookupBackend = Score.Instrument -> Maybe Backend
type MakeInitialize = Midi.Channel -> Instrument.InitializePatch


-- * midi db

data MidiDb = MidiDb
    (Map.Map Instrument.SynthName (Instrument.Synth, SynthPatches))

midi_db :: [SynthDesc] -> MidiDb
midi_db synth_map = MidiDb $ Map.fromList
    [ (Instrument.synth_name synth, (synth, patches))
    | (synth, patches) <- synth_map]

type SynthDesc = (Instrument.Synth, SynthPatches)

data SynthPatches = InventPatch
    | PatchMap (Map.Map Instrument.InstrumentName Instrument.Patch)
    deriving (Show)

patch_map :: [Instrument.Patch] -> SynthPatches
patch_map patches = PatchMap $ Map.fromList
    [(Instrument.inst_name (Instrument.patch_instrument p), p) | p <- patches]

db midi_db = Db
    (const (Just Midi))
    (search midi_db)
    (lookup_instrument midi_db)
    (lookup_synth midi_db)
    (midi_initialize midi_db)

-- | Create a search function from the MidiDb.
search :: MidiDb -> Search
search (MidiDb synths) query = map ((,) Midi) $
    concat [search_synth (Instrument.synth_name synth) patches
        | (synth, patches) <- Map.elems synths]
    where
    q_words = words (map Char.toLower query)
    search_synth synth InventPatch = [join_inst synth "*"]
    search_synth synth (PatchMap pmap) =
        map (join_inst synth) $ filter matches (Map.keys pmap)
    -- TODO also search tags
    matches inst_name =
        all (`List.isInfixOf` (map Char.toLower inst_name)) q_words

lookup_instrument :: MidiDb -> LookupInstrument
lookup_instrument (MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    return $ make_inst synth patch

lookup_patch inst_name InventPatch = Just (invent_patch inst_name)
lookup_patch inst_name (PatchMap patches) = Map.lookup inst_name patches

lookup_synth :: MidiDb -> Score.Instrument -> Maybe Instrument.Synth
lookup_synth (MidiDb synths) inst =
    fmap fst $ Map.lookup (fst (split_inst inst)) synths

midi_initialize :: MidiDb -> Score.Instrument -> Maybe MakeInitialize
midi_initialize (MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (_, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    return $ (initialize_with_chan . Instrument.patch_initialize) patch

initialize_with_chan :: Instrument.InitializePatch -> Midi.Channel
    -> Instrument.InitializePatch
initialize_with_chan init chan = case init of
        Instrument.InitializeMsg msgs ->
            Instrument.InitializeMsg (map (replace_channel chan) msgs)
        _ -> init

replace_channel chan (Midi.ChannelMessage _ msg) = Midi.ChannelMessage chan msg
replace_channel _ msg = msg

split_inst (Score.Instrument inst) = (pre, drop 1 post)
    where (pre, post) = break (=='/') inst
join_inst synth inst_name = Score.Instrument $ synth ++ "/" ++ inst_name

-- | Merge a Synth and a Patch to create an Instrument.
make_inst :: Instrument.Synth -> Instrument.Patch -> Instrument.Instrument
make_inst synth patch = inst { Instrument.inst_controller_map =
        Map.union (Instrument.inst_controller_map inst) cmap }
    where
    inst = Instrument.patch_instrument patch
    cmap = Instrument.synth_controller_map synth

-- | Create a patch from just a name using default settings.
invent_patch :: Instrument.InstrumentName -> Instrument.Patch
invent_patch name =
    Instrument.Patch inst (Instrument.tags []) Instrument.NoInitialization
    where
    inst = Instrument.instrument name Controller.empty_map (-2, 2) Nothing

