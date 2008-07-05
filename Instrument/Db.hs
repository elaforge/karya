module Instrument.Db where
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Derive.Score as Score
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


-- * Db

data Backend = Midi deriving (Show)

-- | Static config type for the instrument db.
data Db = Db {
    -- | Search for Score instruments.
    db_search :: Search.Search
    -- | Specialized version of db_lookup that returns a Midi instrument.
    , db_lookup_midi :: LookupMidiInstrument
    -- | Lookup a score instrument.
    , db_lookup :: Score.Instrument -> Maybe Info

    -- | Internal use, these are probably not totally necessary but can be
    -- handy.
    , db_midi_db :: MidiDb.MidiDb
    , db_index :: Search.Index
    }

data Info = MidiInfo Instrument.Synth Instrument.Patch
    deriving (Show)

empty = Db {
    db_search = const []
    , db_lookup_midi = const Nothing
    , db_lookup = const Nothing
    , db_midi_db = MidiDb.empty
    , db_index = Search.make_index (MidiDb.midi_db [])
    }

-- So Cmd.State can be showable, for debugging.
instance Show Db where
    show _ = "<instrument_db>"

type LookupMidiInstrument = Score.Instrument -> Maybe Instrument.Instrument
type LookupBackend = Score.Instrument -> Maybe Backend
type MakeInitialize = Midi.Channel -> Instrument.InitializePatch


db midi_db extra_index = Db
    (Search.search index)
    (lookup_midi midi_db)
    (lookup_instrument midi_db)
    midi_db
    index
    where index = Search.merge_indices (Search.make_index midi_db) extra_index

size db = MidiDb.size (db_midi_db db)

lookup_instrument :: MidiDb.MidiDb -> Score.Instrument -> Maybe Info
lookup_instrument (MidiDb.MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    return $ MidiInfo synth patch

lookup_midi :: MidiDb.MidiDb -> LookupMidiInstrument
lookup_midi midi_db inst = case lookup_instrument midi_db inst of
    Nothing -> Nothing
    Just (MidiInfo synth patch) -> Just (make_inst synth patch)

lookup_patch inst_name (MidiDb.PatchTemplate patch) =
    Just (from_template patch inst_name)
lookup_patch inst_name (MidiDb.PatchMap patches) = Map.lookup inst_name patches

lookup_synth :: MidiDb.MidiDb -> Score.Instrument -> Maybe Instrument.Synth
lookup_synth (MidiDb.MidiDb synths) inst =
    fmap fst $ Map.lookup (fst (split_inst inst)) synths

midi_initialize :: MidiDb.MidiDb -> Score.Instrument -> Maybe MakeInitialize
midi_initialize (MidiDb.MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (_, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    return $ (initialize_with_chan . Instrument.patch_initialize) patch

initialize_with_chan :: Instrument.InitializePatch -> Midi.Channel
    -> Instrument.InitializePatch
initialize_with_chan init chan = case init of
        Instrument.InitializeMidi msgs ->
            Instrument.InitializeMidi (map (replace_channel chan) msgs)
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

-- | Modify the patch template to have the given name.
from_template :: Instrument.Patch -> Instrument.InstrumentName
    -> Instrument.Patch
from_template patch name = patch { Instrument.patch_instrument =
    (Instrument.patch_instrument patch) { Instrument.inst_name = name } }

