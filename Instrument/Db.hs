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
-- TODO change inst_ to db_
data Db = Db {
    inst_backend :: LookupBackend
    , inst_search :: Search.Search

    -- DB for the midi backend
    , inst_lookup_midi :: LookupInstrument
    , inst_lookup_synth :: Score.Instrument -> Maybe Instrument.Synth
    , inst_midi_initialize :: Score.Instrument -> Maybe MakeInitialize

    , db_index :: Search.Index
    }

empty = Db {
    inst_backend = const Nothing
    , inst_search = const []
    , inst_lookup_midi = const Nothing
    , inst_lookup_synth = const Nothing
    , inst_midi_initialize = const Nothing
    , db_index = Search.make_index (MidiDb.midi_db [])
    }

-- So Cmd.State can be showable, for debugging.
instance Show Db where
    show _ = "<instrument_db>"

type LookupInstrument = Score.Instrument -> Maybe Instrument.Instrument
type LookupBackend = Score.Instrument -> Maybe Backend
type MakeInitialize = Midi.Channel -> Instrument.InitializePatch


db midi_db = Db
    (const (Just Midi))
    (Search.search index)
    (lookup_instrument midi_db)
    (lookup_synth midi_db)
    (midi_initialize midi_db)
    index
    where index = Search.make_index midi_db

lookup_instrument :: MidiDb.MidiDb -> LookupInstrument
lookup_instrument (MidiDb.MidiDb synths) inst = do
    let (synth_name, inst_name) = split_inst inst
    (synth, patches) <- Map.lookup synth_name synths
    patch <- lookup_patch inst_name patches
    return $ make_inst synth patch

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

