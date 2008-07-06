module Instrument.Db where
import qualified Data.Map as Map

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
    , db_lookup :: Score.Instrument -> Maybe MidiDb.Info

    -- | Internal use, these are probably not totally necessary but can be
    -- handy.
    , db_midi_db :: MidiDb.MidiDb
    , db_index :: Search.Index
    }

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
    (MidiDb.lookup_instrument midi_db)
    midi_db
    index
    where index = Search.merge_indices (Search.make_index midi_db) extra_index

size db = MidiDb.size (db_midi_db db)

lookup_midi :: MidiDb.MidiDb -> LookupMidiInstrument
lookup_midi midi_db inst = case MidiDb.lookup_instrument midi_db inst of
    Nothing -> Nothing
    Just (MidiDb.MidiInfo synth patch) -> Just (make_inst synth patch)

-- | Merge a Synth and a Patch to create an Instrument.
make_inst :: Instrument.Synth -> Instrument.Patch -> Instrument.Instrument
make_inst synth patch = inst { Instrument.inst_controller_map =
        Map.union (Instrument.inst_controller_map inst) cmap }
    where
    inst = Instrument.patch_instrument patch
    cmap = Instrument.synth_controller_map synth
