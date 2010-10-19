{- | Instrument DB, for converting Score.Instruments, which are just names, to
    the detailed instrument in whatever backend.

    TODO
    - Midi instruments are probably tangled with non-midi instruments, but
    I can figure that out when I have non-midi instruments.
-}
module Instrument.Db where

import qualified Derive.Score as Score
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


-- * Db

-- | Static config type for the instrument db.
data Db = Db {
    -- | Search for Score instruments.
    db_search :: Search.Search
    -- | Specialized version of db_lookup that returns a Midi instrument.
    , db_lookup_midi :: MidiDb.LookupMidiInstrument
    -- | Lookup a score instrument.
    , db_lookup :: Score.Instrument -> Maybe MidiDb.Info

    -- | Internal use.  It's probably not necessary to expose these, but they
    -- can be handy for testing.
    , db_midi_db :: MidiDb.MidiDb
    , db_index :: Search.Index
    }

empty :: Db
empty = Db {
    db_search = const []
    , db_lookup_midi = \_ _ -> Nothing
    , db_lookup = const Nothing
    , db_midi_db = MidiDb.empty
    , db_index = Search.make_index MidiDb.empty
    }

-- | So Cmd.State can be showable, for debugging.
instance Show Db where
    show _ = "((InstrumentDb))"

type MakeInitialize = Midi.Channel -> Instrument.InitializePatch

db :: MidiDb.MidiDb -> Search.Index -> Db
db midi_db extra_index = Db
    (Search.search index)
    (MidiDb.lookup_midi midi_db)
    (MidiDb.lookup_instrument midi_db)
    midi_db
    index
    where index = Search.merge_indices (Search.make_index midi_db) extra_index

size :: Db -> Int
size db = MidiDb.size (db_midi_db db)
