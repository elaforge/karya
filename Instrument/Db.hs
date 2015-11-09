-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Instrument DB, for converting Score.Instruments, which are just names, to
    the detailed instrument in whatever backend.

    Technically this module is supposed to be backend-independent, while
    MidiDb is specific to MIDI, but since there's only MIDI at the moment
    it doesn't matter much.

    TODO
    - Midi instruments are probably tangled with non-midi instruments, but
    I can figure that out when I have non-midi instruments.
-}
module Instrument.Db where
import qualified Data.Map as Map

import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


-- | Static config type for the instrument db.
data Db code = Db {
    -- | Specialized version of db_lookup that returns a Midi instrument.
    db_lookup_midi :: Score.Instrument -> Maybe Instrument.Instrument
    -- | Lookup a score instrument.
    , db_lookup :: Score.Instrument -> Maybe (MidiDb.Info code)

    -- | Internal use.  It's probably not necessary to expose this, but it can
    -- be handy for testing.
    , db_midi_db :: MidiDb.MidiDb code
    }

empty :: Db code
empty = Db {
    db_lookup_midi = const Nothing
    , db_lookup = const Nothing
    , db_midi_db = MidiDb.empty
    }

-- | So Cmd.State can be showable, for debugging.
instance Show (Db code) where
    show _ = "((InstrumentDb))"

type MakeInitialize = Midi.Channel -> Instrument.InitializePatch

db :: MidiDb.MidiDb code -> Db code
db midi_db = Db
    { db_lookup_midi = MidiDb.lookup_midi midi_db
    , db_lookup = MidiDb.lookup_instrument midi_db
    , db_midi_db = midi_db
    }

-- | Number of entries in the db.
size :: Db code -> Int
size db = MidiDb.size (db_midi_db db)

-- | All the synths in the db.
synths :: Db code -> [Instrument.SynthName]
synths db = Map.keys synths
    where MidiDb.MidiDb synths = (db_midi_db db)
