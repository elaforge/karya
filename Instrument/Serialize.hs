{- | Functions to save and load the midi db.

Unlike in Cmd.Serialize, I don't bother with versions here, because this is
intended to be just a cache.
-}
module Instrument.Serialize where
import qualified Control.Exception as Exception
import qualified Data.Time as Time
import qualified Data.Binary as Binary
import Data.Binary (Binary, get, put, getWord8, putWord8)

import qualified Util.File as File

import qualified Cmd.Serialize () -- get the Binary instances
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


data SavedDb = SavedDb {
    save_creation_date :: Time.UTCTime
    , save_midi_db :: MidiDb.MidiDb
    , save_index :: Search.Index
    } deriving (Show)
saved_db midi_db index = do
    utc <- Time.getCurrentTime
    return $ SavedDb utc midi_db index

serialize :: FilePath -> MidiDb.MidiDb -> Search.Index -> IO ()
serialize fname midi_db index = do
    saved <- saved_db midi_db index
    File.backup_file fname
    Binary.encodeFile fname saved

unserialize :: FilePath -> IO (Either Exception.Exception SavedDb)
unserialize fname = Exception.try $ do
    st <- Binary.decodeFile fname
    -- Cheap strict decode, as in Cmd.Serialize.
    length (show st) `seq` return st

-- * implementation

instance Binary SavedDb where
    put (SavedDb a b c) = put a >> put b >> put c
    get = get >>= \a -> get >>= \b -> get >>= \c -> return (SavedDb a b c)

instance Binary Search.Index where
    put (Search.Index a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (Search.Index a b)


-- ** MidiDb

instance Binary MidiDb.MidiDb where
    put (MidiDb.MidiDb a) = put a
    get = get >>= \a -> return (MidiDb.MidiDb a)

instance Binary Instrument.Synth where
    put (Instrument.Synth a b c) = put a >> put b >> put c
    get = get >>= \a -> get >>= \b -> get >>= \c ->
        return (Instrument.Synth a b c)

instance Binary Controller.Controller where
    put (Controller.Controller a) = put a
    get = get >>= \a -> return (Controller.Controller a)

instance Binary MidiDb.SynthPatches where
    put (MidiDb.PatchTemplate a) = putWord8 0 >> put a
    put (MidiDb.PatchMap a) = putWord8 1 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> get >>= \a -> return (MidiDb.PatchTemplate a)
            1 -> get >>= \a -> return (MidiDb.PatchMap a)
            _ -> fail "no parse for MidiDb.SynthPatches"

instance Binary Instrument.Patch where
    put (Instrument.Patch a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Instrument.Patch a b c d)

instance Binary Instrument.Instrument where
    put (Instrument.Instrument a b c d) = put a >> put b >> put c >> put d
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        return (Instrument.Instrument a b c d)

instance Binary Instrument.InitializePatch where
    put (Instrument.InitializeMidi a) = putWord8 0 >> put a
    put (Instrument.InitializeMessage a) = putWord8 1 >> put a
    put Instrument.NoInitialization = putWord8 2
    put (Instrument.InitializeSysex a) = putWord8 3 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> get >>= \a -> return (Instrument.InitializeMidi a)
            1 -> get >>= \a -> return (Instrument.InitializeMessage a)
            2 -> return Instrument.NoInitialization
            3 -> get >>= \a -> return (Instrument.InitializeSysex a)
            _ -> fail "no parse for MidiDb.SynthPatches"
