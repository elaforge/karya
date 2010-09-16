{- | Functions to save and load the midi db.

    Unlike in 'Cmd.Serialize', I don't bother with versions here, because this
    is intended to be just a cache.
-}
module Instrument.Serialize where
import qualified Control.Exception as Exception
import qualified Data.Time as Time
import qualified Data.Binary as Binary
import Data.Binary (Binary, get, put, getWord8, putWord8)

import qualified Util.File as File

import qualified Cmd.Serialize () -- get the Binary instances
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control

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

unserialize :: FilePath -> IO (Either Exception.SomeException SavedDb)
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

instance Binary Control.Control where
    put (Control.Control a) = put a
    get = get >>= \a -> return (Control.Control a)

instance Binary MidiDb.PatchMap where
    put (MidiDb.PatchMap a) = put a
    get = get >>= \a -> return (MidiDb.PatchMap a)

instance Binary Instrument.Patch where
    put (Instrument.Patch a b c d e f) = put a >> put b >> put c >> put d
        >> put e >> put f
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e ->
        get >>= \f ->
            return (Instrument.Patch a b c d e f)

instance Binary Instrument.Instrument where
    put (Instrument.Instrument a b c d e f g h i) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h >> put i
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h -> get >>= \i ->
            return (Instrument.Instrument a b c d e f g h i)

instance Binary Instrument.InitializePatch where
    put (Instrument.InitializeMidi a) = putWord8 0 >> put a
    put (Instrument.InitializeMessage a) = putWord8 1 >> put a
    put Instrument.NoInitialization = putWord8 2
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> get >>= \a -> return (Instrument.InitializeMidi a)
            1 -> get >>= \a -> return (Instrument.InitializeMessage a)
            2 -> return Instrument.NoInitialization
            _ -> fail "no parse for Instrument.InitializePatch"

instance Binary Instrument.KeyswitchMap where
    put (Instrument.KeyswitchMap a) = put a
    get = get >>= \a -> return (Instrument.KeyswitchMap a)

instance Binary Instrument.Keyswitch where
    put (Instrument.Keyswitch a) = put a
    get = get >>= \a -> return (Instrument.Keyswitch a)

instance Binary Score.Attributes where
    put (Score.Attributes a) = put a
    get = get >>= \a -> return (Score.Attributes a)
