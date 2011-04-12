{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{- | Functions to save and load the midi db.

    Unlike in 'Cmd.Serialize', I don't bother with versions here, because this
    is intended to be just a cache.
-}
module Instrument.Serialize (serialize, unserialize) where
import qualified Control.Exception as Exception
import qualified Data.Binary as Binary
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Util.File as File

import qualified Cmd.Serialize () -- get the Binary instances
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control

import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


-- | Serialize instrument definitions to a file.  Since the @code@ parameter
-- is unserializable code, it will be stripped off.
serialize :: FilePath -> [MidiDb.SynthDesc code] -> IO ()
serialize fname synths = do
    saved <- saved_db synths
    File.backup_file fname
    Binary.encodeFile fname saved

-- | Unserialize instrument definitions.  Since the code was stripped off by
-- 'serialize', it must be provided on a per-patch basis to reconstitute the
-- definitions.
unserialize :: (Instrument.Patch -> code) -> FilePath
    -> IO (Either Exception.SomeException
        (Time.UTCTime, [MidiDb.SynthDesc code]))
unserialize code_for fname = Exception.try $ do
    SavedDb (time, db) <- Binary.decodeFile fname
    return (time, make_synths code_for db)


-- * implementation

newtype Db = Db [MidiDb.SynthDesc ()]
    deriving (Binary)

make_db :: [MidiDb.SynthDesc code] -> Db
make_db synths = Db [(synth, strip patches) | (synth, patches) <- synths]
    where
    strip (MidiDb.PatchMap patches) = MidiDb.PatchMap $
        Map.map (\(p, _) -> (p, ())) patches

make_synths :: (Instrument.Patch -> code) -> Db -> [MidiDb.SynthDesc code]
make_synths code_for (Db synths) =
    [(synth, insert patches) | (synth, patches) <- synths]
    where
    insert (MidiDb.PatchMap patches) = MidiDb.PatchMap $
        Map.map (\(p, _) -> (p, code_for p)) patches

newtype SavedDb = SavedDb (Time.UTCTime, Db)
    deriving (Binary)

saved_db :: [MidiDb.SynthDesc code] -> IO SavedDb
saved_db synths = do
    utc <- Time.getCurrentTime
    return $ SavedDb (utc, make_db synths)

-- * instances

instance Binary Search.Index where
    put (Search.Index a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (Search.Index a b)

instance Binary Instrument.Synth where
    put (Instrument.Synth a b c) = put a >> put b >> put c
    get = get >>= \a -> get >>= \b -> get >>= \c ->
        return (Instrument.Synth a b c)

instance Binary Control.Control where
    put (Control.Control a) = put a
    get = get >>= \a -> return (Control.Control a)

instance Binary (MidiDb.PatchMap ()) where
    put (MidiDb.PatchMap a) = put a
    get = get >>= \a -> return (MidiDb.PatchMap a)

instance Binary Instrument.Patch where
    put (Instrument.Patch a b c d e f) = put a >> put b >> put c
        >> put d >> put e >> put f
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
