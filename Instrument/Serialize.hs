{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{- | Functions to save and load the midi db.

    Unlike in 'Cmd.Serialize', I don't bother with versions here, because this
    is intended to be just a cache.
-}
module Instrument.Serialize (serialize, unserialize) where
import qualified Data.Map as Map
import qualified Data.Time as Time

import Util.Serialize
       (Serialize, get, put, get_tag, put_tag, bad_tag)
import qualified Cmd.Serialize
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


-- | Serialize instrument definitions to a file.  Since the @code@ parameter
-- is unserializable code, it will be stripped off.
serialize :: FilePath -> [MidiDb.SynthDesc code] -> IO ()
serialize fname synths = Cmd.Serialize.serialize fname =<< saved_db synths

-- | Unserialize instrument definitions.  Since the code was stripped off by
-- 'serialize', it must be provided on a per-patch basis to reconstitute the
-- definitions.
unserialize :: (Instrument.Patch -> code) -> FilePath
    -> IO (Either String (Time.UTCTime, [MidiDb.SynthDesc code]))
unserialize code_for fname = do
    result <- Cmd.Serialize.unserialize fname
    return $ case result of
        Right (SavedDb (time, db)) -> Right (time, make_synths code_for db)
        Left err -> Left err


-- * implementation

newtype Db = Db [MidiDb.SynthDesc ()]
    deriving (Serialize)

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
    deriving (Serialize)

saved_db :: [MidiDb.SynthDesc code] -> IO SavedDb
saved_db synths = do
    utc <- Time.getCurrentTime
    return $ SavedDb (utc, make_db synths)

-- * instances

instance Serialize Search.Index where
    put (Search.Index a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (Search.Index a b)

instance Serialize Instrument.Synth where
    put (Instrument.Synth a b c) = put a >> put b >> put c
    get = get >>= \a -> get >>= \b -> get >>= \c ->
        return (Instrument.Synth a b c)

instance Serialize Control.Control where
    put (Control.Control a) = put a
    get = get >>= \a -> return (Control.Control a)

instance Serialize (MidiDb.PatchMap ()) where
    put (MidiDb.PatchMap a) = put a
    get = get >>= \a -> return (MidiDb.PatchMap a)

instance Serialize Instrument.Patch where
    put (Instrument.Patch a b c d e f g h i) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h >> put i
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e ->
        get >>= \f -> get >>= \g -> get >>= \h -> get >>= \i ->
            return (Instrument.Patch a b c d e f g h i)

instance Serialize Instrument.Flag where
    put Instrument.Triggered = put_tag 0
    put Instrument.Pressure = put_tag 1
    get = do
        tag <- get_tag
        case tag of
            0 -> return Instrument.Triggered
            1 -> return Instrument.Pressure
            _ -> bad_tag "Instrument.Flag" tag

instance Serialize Instrument.Instrument where
    put (Instrument.Instrument a b c d e f g h i) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h >> put i
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h ->
        get >>= \i ->
            return (Instrument.Instrument a b c d e f g h i)

instance Serialize Instrument.InitializePatch where
    put (Instrument.InitializeMidi a) = put_tag 0 >> put a
    put (Instrument.InitializeMessage a) = put_tag 1 >> put a
    put Instrument.NoInitialization = put_tag 2
    get = do
        tag <- get_tag
        case tag of
            0 -> get >>= \a -> return (Instrument.InitializeMidi a)
            1 -> get >>= \a -> return (Instrument.InitializeMessage a)
            2 -> return Instrument.NoInitialization
            _ -> bad_tag "Instrument.InitializePatch" tag

instance Serialize Instrument.KeyswitchMap where
    put (Instrument.KeyswitchMap a) = put a
    get = get >>= \a -> return (Instrument.KeyswitchMap a)

instance Serialize Instrument.Keyswitch where
    put (Instrument.Keyswitch a) = put_tag 0 >> put a
    put (Instrument.ControlSwitch a b) = put_tag 1 >> put a >> put b
    get = do
        tag <- get_tag
        case tag of
            0 -> get >>= \a -> return (Instrument.Keyswitch a)
            1 -> get >>= \a -> get >>= \b ->
                return (Instrument.ControlSwitch a b)
            _ -> bad_tag "Instrument.Keyswitch" tag

instance Serialize Score.Attributes where
    put = put . Score.attrs_set
    get = fmap BaseTypes.set_to_attrs get
