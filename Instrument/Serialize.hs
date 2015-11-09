-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TupleSections #-}
{- | Functions to save and load the midi db.

    Unlike in "Cmd.Serialize", I don't bother with versions here, because this
    is intended to be just a cache.
-}
module Instrument.Serialize (serialize, unserialize) where
import qualified Data.Time as Time

import Util.Serialize (Serialize, get, put, get_tag, put_tag, bad_tag)
import Midi.Instances ()
import qualified Cmd.Serialize
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Search as Search
import Global


magic :: Cmd.Serialize.Magic
magic = Cmd.Serialize.Magic 'i' 'n' 's' 't'

-- | Serialize instrument definitions to a file.  The @code@ parameter is
-- restricted to @()@ since it can't be serialized.
serialize :: FilePath -> Instrument.Synth () -> IO ()
serialize fname = Cmd.Serialize.serialize magic fname <=< saved_db

-- | Unserialize instrument definitions.  Since the code was stripped off by
-- 'serialize', it must be provided on a per-patch basis to reconstitute the
-- definitions.
unserialize :: (Instrument.Patch -> code) -> FilePath
    -> IO (Either Text (Time.UTCTime, Instrument.Synth code))
unserialize code_for fname = do
    result <- Cmd.Serialize.unserialize magic fname
    return $ case result of
        Right (Just (SavedDb (time, synth))) ->
            Right (time, Instrument.modify_code code_for synth)
        Right Nothing -> Left $ txt fname <> ": file doesn't exist"
        Left err -> Left err


-- * implementation

newtype SavedDb = SavedDb (Time.UTCTime, Instrument.Synth ())
    deriving (Serialize)

saved_db :: Instrument.Synth () -> IO SavedDb
saved_db synth = SavedDb . (, synth) <$> Time.getCurrentTime

-- * instances

instance Serialize Search.Index where
    put (Search.Index a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (Search.Index a b)

instance Serialize (Instrument.Synth ()) where
    put (Instrument.Synth a b c d e) = put a >> put b >> put c >> put d >> put e
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e ->
        return (Instrument.Synth a b c d e)

instance Serialize Instrument.Patch where
    put (Instrument.Patch a b c d e f g h i j) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h >> put i >> put j
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e ->
        get >>= \f -> get >>= \g -> get >>= \h -> get >>= \i -> get >>= \j ->
            return (Instrument.Patch a b c d e f g h i j)

instance Serialize Instrument.Flag where
    put Instrument.Triggered = put_tag 0
    put Instrument.Pressure = put_tag 1
    put Instrument.ConstantPitch = put_tag 2
    get = do
        tag <- get_tag
        case tag of
            0 -> return Instrument.Triggered
            1 -> return Instrument.Pressure
            2 -> return Instrument.ConstantPitch
            _ -> bad_tag "Instrument.Flag" tag

instance Serialize Instrument.Instrument where
    put (Instrument.Instrument a b c d e f g h) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h
    get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d ->
        get >>= \e -> get >>= \f -> get >>= \g -> get >>= \h ->
            return (Instrument.Instrument a b c d e f g h)

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

instance Serialize Instrument.AttributeMap where
    put (Instrument.AttributeMap a) = put a
    get = get >>= \a -> return (Instrument.AttributeMap a)

instance Serialize Instrument.Keymap where
    put (Instrument.UnpitchedKeymap a) = put_tag 0 >> put a
    put (Instrument.PitchedKeymap a b c) = put_tag 1 >> put a >> put b >> put c
    get = do
        tag <- get_tag
        case tag of
            0 -> Instrument.UnpitchedKeymap <$> get
            1 -> Instrument.PitchedKeymap <$> get <*> get <*> get
            _ -> bad_tag "Instrument.Keymap" tag

instance Serialize Instrument.Keyswitch where
    put (Instrument.Keyswitch a) = put_tag 0 >> put a
    put (Instrument.ControlSwitch a b) = put_tag 1 >> put a >> put b
    get = do
        tag <- get_tag
        case tag of
            0 -> Instrument.Keyswitch <$> get
            1 -> Instrument.ControlSwitch <$> get <*> get
            _ -> bad_tag "Instrument.Keyswitch" tag
