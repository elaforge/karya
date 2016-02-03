-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
{- | Functions to save and load the midi db.

    Unlike in "Cmd.Serialize", I don't bother with versions here, because this
    is intended to be just a cache.
-}
module Instrument.Serialize (serialize, unserialize) where
import Util.Serialize (Serialize, get, put, get_tag, put_tag, bad_tag)
import Midi.Instances ()
import qualified Cmd.Serialize as Serialize
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Common as Common
import qualified Instrument.Search as Search


-- | Serialize instrument definitions to a file.
serialize :: FilePath -> Serialize.InstrumentDb -> IO ()
serialize = Serialize.serialize Serialize.instrument_db_magic

-- | Unserialize instrument definitions.
unserialize :: FilePath
    -> IO (Either Serialize.UnserializeError Serialize.InstrumentDb)
unserialize = Serialize.unserialize Serialize.instrument_db_magic


-- * instances

instance Serialize Serialize.InstrumentDb where
    put (Serialize.InstrumentDb a b) = put a >> put b
    get = Serialize.InstrumentDb <$> get <*> get

instance Serialize (Common.Common ()) where
    put (Common.Common a b c d) = put a >> put b >> put c >> put d
    get = Common.Common <$> get <*> get <*> get <*> get

instance Serialize Search.Index where
    put (Search.Index a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (Search.Index a b)

instance Serialize Instrument.Patch where
    put (Instrument.Patch a b c d e f g h i) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h >> put i
    get = Instrument.Patch <$> get <*> get <*> get <*> get <*> get <*> get
        <*> get <*> get <*> get

instance Serialize Instrument.Flag where
    put Instrument.Triggered = put_tag 0
    put Instrument.Pressure = put_tag 1
    put Instrument.ConstantPitch = put_tag 2
    put Instrument.HoldKeyswitch = put_tag 3
    get = do
        tag <- get_tag
        case tag of
            0 -> return Instrument.Triggered
            1 -> return Instrument.Pressure
            2 -> return Instrument.ConstantPitch
            3 -> return Instrument.HoldKeyswitch
            _ -> bad_tag "Instrument.Flag" tag

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
    put (Instrument.Aftertouch a) = put_tag 2 >> put a
    get = do
        tag <- get_tag
        case tag of
            0 -> Instrument.Keyswitch <$> get
            1 -> Instrument.ControlSwitch <$> get <*> get
            2 -> Instrument.Aftertouch <$> get
            _ -> bad_tag "Instrument.Keyswitch" tag
