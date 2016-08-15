-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
{- | Functions to save and load the midi db.

    Unlike in "Cmd.Serialize", I don't bother with versions here, because this
    is intended to be just a cache.
-}
module Instrument.Serialize (serialize, unserialize, InstrumentDb(..)) where
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Util.Serialize as Serialize
import Util.Serialize (Serialize, get, put, get_tag, put_tag, bad_tag)
import Midi.Instances ()
import Cmd.Serialize ()
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Search as Search


-- | Serialize instrument definitions to a file.
serialize :: FilePath -> InstrumentDb -> IO ()
serialize = Serialize.serialize instrument_db_magic

-- | Unserialize instrument definitions.
unserialize :: FilePath -> IO (Either Serialize.UnserializeError InstrumentDb)
unserialize = Serialize.unserialize instrument_db_magic

instrument_db_magic :: Serialize.Magic InstrumentDb
instrument_db_magic = Serialize.Magic 'i' 'n' 's' 't'

-- | Time serialized, patches.
data InstrumentDb = InstrumentDb
    Time.UTCTime (Map.Map InstTypes.Name (Patch.Patch, Common.Common ()))

-- * instances

instance Serialize InstrumentDb where
    put (InstrumentDb a b) = put a >> put b
    get = InstrumentDb <$> get <*> get

instance Serialize (Common.Common ()) where
    put (Common.Common a b c d) = put a >> put b >> put c >> put d
    get = Common.Common <$> get <*> get <*> get <*> get

instance Serialize Search.Index where
    put (Search.Index a b) = put a >> put b
    get = get >>= \a -> get >>= \b -> return (Search.Index a b)

instance Serialize Patch.Patch where
    put (Patch.Patch a b c d e f g h i) = put a >> put b >> put c
        >> put d >> put e >> put f >> put g >> put h >> put i
    get = Patch.Patch <$> get <*> get <*> get <*> get <*> get <*> get
        <*> get <*> get <*> get

instance Serialize Patch.Flag where
    put Patch.Triggered = put_tag 0
    put Patch.Pressure = put_tag 1
    put Patch.ConstantPitch = put_tag 2
    put Patch.HoldKeyswitch = put_tag 3
    put Patch.ResumePlay = put_tag 4
    get = do
        tag <- get_tag
        case tag of
            0 -> return Patch.Triggered
            1 -> return Patch.Pressure
            2 -> return Patch.ConstantPitch
            3 -> return Patch.HoldKeyswitch
            4 -> return Patch.ResumePlay
            _ -> bad_tag "Patch.Flag" tag

instance Serialize Patch.InitializePatch where
    put (Patch.InitializeMidi a) = put_tag 0 >> put a
    put (Patch.InitializeMessage a) = put_tag 1 >> put a
    put Patch.NoInitialization = put_tag 2
    get = do
        tag <- get_tag
        case tag of
            0 -> get >>= \a -> return (Patch.InitializeMidi a)
            1 -> get >>= \a -> return (Patch.InitializeMessage a)
            2 -> return Patch.NoInitialization
            _ -> bad_tag "Patch.InitializePatch" tag

instance Serialize Patch.Keymap where
    put (Patch.UnpitchedKeymap a) = put_tag 0 >> put a
    put (Patch.PitchedKeymap a b c) = put_tag 1 >> put a >> put b >> put c
    get = do
        tag <- get_tag
        case tag of
            0 -> Patch.UnpitchedKeymap <$> get
            1 -> Patch.PitchedKeymap <$> get <*> get <*> get
            _ -> bad_tag "Patch.Keymap" tag

instance Serialize Patch.Keyswitch where
    put (Patch.Keyswitch a) = put_tag 0 >> put a
    put (Patch.ControlSwitch a b) = put_tag 1 >> put a >> put b
    put (Patch.Aftertouch a) = put_tag 2 >> put a
    get = do
        tag <- get_tag
        case tag of
            0 -> Patch.Keyswitch <$> get
            1 -> Patch.ControlSwitch <$> get <*> get
            2 -> Patch.Aftertouch <$> get
            _ -> bad_tag "Patch.Keyswitch" tag
