{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | Description of a midi-specific instrument, as well as the runtime midi
device and channel mapping.
-}
module Perform.Midi.Instrument where
import qualified Data.Generics as Generics
import qualified Data.Map as Map

import Util.Pretty
import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Perform.Midi.Controller as Controller


-- | The Instrument contains all the data necessary to render
-- a Midi.Perform.Event to a midi message.  Each Event has an attached
-- Instrument.
data Instrument = Instrument {
    inst_name :: InstrumentName
	-- | Map controller names to a controller number.  Some controllers are
	-- shared by all midi instruments, but some instruments have special
	-- controllers.
	, inst_controller_map :: Controller.ControllerMap

    , inst_pitch_bend_range :: Controller.PbRange
    -- | Time from NoteOff to inaudible, in seconds.  This is used to determine
    -- note overlap for the purposes of channel allocation.  It could also
    -- be used to automatically trim control msgs, but I don't do that yet.
    -- (but I always allocate LRU, so it shouldn't make a difference, right?)
    , inst_decay :: Maybe Double
    } deriving (Eq, Ord, Show)
instrument = Instrument

-- | Midi instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)

-- | Per-song instrument configuration.
data Config  = Config {
    -- | An instrument may occur multiple times in this map, which means it
    -- may be multiplexed across multiple channels.  You can also assign it
    -- multiple devices the same way, but the use for that seems more limited.
    --
    -- TODO it would be more natural to go Instrument -> [Addr] and that's
    -- what everyone converts this into anyway.  And multiple instruments
    -- sharing the same Addr is also potentially legitimate.
    config_alloc :: Map.Map Addr Score.Instrument
    -- | If this is given, it will be used as the Addr for e.g. midi thru
    -- when it cant't figure out what instrument is involved, or if the
    -- instrument has no allocation.
    , config_default_addr :: Maybe Addr
    } deriving (Show, Read, Generics.Data, Generics.Typeable)
config addr_insts default_addr = Config (Map.fromList addr_insts) default_addr

instance Pretty Instrument where
    pretty inst = "<inst: " ++ inst_name inst ++ ">"


-- * instrument db types

-- When there are multiple backends, this will have to move to a more general
-- place.

-- | Patch is information about one specific instrument.  The performance
-- Instrument and MIDI config are derived from it, via its Synth.
data Patch = Patch {
	-- | The Instrument is a subset of the data available in the Patch.
	-- The patch_instrument is not necessarily the same as the one eventually
	-- used in performance, because e.g. synth controllers can get added in.
	patch_instrument :: Instrument
    , patch_initialize :: InitializePatch
	-- | Key-value pairs used to index the patch.
	, patch_tags :: [Tag]
    -- | Some free form text about the patch.
    , patch_text :: String
	} deriving (Eq, Show)
patch = Patch

type Tag = (TagKey, TagVal)
tag :: String -> String -> Tag
tag = (,)
type TagKey = String
type TagVal = String

-- | A Synth defines common features for a set of instruments, like device and
-- controllers.
data Synth = Synth {
    -- | Uniquely defines the synth, and is indexed by the
    -- Instrument.inst_synth field.
    synth_name :: SynthName
    , synth_device :: Midi.WriteDevice
    -- | Often synths have a set of common controllers in addition to the
    -- global midi defaults.
    , synth_controller_map :: Controller.ControllerMap
    } deriving (Eq, Show)

synth name wdev controllers =
    Synth name (Midi.WriteDevice wdev) (Controller.controller_map controllers)

type SynthName = String
type InstrumentName = String

-- | Describe how an instrument should be initialized before it can be played.
data InitializePatch =
    -- | Send these msgs to initialize the patch.  Probably a patch change or
    -- a sysex.
    InitializeMidi [Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage String
    | NoInitialization
    deriving (Eq, Show)

patch_summary :: Patch -> String
patch_summary patch = inst_name inst ++ " -- " ++ show (patch_tags patch)
    where inst = patch_instrument patch

add_tag :: Tag -> Patch -> Patch
add_tag tag patch = patch { patch_tags = tag : patch_tags patch }
