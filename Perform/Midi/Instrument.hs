{- | Description of a midi-specific instrument, as well as the runtime midi
device and channel mapping.
-}
module Perform.Midi.Instrument where
import qualified Data.Map as Map

import qualified Midi.Midi as Midi


data Instrument = Instrument {
    -- | Patch name.
    inst_name :: String
    -- | Name of the synth or program that the patch lives in.  Could be used
    -- with a synth->device mapping to figure out the device it should be
    -- mapped to in config_devices.
    , inst_synth :: String
    , inst_initialize :: InitializeInstrument
    -- | Pitchbend range in tempered semitones below and above unity.
    , inst_pitch_bend_range :: PbRange
    -- | Time from NoteOff to inaudible, in seconds.  Optional, but if it's
    -- supplied, channel allocation can be smarter (or can it?  it should go
    -- LRU on the channels anyway)
    -- At least I can know when there's no point emitting control msgs.
    , inst_decay :: Maybe Double
    } deriving (Eq, Ord, Show)

type PbRange = (Int, Int)

-- | Midi instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)

-- | Per-song instrument configuration.
data Config  = Config {
    -- | An instrument may occur multiple times in this map, which means it
    -- may be multiplexed across multiple channels.  You can also assign it
    -- multiple devices the same way, but the use for that seems more limited.
    config_alloc :: Map.Map Addr Instrument
    } deriving (Show)

-- | Describe how an instrument should be initialized before it can be played.
data InitializeInstrument =
    -- | Send these msgs to initialize the patch.
    InitializeMidi [Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage String
    deriving (Eq, Ord, Show)
