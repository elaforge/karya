{- | Description of a midi-specific instrument, as well as the runtime midi
    device and channel mapping.
-}
module Perform.Midi.Instrument where
import qualified Data.Map as Map

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Derive.Twelve as Twelve
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Controller as Controller

import qualified Data.ByteString as B


default_scale :: Pitch.ScaleId
default_scale = Twelve.scale_id

-- | The Instrument contains all the data necessary to render
-- a Midi.Perform.Event to a midi message.  Each Event has an attached
-- Instrument.
data Instrument = Instrument {
    inst_synth :: SynthName
    , inst_name :: InstrumentName
    , inst_keyswitch :: Maybe Keyswitch
    -- | Instrument name as given in the Score.Instrument.  It's just used
    -- to print msgs, but it should be the same to avoid confusion.
    , inst_score_name :: String
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
    -- | Scale associated with this instrument, which also includes the drum
    -- map for a drum kit instrument.  This can be overridden per-project by
    -- State.state_scale_config.
    , inst_scale :: Pitch.ScaleId
    } deriving (Eq, Ord, Show)

instrument :: Synth -> InstrumentName -> Maybe Keyswitch
    -> Controller.ControllerMap -> Controller.PbRange -> Instrument
instrument synth name keyswitch cmap pb_range =
    set_instrument_name synth name keyswitch
        (Instrument "" "" Nothing "" cmap pb_range Nothing default_scale)

set_instrument_name synth name keyswitch inst = inst
    { inst_synth = synth_name synth
    , inst_name = name
    , inst_keyswitch = keyswitch
    , inst_score_name = synth_name synth ++ "/" ++ name ++ ks_str
    }
    where
    ks_str = case keyswitch of
        Just (Keyswitch ks_name _) -> "/" ++ ks_name
        _ -> ""


-- | Per-song instrument configuration.
data Config = Config {
    -- | An instrument may have multiple addresses assigned to it, which means
    -- that it can be multiplexed across multiple channels.  In addition,
    -- multiple instruments can be allocated to overlapping addresses.  An
    -- instrument wishing to use an address will emit an appropriate message to
    -- configure it (probably a keyswitch, possibly a program change).
    config_alloc :: Map.Map Score.Instrument [Addr]
    -- | If this is given, it will be used as the Addr for e.g. midi thru
    -- when it cant't figure out what instrument is involved, or if the
    -- instrument has no allocation.
    , config_default_addr :: Maybe Addr
    } deriving (Show, Read)
config inst_addrs default_addr = Config (Map.fromList inst_addrs) default_addr

-- | Midi instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)

-- | Remember the current state of each midi addr in use.  This is because
-- more than one instrument can share the same channel, so I need to emit the
-- requisite program change or keyswitch to get the channel into the right
-- state.  For synths that can't switch programs instantly, any program change
-- should probably be done by hand in advance, but keyswitches all happen
-- instantly and don't need that.
type ChannelMap = Map.Map Addr Instrument

-- | Keyswitch name and key to activate it.
data Keyswitch = Keyswitch String Midi.Key
    deriving (Eq, Ord, Show, Read)

-- * instrument db types

-- When there are multiple backends, this will have to move to a more general
-- place.

-- | A Patch has information about one specific instrument.  The performance
-- 'Instrument' and MIDI config are derived from it, via its 'Synth'.
data Patch = Patch {
	-- | The Instrument is a subset of the data available in the Patch.
	-- The patch_instrument is not necessarily the same as the one eventually
	-- used in performance, because e.g. synth controllers can get added in.
	patch_instrument :: Instrument
    , patch_initialize :: InitializePatch
    -- | Keyswitches available to this instrument, if any.  Each of these is
    -- considered its own instrument, like synth\/inst\/ks.  A keyswitch key may
    -- occur more than once, and a name of \"\" is used when the instrument is
    -- looked up without a keyswitch.
    , patch_keyswitches :: [Keyswitch]
	-- | Key-value pairs used to index the patch.
	, patch_tags :: [Tag]
    -- | Some free form text about the patch.
    , patch_text :: String
	} deriving (Eq, Show)
-- | Create a Patch with empty vals, to set them as needed.
patch inst = Patch inst NoInitialization [] [] ""

patch_name :: Patch -> InstrumentName
patch_name = inst_name . patch_instrument

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
    -- | This is redundant with InitializeMidi, but 1000 3K sysex msgs uses
    -- lots of memory and makes the browser crawl.
    | InitializeSysex B.ByteString
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage String
    | NoInitialization
    deriving (Eq, Show)

sysex_to_msg :: B.ByteString -> Maybe Midi.Message
sysex_to_msg bytes
    | B.length bytes < 3 || B.index bytes 0 /= Midi.sox_byte = Nothing
    | otherwise = Just $ Midi.CommonMessage $
        -- Should be [SOX, manufacturer_code, ..., EOX]
        Midi.SystemExclusive (B.index bytes 1) (B.unpack (B.drop 2 bytes))

patch_summary :: Patch -> String
patch_summary patch = inst_name inst ++ " -- " ++ show (patch_tags patch)
    where inst = patch_instrument patch

add_tag :: Tag -> Patch -> Patch
add_tag tag patch = patch { patch_tags = tag : patch_tags patch }
