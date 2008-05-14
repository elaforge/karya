module Midi.Midi where
import qualified Perform.Timestamp as Timestamp
import Data.Word (Word8)

data WriteMessage = WriteMessage {
    wmsg_dev :: WriteDevice
    , wmsg_ts :: Timestamp.Timestamp
    , wmsg_msg :: Message
    } deriving (Eq, Ord, Show)
data ReadMessage = ReadMessage {
    rmsg_dev :: ReadDevice
    , rmsg_ts :: Timestamp.Timestamp
    , rmsg_msg :: Message
    } deriving (Eq, Ord, Show)

-- | Implementation independent representation of a MIDI Device.
--
-- This can be saved to and loaded from files without regard for the devices
-- actually installed or opened.  When the Devices are opened, a mapping should
-- be established between Devices and the runtime representation.

newtype ReadDevice = ReadDevice String deriving (Eq, Ord, Show)
newtype WriteDevice = WriteDevice String deriving (Eq, Ord, Show)


data Message
    = ChannelMessage Channel ChannelMessage
    | CommonMessage CommonMessage
    | RealtimeMessage RealtimeMessage
    | UnknownMessage Word8 Word8 Word8
    deriving (Eq, Ord, Show)

type Channel = Word8 -- actually 4 bits
type Key = Word8
type Velocity = Word8
type Controller = Word8
type Program = Word8
type ControlValue = Word8
data ChannelMessage =
    NoteOff Key Velocity
    | NoteOn Key Velocity
    | Aftertouch Key ControlValue
    | ControlChange Controller ControlValue
    | ProgramChange Program
    | ChannelPressure ControlValue
    -- | Number between -0x2000 and +0x2000.
    | PitchBend Int
    -- channel mode messages (special controller values)
    | AllSoundOff
    | ResetAllControllers
    | LocalControl Bool
    | AllNotesOff
    | UndefinedChannelMode Word8 Word8
    deriving (Eq, Ord, Show)

data CommonMessage =
    -- manufacturer id, data
    SystemExclusive Word8 [Word8]
    | SongPositionPointer Int
    | SongSelect Word8
    | TuneRequest
    | EOX
    | UndefinedCommon Word8
    deriving (Eq, Ord, Show)

data RealtimeMessage = TimingClock | Start | Continue | Stop | ActiveSense
    | Reset | UndefinedRealtime Word8
    deriving (Eq, Ord, Show)
