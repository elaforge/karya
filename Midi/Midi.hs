{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Midi.Midi where
import Data.Bits
import qualified Data.Generics as Generics
import qualified Perform.Timestamp as Timestamp
import Data.Word (Word8)

-- * devices

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

newtype ReadDevice = ReadDevice String
    deriving (Eq, Ord, Show, Generics.Data, Generics.Typeable)
newtype WriteDevice = WriteDevice String
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)


-- * constructors

-- | Emit a program change with bank in [msb, lsb, pchange] order.
program_change :: Integer -> Integer -> [Message]
program_change bank program = map (ChannelMessage 0)
    [ ControlChange cc_bank_msb msb, ControlChange cc_bank_lsb lsb
    , ProgramChange (fromIntegral program)
    ]
    where (msb, lsb) = split14 (fromIntegral bank)

cc_bank_msb, cc_bank_lsb :: Controller
cc_bank_msb = 0
cc_bank_lsb = 32

-- * predicates

-- | Check to make sure midi msg vals are all in range.
valid_msg :: Message -> Bool
valid_msg (ChannelMessage chan msg) =
    0 <= chan && chan < 16 && valid_chan_msg msg
valid_msg msg = error $ "unknown msg: " ++ show msg
val7 v = 0 <= v && v < 128
valid_chan_msg msg = case msg of
    ControlChange cc val -> val7 cc && val7 val
    NoteOn key vel -> val7 key && val7 vel
    NoteOff key vel -> val7 key && val7 vel
    _ -> error $ "unknown msg: " ++ show msg

is_cc (ChannelMessage _ (ControlChange _ _)) = True
is_cc _ = False

is_sysex (CommonMessage (SystemExclusive _ _)) = True
is_sysex _ = False

is_note (ChannelMessage _ (NoteOn _ _)) = True
is_note (ChannelMessage _ (NoteOff _ _)) = True
is_note _ = False

-- * types

data Message
    = ChannelMessage Channel ChannelMessage
    | CommonMessage CommonMessage
    | RealtimeMessage RealtimeMessage
    | UnknownMessage Word8 Word8 Word8
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- TODO using Word8 here is kind of iffy.  Word8s silently overflow after 0xff.
-- On the other hand, these all have 7 bit ranges, so I can still check for
-- out of range values, at least until it wraps.
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
    -- | channel mode messages (special controller values)
    | AllSoundOff
    | ResetAllControllers
    | LocalControl Bool
    | AllNotesOff
    | UndefinedChannelMode Word8 Word8
    deriving (Eq, Ord, Read, Show, Generics.Data, Generics.Typeable)

data CommonMessage =
    -- | manufacturer id, data including eox
    SystemExclusive Word8 [Word8]
    | SongPositionPointer Int
    | SongSelect Word8
    | TuneRequest
    | EOX
    | UndefinedCommon Word8
    deriving (Eq, Ord, Read, Show, Generics.Data, Generics.Typeable)

data RealtimeMessage = TimingClock | Start | Continue | Stop | ActiveSense
    | Reset | UndefinedRealtime Word8
    deriving (Eq, Ord, Read, Show, Generics.Data, Generics.Typeable)

-- * util

-- | Split an Int into (msb, lsb)
split14 :: Int -> (Word8, Word8)
split14 i = (fromIntegral (shiftR i 7 .&. 0x7f), fromIntegral (i .&. 0x7f))

-- | Split an Int into two 7-bit Word8s, or go the other way.  MIDI sends
-- the lsb first, so the args will usually be swapped.
join14 :: Word8 -> Word8 -> Int
join14 msb lsb = fromIntegral (shiftL (msb .&. 0x7f) 7 .|. (lsb .&. 0x7f))
