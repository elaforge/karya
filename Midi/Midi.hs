{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Midi.Midi where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Generics as Generics
import qualified Perform.Timestamp as Timestamp
import Data.Word (Word8)

import qualified Util.Pretty as Pretty

import qualified Midi.CC as CC


-- | Declared abstract here so I can switch to a more compact representation
-- later.
type WriteMessages = [WriteMessage]
type ReadMessages = [ReadMessage]

data WriteMessage = WriteMessage {
    wmsg_dev :: !WriteDevice
    , wmsg_ts :: !Timestamp.Timestamp
    , wmsg_msg :: !Message
    } deriving (Eq, Ord, Show)
data ReadMessage = ReadMessage {
    rmsg_dev :: !ReadDevice
    , rmsg_ts :: !Timestamp.Timestamp
    , rmsg_msg :: !Message
    } deriving (Eq, Ord, Show)

-- Midi msgs are already strict so deepseq is unnecessary, but an NFData
-- instance will make deepseq work on things that contain msgs.
instance DeepSeq.NFData WriteMessage where
    rnf (WriteMessage _ _ _) = ()
instance DeepSeq.NFData ReadMessage where
    rnf (ReadMessage _ _ _) = ()

instance Pretty.Pretty ReadMessage where
    pretty (ReadMessage (ReadDevice dev) ts msg) =
        dev ++ ", " ++ Pretty.pretty ts ++ ": " ++ Pretty.pretty msg

-- * devices

-- | Implementation independent representation of a MIDI Device.
--
-- This can be saved to and loaded from files without regard for the devices
-- actually installed or opened.  When the Devices are opened, a mapping should
-- be established between Devices and the runtime representation.

newtype ReadDevice = ReadDevice String
    deriving (Eq, Ord, Show, Generics.Typeable)
newtype WriteDevice = WriteDevice String
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

un_read_device (ReadDevice dev) = dev
un_write_device (WriteDevice dev) = dev

add_timestamp :: Timestamp.Timestamp -> WriteMessage -> WriteMessage
add_timestamp ts wmsg = wmsg { wmsg_ts = wmsg_ts wmsg + ts }


-- * constructors

-- | Emit a program change with bank in [msb, lsb, pchange] order.
program_change :: Integer -> Integer -> [ChannelMessage]
program_change bank program =
    [ ControlChange CC.bank msb, ControlChange CC.bank_lsb lsb
    , ProgramChange (fromIntegral program)
    ]
    where (lsb, msb) = split14 (fromIntegral bank)

-- | Emit a pitch bend sensitivity RPN message for the given range.
pitch_bend_sensitivity :: Double -> [ChannelMessage]
pitch_bend_sensitivity range =
    [ ControlChange CC.rpn_msb 0, ControlChange CC.rpn_lsb 0
    , ControlChange CC.data_entry semitones
    ] ++ if cents <= 0 then [] else [ControlChange CC.data_entry_lsb cents]
    where
    (semitones, frac) = properFraction range
    cents = round (frac * 100)

-- * constants

-- | These aren't used here, but even though I'd like to constrain all midi
-- parsing to Midi.Parse, other places wind up dealing with raw sysex msgs.
sox_byte, eox_byte :: Word8
sox_byte = 0xf0
eox_byte = 0xf7

-- * modify

set_channel :: Channel -> Message -> Message
set_channel chan (ChannelMessage _ msg) = ChannelMessage chan msg
set_channel _ msg = msg

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
    PitchBend val -> 0 <= val && val < 2^14
    _ -> error $ "valid_chan_msg: unknown msg: " ++ show msg

is_cc (ChannelMessage _ (ControlChange _ _)) = True
is_cc _ = False

is_sysex (CommonMessage (SystemExclusive _ _)) = True
is_sysex _ = False

is_note (ChannelMessage _ (NoteOn _ _)) = True
is_note (ChannelMessage _ (NoteOff _ _)) = True
is_note _ = False

is_note_on (ChannelMessage _ (NoteOn _ _)) = True
is_note_on _ = False

channel_message :: Message -> Maybe ChannelMessage
channel_message (ChannelMessage _ m) = Just m
channel_message _ = Nothing

-- * types

data Message =
    ChannelMessage !Channel !ChannelMessage
    | CommonMessage !CommonMessage
    | RealtimeMessage !RealtimeMessage
    | UnknownMessage !Word8 !Word8 !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

instance DeepSeq.NFData Message where
    rnf _ = ()

instance Pretty.Pretty Message where
    pretty (CommonMessage (SystemExclusive manuf bytes)) =
        "CommonMessage (SystemExclusive " ++ show manuf
            ++ " <" ++ show (ByteString.length bytes) ++ " bytes>)"
    pretty msg = show msg

-- TODO using Word8 here is kind of iffy.  Word8s silently overflow after 0xff.
-- On the other hand, these all have 7 bit ranges, so I can still check for
-- out of range values, at least until it wraps.
type Channel = Word8 -- actually 4 bits
type Key = Word8
type Velocity = Word8
type Control = CC.Control
type Program = Word8
type ControlValue = Word8
-- | This is converted to and from the -0x2000 and +0x2000 range by the parser.
type PitchBendValue = Float
data ChannelMessage =
    NoteOff !Key !Velocity
    | NoteOn !Key !Velocity
    | Aftertouch !Key !ControlValue
    | ControlChange !Control !ControlValue
    | ProgramChange !Program
    | ChannelPressure !ControlValue
    | PitchBend !PitchBendValue
    -- | channel mode messages (special control values)
    | AllSoundOff
    | ResetAllCcontrols
    | LocalControl !Bool
    | AllNotesOff
    | UndefinedChannelMode !Word8 !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

data CommonMessage =
    -- | manufacturer id, data including eox
    SystemExclusive !Word8 !ByteString.ByteString
    | SongPositionPointer !Int
    | SongSelect !Word8
    | TuneRequest
    | EOX
    | UndefinedCommon !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

data RealtimeMessage = TimingClock | Start | Continue | Stop | ActiveSense
    | Reset | UndefinedRealtime !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

-- * util

-- | Split an Int into (lsb, msb)
split14 :: Int -> (Word8, Word8)
split14 i = (fromIntegral (i .&. 0x7f), fromIntegral (shiftR i 7 .&. 0x7f))

-- | Split an Int into two 7-bit Word8s, or go the other way.  MIDI sends
-- the lsb first, so the args will usually be swapped.
join14 :: Word8 -> Word8 -> Int
join14 lsb msb =
    shiftL (fromIntegral msb .&. 0x7f) 7 .|. (fromIntegral lsb .&. 0x7f)
