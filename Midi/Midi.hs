-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Midi.Midi (
    WriteMessages, ReadMessages
    , WriteMessage(..), ReadMessage(..)

    -- * devices
    , ReadDevice, WriteDevice, read_device, write_device
    , read_device_text, write_device_text
    , peek_wdev, peek_rdev, with_wdev, with_rdev
    , add_timestamp, modify_timestamp

    -- * constructors
    , program_change, pitch_bend_sensitivity, reset_channel

    -- * constants
    , sox_byte, eox_byte

    -- * modify
    , set_channel

    -- * predicates
    , valid_msg, valid_chan_msg, is_cc, is_sysex, is_note, is_note_on, is_state
    , channel_message, message_channel

    -- * types
    , Message(..), Channel, Velocity, Control, Program, ControlValue
    , PitchBendValue, Manufacturer
    , Key(..), from_key, to_key
    , ChannelMessage(..), CommonMessage(..), RealtimeMessage(..)
    , Timing(..), TimingRate(..), SmpteFragment(..), Smpte(..)
    , mtc_sync, mtc_fragments

    -- * util
    , join14, split14, join4, split4
    -- ** manufacturer
    , manufacturer_name
    , yamaha_code, korg_code
) where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Generics as Generics
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Encoding
import Data.Word (Word8)

import qualified Foreign.C
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Pretty (pretty, format, (<+>))
import qualified Util.Serialize as Serialize

import qualified Midi.CC as CC
import qualified Ui.Util
import Perform.RealTime (RealTime)


-- | Declared abstract here so I can switch to a more compact representation
-- later.
type WriteMessages = [WriteMessage]
type ReadMessages = [ReadMessage]

data WriteMessage = WriteMessage {
    wmsg_dev :: !WriteDevice
    , wmsg_ts :: !RealTime
    , wmsg_msg :: !Message
    } deriving (Eq, Ord, Show)
data ReadMessage = ReadMessage {
    rmsg_dev :: !ReadDevice
    , rmsg_ts :: !RealTime
    , rmsg_msg :: !Message
    } deriving (Eq, Ord, Show)

-- Midi msgs are already strict so deepseq is unnecessary, but an NFData
-- instance will make deepseq work on things that contain msgs.
instance DeepSeq.NFData WriteMessage where
    rnf (WriteMessage _ _ _) = ()
instance DeepSeq.NFData ReadMessage where
    rnf (ReadMessage _ _ _) = ()

instance Pretty.Pretty ReadMessage where
    pretty (ReadMessage dev ts msg) = Printf.printf "%s %s: %s"
        (pretty dev) (pretty ts) (pretty msg)
instance Pretty.Pretty WriteMessage where
    pretty (WriteMessage dev ts msg) = Printf.printf "%s %s: %s"
        (pretty dev) (pretty ts) (pretty msg)

-- * devices

-- | Implementation independent representation of a MIDI Device.
--
-- This can be saved to and loaded from files without regard for the devices
-- actually installed or opened.
newtype ReadDevice = ReadDevice ByteString.ByteString
    deriving (Eq, Ord, Show, Read, Generics.Typeable, Serialize.Serialize)
    -- Storing these as ByteString gives a cheap marshal and unmarshal.  Not
    -- that it matters, but maybe it will someday for a different MIDI backend.
newtype WriteDevice = WriteDevice ByteString.ByteString
    deriving (Eq, Ord, Show, Read, Generics.Typeable, Serialize.Serialize)

read_device :: Text -> ReadDevice
read_device = ReadDevice . Encoding.encodeUtf8

write_device :: Text -> WriteDevice
write_device = WriteDevice . Encoding.encodeUtf8

read_device_text :: ReadDevice -> Text
read_device_text (ReadDevice bs) = Ui.Util.decodeUtf8 bs

write_device_text :: WriteDevice -> Text
write_device_text (WriteDevice bs) = Ui.Util.decodeUtf8 bs

peek_wdev :: Foreign.C.CString -> IO WriteDevice
peek_wdev = fmap WriteDevice . ByteString.packCString

peek_rdev :: Foreign.C.CString -> IO ReadDevice
peek_rdev = fmap ReadDevice . ByteString.packCString

with_wdev :: WriteDevice -> (Foreign.C.CString -> IO a) -> IO a
with_wdev (WriteDevice dev) = ByteString.useAsCString dev

with_rdev :: ReadDevice -> (Foreign.C.CString -> IO a) -> IO a
with_rdev (ReadDevice dev) = ByteString.useAsCString dev

instance Pretty.Pretty ReadDevice where pretty = untxt . read_device_text
instance Pretty.Pretty WriteDevice where pretty = untxt . write_device_text

add_timestamp :: RealTime -> WriteMessage -> WriteMessage
add_timestamp ts wmsg = wmsg { wmsg_ts = wmsg_ts wmsg + ts }

modify_timestamp :: (RealTime -> RealTime) -> WriteMessage -> WriteMessage
modify_timestamp f wmsg = wmsg { wmsg_ts = f (wmsg_ts wmsg) }


-- * constructors

-- | Emit a program change with bank in [msb, lsb, pchange] order.
program_change :: Int -> Program -> [ChannelMessage]
program_change bank program =
    [ ControlChange CC.bank msb, ControlChange CC.bank_lsb lsb
    , ProgramChange program
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

reset_channel :: Channel -> [Message]
reset_channel chan =
    -- There is also AllNotesOff, but AllSoundOff seems more widely supported.
    [ ChannelMessage chan AllSoundOff
    , ChannelMessage chan ResetAllControls
    ]

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

valid_chan_msg :: ChannelMessage -> Bool
valid_chan_msg msg = case msg of
    ControlChange cc val -> val7 cc && val7 val
    NoteOn (Key key) vel -> val7 key && val7 vel
    NoteOff (Key key) vel -> val7 key && val7 vel
    PitchBend val -> 0 <= val && val < 2^14
    _ -> error $ "valid_chan_msg: unknown msg: " ++ show msg
    where val7 v = 0 <= v && v < 128

is_cc :: Message -> Bool
is_cc (ChannelMessage _ (ControlChange _ _)) = True
is_cc _ = False

is_sysex :: Message -> Bool
is_sysex (CommonMessage (SystemExclusive _ _)) = True
is_sysex _ = False

is_note :: Message -> Bool
is_note (ChannelMessage _ (NoteOn _ _)) = True
is_note (ChannelMessage _ (NoteOff _ _)) = True
is_note _ = False

is_note_on :: Message -> Bool
is_note_on (ChannelMessage _ (NoteOn _ _)) = True
is_note_on _ = False

-- | Is this a message that will change the channel state?  These are the
-- messages that will affect subsequent NoteOns.
is_state :: Message -> Bool
is_state (ChannelMessage _ msg) = case msg of
    NoteOn {} -> False
    NoteOff {} -> False
    _ -> True
is_state _ = False

-- * projections

channel_message :: Message -> Maybe ChannelMessage
channel_message (ChannelMessage _ m) = Just m
channel_message _ = Nothing

message_channel :: Message -> Maybe Channel
message_channel (ChannelMessage chan _) = Just chan
message_channel _ = Nothing

-- * types

data Message =
    ChannelMessage !Channel !ChannelMessage
    | CommonMessage !CommonMessage
    | RealtimeMessage !RealtimeMessage
    | UnknownMessage !Word8 !Word8 !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

instance Pretty.Pretty Message where
    pretty (CommonMessage (SystemExclusive manuf bytes)) =
        Printf.printf "sysex %s <%d bytes>" (manufacturer_name manuf)
            (ByteString.length bytes)
    pretty (ChannelMessage chan msg) =
        Printf.printf "chan:%d %s" chan (pretty msg)
    pretty msg = show msg

-- TODO using Word8 here is kind of iffy.  Word8s silently overflow after 0xff.
-- On the other hand, these all have 7 bit ranges, so I can still check for
-- out of range values, at least until it wraps.
type Channel = Word8 -- actually 4 bits
type Velocity = Word8
type Control = CC.Control
type Program = Word8
type ControlValue = Word8
-- | This is converted to and from the -0x2000 and +0x2000 range by the parser.
type PitchBendValue = Float
type Manufacturer = Word8

newtype Key = Key Word8 deriving (Eq, Ord, Read, Show, Num, Enum)

instance Pretty.Pretty Key where
    pretty (Key key) = note ++ show (oct - 1)
        where
        (oct, k) = (fromIntegral key :: Int) `divMod` 12
        note = case k of
            0 -> "c"; 1 -> "cs"
            2 -> "d"; 3 -> "ds"
            4 -> "e"
            5 -> "f"; 6 -> "fs"
            7 -> "g"; 8 -> "gs"
            9 -> "a"; 10 -> "as"
            11 -> "b"
            _ -> ""

from_key :: (Num a) => Key -> a
from_key (Key k) = fromIntegral k

to_key :: (Integral a) => a -> Key
to_key = Key . fromIntegral . min 127 . max 0

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
    | ResetAllControls
    | LocalControl !Bool
    | AllNotesOff
    | UndefinedChannelMode !Word8 !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

data CommonMessage =
    -- | manufacturer id, data including eox
    SystemExclusive !Manufacturer !ByteString.ByteString
    | SongPositionPointer !Int
    | SongSelect !Word8
    | TuneRequest
    | EOX
    | UndefinedCommon !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

data RealtimeMessage = TimingClock !Timing | Start | Continue | Stop
    | ActiveSense | Reset | UndefinedRealtime !Word8
    deriving (Eq, Ord, Show, Read, Generics.Typeable)

instance DeepSeq.NFData Message where rnf _ = ()
instance DeepSeq.NFData ChannelMessage where rnf _ = ()
instance DeepSeq.NFData CommonMessage where rnf _ = ()
instance DeepSeq.NFData RealtimeMessage where rnf _ = ()

instance Pretty.Pretty ChannelMessage where
    format msg = case msg of
        NoteOff key vel -> "NoteOff" <+> format key <+> format vel
        NoteOn key vel -> "NoteOn" <+> format key <+> format vel
        Aftertouch key vel -> "Aftertouch" <+> format key <+> format vel
        _ -> Pretty.text (show msg)

-- ** MTC

data Timing = Timing !SmpteFragment !Word8 -- actually Word4
    deriving (Eq, Ord, Show, Read)
data TimingRate = Frame24 | Frame25 | Frame29 | Frame30
    deriving (Enum, Show)
data SmpteFragment = FrameLsb | FrameMsb | SecondLsb | SecondMsb
    | MinuteLsb | MinuteMsb | HourLsb | RateHourMsb
    deriving (Enum, Eq, Ord, Show, Read)

data Smpte = Smpte {
    hours :: !Word8
    , minutes :: !Word8
    , seconds :: !Word8
    , frames :: !Word8
    , subframes :: !Word8
    } deriving (Eq, Show)

-- | Sent full MTC sync code.
mtc_sync :: Smpte -> Message
mtc_sync (Smpte hours mins secs frames _ ) =
    CommonMessage $ SystemExclusive 0x7f $
        ByteString.pack [chan, 01, 01, hours, mins, secs, frames, eox_byte]
    where
    chan = 0x7f -- send to all devices

-- | These should be transmitted once each quarter frame, so 96--120 times per
-- second.
mtc_fragments :: TimingRate -> Smpte -> [Timing]
mtc_fragments rate (Smpte hours minutes seconds frames _) = map (uncurry Timing)
    [ (FrameLsb, frame_lsb), (FrameMsb, frame_msb)
    , (SecondLsb, sec_lsb), (SecondMsb, sec_msb)
    , (MinuteLsb, min_lsb), (MinuteMsb, min_msb)
    , (HourLsb, hour_lsb), (RateHourMsb, rate_code .|. hour_msb)
    ]
    where
    (frame_msb, frame_lsb) = split4 frames
    (sec_msb, sec_lsb) = split4 seconds
    (min_msb, min_lsb) = split4 minutes
    (hour_msb, hour_lsb) = split4 hours
    rate_code = shiftL (fromIntegral (fromEnum rate)) 1

-- * util

-- | Split an Int into two 7 bit words.
split14 :: Int -> (Word8, Word8) -- ^ (LSB, MSB)
split14 i = (fromIntegral (i .&. 0x7f), fromIntegral (shiftR i 7 .&. 0x7f))

-- | Join (LSB, MSB) 7-bit words into an int.
join14 :: Word8 -> Word8 -> Int
join14 lsb msb =
    shiftL (fromIntegral msb .&. 0x7f) 7 .|. (fromIntegral lsb .&. 0x7f)

-- | Split a Word8 into (msb, lsb) nibbles, and join back.
split4 :: Word8 -> (Word8, Word8)
split4 word = (shiftR word 4 .&. 0xf, word .&. 0xf)

-- | Join msb and lsb into a Word8.
join4 :: Word8 -> Word8 -> Word8
join4 d1 d2 = (shiftL d1 4 .&. 0xf0) .|. (d2 .&. 0x0f)

-- ** manufacturer

manufacturer_name :: Manufacturer -> String
manufacturer_name code = Maybe.fromMaybe (show code) $
    Map.lookup code manufacturer_codes

korg_code, yamaha_code :: Manufacturer
korg_code = 0x42
yamaha_code = 0x43

-- | TODO get a more complete list
manufacturer_codes :: Map.Map Manufacturer String
manufacturer_codes = Map.fromList
    [(korg_code, "korg"), (yamaha_code, "yamaha")]
