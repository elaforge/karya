-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    , program_change, reset_channel
    , realtime_sysex
    -- ** rpn / nrpn
    , pitch_bend_range, nrpn_tuning
    , rpn, nrpn

    -- * modify
    , set_channel

    -- * predicates
    , valid_msg, valid_chan_msg, is_cc, is_sysex, is_note, is_note_on, is_state
    , is_pitched
    , channel_message, message_channel

    -- * types
    , Message(..), Channel, Velocity, Control, Program, ControlValue
    , PitchBendValue, Manufacturer
    , Key(..), from_key, to_key, to_closest_key
    , ChannelMessage(..), CommonMessage(..), RealtimeMessage(..)
    -- * MTC
    , Mtc(..), FrameRate(..), SmpteFragment(..), Smpte(..)
    , seconds_to_frame, frame_to_seconds, frame_to_smpte, seconds_to_smpte
    , generate_mtc
    , mtc_sync, mtc_fragments
    -- * tuning
    , NoteNumber
    , realtime_tuning

    -- * util
    , join14, split14, join4, split4
    -- ** manufacturer
    , manufacturer_name
    , yamaha_code, korg_code
) where
import qualified Control.DeepSeq as DeepSeq
import           Control.DeepSeq (rnf)
import qualified Data.Bits as Bits
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Encoding
import           Data.Word (Word8)

import qualified Foreign.C

import qualified Util.CUtil as CUtil
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import           Util.Pretty (format, (<+>))
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Midi.CC as CC
import           Perform.RealTime (RealTime)

import           Global


-- | Declared abstract here so I can switch to a more compact representation
-- later.
type WriteMessages = [WriteMessage]
type ReadMessages = [ReadMessage]

data WriteMessage = WriteMessage {
    wmsg_dev :: !WriteDevice
    , wmsg_ts :: !RealTime
    , wmsg_msg :: !Message
    } deriving (Eq, Ord, Read, Show)
data ReadMessage = ReadMessage {
    rmsg_dev :: !ReadDevice
    , rmsg_ts :: !RealTime
    , rmsg_msg :: !Message
    } deriving (Eq, Ord, Read, Show)

-- Midi msgs are already strict so deepseq is unnecessary, but an NFData
-- instance will make deepseq work on things that contain msgs.
instance DeepSeq.NFData WriteMessage where rnf _ = ()
instance DeepSeq.NFData ReadMessage where rnf _ = ()

instance Pretty ReadMessage where
    pretty (ReadMessage dev ts msg) =
        pretty dev <> " " <> pretty ts <> ": " <> pretty msg
instance Pretty WriteMessage where
    pretty (WriteMessage dev ts msg) =
        pretty dev <> " " <> pretty ts <> ": " <> pretty msg

-- * devices

-- | Implementation independent representation of a MIDI Device.
--
-- This can be saved to and loaded from files without regard for the devices
-- actually installed or opened.
newtype ReadDevice = ReadDevice ByteString.ByteString
    deriving (Eq, Ord, Show, Read, Serialize.Serialize)
    -- Storing these as ByteString gives a cheap marshal and unmarshal.  Not
    -- that it matters, but maybe it will someday for a different MIDI backend.
newtype WriteDevice = WriteDevice ByteString.ByteString
    deriving (Eq, Ord, Show, Read, Serialize.Serialize)

read_device :: Text -> ReadDevice
read_device = ReadDevice . Encoding.encodeUtf8

write_device :: Text -> WriteDevice
write_device = WriteDevice . Encoding.encodeUtf8

read_device_text :: ReadDevice -> Text
read_device_text (ReadDevice bs) = CUtil.decodeUtf8 bs

write_device_text :: WriteDevice -> Text
write_device_text (WriteDevice bs) = CUtil.decodeUtf8 bs

peek_wdev :: Foreign.C.CString -> IO WriteDevice
peek_wdev = fmap WriteDevice . ByteString.packCString

peek_rdev :: Foreign.C.CString -> IO ReadDevice
peek_rdev = fmap ReadDevice . ByteString.packCString

with_wdev :: WriteDevice -> (Foreign.C.CString -> IO a) -> IO a
with_wdev (WriteDevice dev) = ByteString.useAsCString dev

with_rdev :: ReadDevice -> (Foreign.C.CString -> IO a) -> IO a
with_rdev (ReadDevice dev) = ByteString.useAsCString dev

instance Pretty ReadDevice where pretty = read_device_text
instance Pretty WriteDevice where pretty = write_device_text

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
    where (lsb, msb) = split14 bank

reset_channel :: Channel -> [Message]
reset_channel chan =
    -- There is also AllNotesOff, but AllSoundOff seems more widely supported.
    [ ChannelMessage chan AllSoundOff
    , ChannelMessage chan ResetAllControls
    ]

-- | This is a special kind of sysex which is meant to be interpreted in real
-- time.
realtime_sysex :: ByteString.ByteString -> Message
realtime_sysex = CommonMessage . SystemExclusive 0x7f

-- ** rpn / nrpn

-- | Emit a pitch bend range RPN message for the given range.
pitch_bend_range :: Double -> [ChannelMessage]
pitch_bend_range range = rpn (0, 0) (semitones, cents) ++ cancel_rpn
    where
    (semitones, frac) = properFraction range
    cents = round (frac * 100)

{- | This is an emulation of 'realtime_tuning' for Kontakt KSP, which
    understands NRPNs, but not sysex.

    Each key gets (50, 0) with the source key, (51, 0) with the destination,
    and (52, 0) with tenths of a cent as a 14-bit number.  I have to put
    source and destination NoteNumbers into separate NRPN numbers because
    msb and lsb arrive separately, so they pretty much have to be used as
    msb and lsb of a single number.

    The KSP for this is User/Elaforge/Instrument/Kontakt/ksp/nrpn_tuning.ksp.
-}
nrpn_tuning :: [(Key, NoteNumber)] -> [ChannelMessage]
nrpn_tuning = concatMap retune_key
    where
    retune_key (key, nn) = concat
        [ emit 50 (from_key key)
        , emit 51 to_key
        , nrpn (0, 52) (msb, lsb)
        ]
        where
        emit key value = nrpn (0, key) (value, 0)
        (lsb, msb) = split14 $ round $ frac * centicent
        -- 2^14 = 16384, so I can fit 10000.  I think KSP only does integer
        -- math, so I can't properly rescale 2^14 to its millicents.
        centicent = 100 * 100
        (to_key, frac) = properFraction nn

rpn :: (ControlValue, ControlValue) -> (ControlValue, ControlValue)
    -> [ChannelMessage]
rpn (key_msb, key_lsb) (value_msb, value_lsb) =
    [ ControlChange CC.rpn_msb key_msb
    , ControlChange CC.rpn_lsb key_lsb
    , ControlChange CC.data_entry value_msb
    , ControlChange CC.data_entry_lsb value_lsb
    ]

cancel_rpn :: [ChannelMessage]
cancel_rpn = [ControlChange CC.rpn_msb 127, ControlChange CC.rpn_lsb 127]

nrpn :: (ControlValue, ControlValue) -> (ControlValue, ControlValue)
    -> [ChannelMessage]
nrpn (key_msb, key_lsb) (value_msb, value_lsb) =
    [ ControlChange CC.nrpn_msb key_msb
    , ControlChange CC.nrpn_lsb key_lsb
    , ControlChange CC.data_entry value_msb
    , ControlChange CC.data_entry_lsb value_lsb
    ]

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

-- | True for messages with a pitch: NoteOn, NoteOff and PitchBend.
is_pitched :: Message -> Bool
is_pitched msg = is_note msg || case msg of
    ChannelMessage _ (PitchBend _) -> True
    _ -> False

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
    deriving (Eq, Ord, Show, Read)

instance Pretty Message where
    pretty (CommonMessage (SystemExclusive manuf bytes)) =
        "sysex " <> manufacturer_name manuf
            <> " <" <> showt (ByteString.length bytes) <> " bytes>"
    pretty (ChannelMessage chan msg) =
        "chan:" <> showt chan <> " " <> pretty msg
    pretty msg = showt msg

-- | These will be encoded with 7-bits, via the usual truncation.  It's not
-- exactly safe, but then it's not like haskell checks for overflow anywhere
-- else either.
type Word7 = Word8
type Word4 = Word8

type Channel = Word4
type Velocity = Word7
type Control = CC.Control
type Program = Word7
type ControlValue = Word7
-- | This is converted to and from the -0x2000 and +0x2000 range by the parser.
type PitchBendValue = Float
type Manufacturer = Word7

newtype Key = Key Int
    deriving (Eq, Ord, Num, Enum, Show, Read, DeepSeq.NFData)
    -- This was initially a Word7 to match MIDI's range, but unlike the other
    -- types, I sometimes do math on these, and Word7's tiny range is kind of
    -- scary for that.

instance Serialize.Serialize Key where
    -- The old encoding used Word8, so keep that for compatibility.
    put key = Serialize.put (from_key key :: Word8)
    get = (to_key :: Word8 -> Key) <$> Serialize.get

instance Pretty Key where
    pretty (Key key) = note <> showt (oct - 1) <> "(" <> showt key <> ")"
        where
        (oct, k) = key `divMod` 12
        note = case k of
            0 -> "c"; 1 -> "cs"
            2 -> "d"; 3 -> "ds"
            4 -> "e"
            5 -> "f"; 6 -> "fs"
            7 -> "g"; 8 -> "gs"
            9 -> "a"; 10 -> "as"
            11 -> "b"
            _ -> ""

from_key :: Num a => Key -> a
from_key (Key k) = fromIntegral k

to_key :: Integral a => a -> Key
to_key = Key . fromIntegral . min 127 . max 0

to_closest_key :: RealFrac a => a -> (Key, a)
to_closest_key nn
    | frac >= 0.5 = (to_key (d+1), frac - 1)
    | otherwise = (to_key d, frac)
    where (d, frac) = properFraction nn

data ChannelMessage =
    NoteOff !Key !Velocity
    | NoteOn !Key !Velocity
    | Aftertouch !Key !ControlValue
    | ControlChange !Control !ControlValue
    | ProgramChange !Program
    | ChannelPressure !ControlValue
    | PitchBend !PitchBendValue
    -- | This is PitchBend, but with precise control over the bytes sent.
    | PitchBendInt !Word7 !Word7
    -- | channel mode messages (special control values)
    | AllSoundOff
    | ResetAllControls
    | LocalControl !Bool
    | AllNotesOff
    -- | There are a few values in the ChannelMode range left undefined.
    | UndefinedChannelMode !Word7 !Word7
    deriving (Eq, Ord, Show, Read)

data CommonMessage =
    -- | manufacturer id, data including eox
    SystemExclusive !Manufacturer !ByteString.ByteString
    | SongPositionPointer !Int
    | SongSelect !Word8
    | TuneRequest
    | EOX
    | UndefinedCommon !Word8
    deriving (Eq, Ord, Show, Read)

data RealtimeMessage = MtcQuarterFrame !Mtc | TimingClock | Start | Continue
    | Stop | ActiveSense | Reset | UndefinedRealtime !Word8
    deriving (Eq, Ord, Show, Read)

instance DeepSeq.NFData Message where rnf _ = ()
instance DeepSeq.NFData ChannelMessage where rnf _ = ()
instance DeepSeq.NFData CommonMessage where rnf _ = ()
instance DeepSeq.NFData RealtimeMessage where rnf _ = ()

instance Pretty ChannelMessage where
    format msg = case msg of
        NoteOff key vel -> "NoteOff" <+> format key <+> format vel
        NoteOn key vel -> "NoteOn" <+> format key <+> format vel
        Aftertouch key vel -> "Aftertouch" <+> format key <+> format vel
        _ -> Pretty.text (showt msg)

-- * MTC

data Mtc = Mtc !SmpteFragment !Word4
    deriving (Eq, Ord, Show, Read)
data FrameRate = Frame24 | Frame25 | Frame29_97df | Frame30
    deriving (Enum, Show)
data SmpteFragment = FrameLsb | FrameMsb | SecondLsb | SecondMsb
    | MinuteLsb | MinuteMsb | HourLsb | RateHourMsb
    deriving (Enum, Eq, Ord, Show, Read)

rate_fps :: FrameRate -> Double
rate_fps Frame24 = 24
rate_fps Frame25 = 25
rate_fps Frame29_97df = 29.97
rate_fps Frame30 = 30

data Smpte = Smpte {
    smpte_hours :: !Word8
    , smpte_minutes :: !Word8
    , smpte_seconds :: !Word8
    , smpte_frames :: !Word8
    } deriving (Eq, Show)

type Seconds = Double
type Frames = Int

seconds_to_frame :: FrameRate -> Seconds -> Frames
seconds_to_frame rate = floor . (* rate_fps rate)

frame_to_seconds :: FrameRate -> Frames -> Seconds
frame_to_seconds rate = (/ rate_fps rate) . fromIntegral

frame_to_smpte :: FrameRate -> Frames -> Smpte
frame_to_smpte rate frame = Smpte (w7 hours) (w7 mins) (w7 secs) (w7 t3)
    where
    fps = case rate of
        Frame29_97df -> 30
        _ -> floor (rate_fps rate)
    (hours, t1) = undrop_frames rate frame `divMod` (60 * 60 * fps)
    (mins, t2) = t1 `divMod` (60 * fps)
    (secs, t3) = t2 `divMod` fps
    w7 :: Int -> Word7
    w7 = fromIntegral . Num.clamp 0 0x7f

seconds_to_smpte :: FrameRate -> Seconds -> Smpte
seconds_to_smpte rate = frame_to_smpte rate . seconds_to_frame rate

-- | Add dropped frames back in.  When converted back into SMPTE the effect is
-- that the dropped frames are skipped.
undrop_frames :: FrameRate -> Frames -> Frames
undrop_frames Frame29_97df frames =
    frames + 2 * (frames `div` (30 * 60)) - 2 * (frames `div` (10 * 30 * 60))
undrop_frames _ frames = frames

-- | Send full MTC sync code.  This is supposed to happen every time there is
-- a time dicontinuity.
mtc_sync :: FrameRate -> Smpte -> Message
mtc_sync rate (Smpte hours mins secs frames) =
    realtime_sysex $ ByteString.pack
        [chan, 01, 01, rate_code .|. hours, mins, secs, frames]
    where
    chan = 0x7f -- send to all devices
    rate_code = Bits.shiftL (fromIntegral (fromEnum rate)) 5
    -- TODO is chan the same as MMC DeviceId?

-- | Generate MTC starting at the given time and going on until the well of
-- time runs dry.  Or 7 bits overflow.
--
-- Since MTC can only start on a frame, the first returned time might be
-- slightly before the requested time.
--
-- One MtcQuarterFrame is transmitted per quarter frame.  Since it takes 8
-- to make a complete SMPTE frame, you wind up getting every other frame.
generate_mtc :: FrameRate -> Frames -> [(Double, Message)]
generate_mtc rate frame = zip times (concatMap msgs (Seq.range_ frame 2))
    where
    -- Round up to the previous whole frame, then step forward frames and time
    -- together.  frame_to_smpte will take care of drop frame.
    msgs frame = map (RealtimeMessage . MtcQuarterFrame) $
        mtc_fragments rate (frame_to_smpte rate frame)
    times = Seq.range_ start fragment
    start = fromIntegral frame / rate_fps rate
    fragment = 1 / rate_fps rate / 4

mtc_fragments :: FrameRate -> Smpte -> [Mtc]
mtc_fragments rate (Smpte hours minutes seconds frames) = map (uncurry Mtc)
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
    rate_code = Bits.shiftL (fromIntegral (fromEnum rate)) 1

-- * tuning

type NoteNumber = Double

-- | Create a realtime tuning msg.  Few synthesizers support this.
--
-- Based on <http://www.midi.org/techspecs/midituning.php>
realtime_tuning :: [(Key, NoteNumber)] -> Message
realtime_tuning nns = realtime_sysex $ ByteString.pack $
    [generic_device, 8, 2, 0, fromIntegral (length nns)]
        ++ concatMap (uncurry retune_key) nns
    where
    retune_key :: Key -> NoteNumber -> [Word7]
    retune_key key nn = [from_key key, nn_key, msb, lsb]
        where
        (lsb, msb) = split14 $ round $ frac * 2^14
        (nn_key, frac) = properFraction nn

-- * util

-- | Split an Int into two 7 bit words.
split14 :: Int -> (Word7, Word7) -- ^ (LSB, MSB)
split14 i = (fromIntegral (i .&. 0x7f), fromIntegral (Bits.shiftR i 7 .&. 0x7f))

-- | Join (LSB, MSB) 7-bit words into an int.
join14 :: Word7 -> Word7 -> Int
join14 lsb msb =
    Bits.shiftL (fromIntegral msb .&. 0x7f) 7 .|. (fromIntegral lsb .&. 0x7f)

-- | Split a Word8 into nibbles.
split4 :: Word8 -> (Word4, Word4) -- ^ (MSB, LSB)
split4 word = (Bits.shiftR word 4 .&. 0xf, word .&. 0xf)

-- | Join (MSB, LSB) into a Word7.
join4 :: Word4 -> Word4 -> Word7
join4 d1 d2 = (Bits.shiftL d1 4 .&. 0xf0) .|. (d2 .&. 0x0f)

-- * constants

-- | Softsynths don't care about device ID, so use this.
generic_device :: Word7
generic_device = 0x7f

manufacturer_name :: Manufacturer -> Text
manufacturer_name code = Maybe.fromMaybe (showt code) $
    Map.lookup code manufacturer_codes

korg_code, yamaha_code :: Manufacturer
korg_code = 0x42
yamaha_code = 0x43

-- | TODO get a more complete list
manufacturer_codes :: Map Manufacturer Text
manufacturer_codes = Map.fromList
    [(korg_code, "korg"), (yamaha_code, "yamaha")]
