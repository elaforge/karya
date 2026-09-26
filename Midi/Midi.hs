-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -funbox-strict-fields #-}
module Midi.Midi (
    WriteMessages, ReadMessages
    , WriteMessage(..), ReadMessage(..)

    -- * devices
    , ReadDevice, WriteDevice, readDevice, writeDevice
    , readDeviceText, writeDeviceText
    , peekWdev, peekRdev, withWdev, withRdev
    , addTimestamp, modifyTimestamp

    -- * constructors
    , programChange, resetChannel
    , realtimeSysex
    -- ** rpn / nrpn
    , pitchBendRange, nrpnTuning
    , MpeZone(..), mpe2To16
    , rpn, nrpn

    -- * modify
    , setChannel

    -- * predicates
    , validMsg, validChanMsg, isCc, isSysex, isNote, isNoteOn, isState
    , isPitched
    , channelMessage, messageChannel

    -- * types
    , Message(..), Channel, Velocity, Control, Program, ControlValue
    , PitchBendValue, Manufacturer
    , Key(..), fromKey, toKey, toClosestKey
    , ChannelMessage(..), CommonMessage(..), RealtimeMessage(..)
    -- * MTC
    , Mtc(..), FrameRate(..), SmpteFragment(..), Smpte(..)
    , secondsToFrame, frameToSeconds, frameToSmpte, secondsToSmpte
    , generateMtc
    , mtcSync, mtcFragments
    -- * tuning
    , NoteNumber
    , realtimeTuning

    -- * util
    , join14, split14, join4, split4
    -- ** manufacturer
    , manufacturerName
    , yamahaCode, korgCode
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

import qualified Util.FFI as FFI
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import           Util.Pretty (format, (<+>))
import qualified Util.Lists as Lists
import qualified Util.Serialize as Serialize

import qualified Midi.CC as CC
import           Perform.RealTime (RealTime)

import           Global


-- | Declared abstract here so I can switch to a more compact representation
-- later.
type WriteMessages = [WriteMessage]
type ReadMessages = [ReadMessage]

data WriteMessage = WriteMessage
    { wmsgDev :: !WriteDevice
    , wmsgTs :: !RealTime
    , wmsgMsg :: !Message
    } deriving (Eq, Ord, Read, Show)
data ReadMessage = ReadMessage
    { rmsgDev :: !ReadDevice
    , rmsgTs :: !RealTime
    , rmsgMsg :: !Message
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

readDevice :: Text -> ReadDevice
readDevice = ReadDevice . Encoding.encodeUtf8

writeDevice :: Text -> WriteDevice
writeDevice = WriteDevice . Encoding.encodeUtf8

readDeviceText :: ReadDevice -> Text
readDeviceText (ReadDevice bs) = FFI.decodeUtf8 bs

writeDeviceText :: WriteDevice -> Text
writeDeviceText (WriteDevice bs) = FFI.decodeUtf8 bs

peekWdev :: Foreign.C.CString -> IO WriteDevice
peekWdev = fmap WriteDevice . ByteString.packCString

peekRdev :: Foreign.C.CString -> IO ReadDevice
peekRdev = fmap ReadDevice . ByteString.packCString

withWdev :: WriteDevice -> (Foreign.C.CString -> IO a) -> IO a
withWdev (WriteDevice dev) = ByteString.useAsCString dev

withRdev :: ReadDevice -> (Foreign.C.CString -> IO a) -> IO a
withRdev (ReadDevice dev) = ByteString.useAsCString dev

instance Pretty ReadDevice where pretty = readDeviceText
instance Pretty WriteDevice where pretty = writeDeviceText

addTimestamp :: RealTime -> WriteMessage -> WriteMessage
addTimestamp ts wmsg = wmsg { wmsgTs = wmsgTs wmsg + ts }

modifyTimestamp :: (RealTime -> RealTime) -> WriteMessage -> WriteMessage
modifyTimestamp f wmsg = wmsg { wmsgTs = f (wmsgTs wmsg) }


-- * constructors

-- | Emit a program change with bank in [msb, lsb, pchange] order.
programChange :: Int -> Program -> [ChannelMessage]
programChange bank program =
    [ ControlChange CC.bank msb, ControlChange CC.bankLsb lsb
    , ProgramChange program
    ]
    where (lsb, msb) = split14 bank

resetChannel :: Channel -> [Message]
resetChannel chan =
    -- There is also AllNotesOff, but AllSoundOff seems more widely supported.
    [ ChannelMessage chan AllSoundOff
    , ChannelMessage chan ResetAllControls
    ]

-- | This is a special kind of sysex which is meant to be interpreted in real
-- time.
realtimeSysex :: ByteString.ByteString -> Message
realtimeSysex = CommonMessage . SystemExclusive 0x7f

-- ** rpn / nrpn

-- | Emit a pitch bend range RPN message for the given range.
pitchBendRange :: Double -> [ChannelMessage]
pitchBendRange range = rpn (0, 0) (semitones, cents) ++ cancelRpn
    where
    (semitones, frac) = properFraction range
    cents = round (frac * 100)

data MpeZone = Lower | Upper deriving (Eq, Show)

-- | MPE config where channel 0 is the useless master and 1-15 are used for
-- notes.  This is the one the MPE doc recommends MPE synths start up in.
mpe2To16 :: [Message]
mpe2To16 = mpeConfigure Lower (Just 15)

-- | MPE basically means "midi channels actually work."  Except we lose a
-- channel to be the master channel, where a channel message still affects all
-- the other channels too.  The MIDI backend doesn't support a separate master
-- channel, but it seems pretty useless because if I want to affect
-- all notes, I will send messages for all notes.
--
-- Still, MPE support is a good thing because at least 15 channels work
-- properly, and hopefully 'pitch_bend_range' is supported, though the default
-- of (-48, +48) is acceptable.
--
-- The other MPE innovation is that it might support cc 74, to do something
-- expressive.
mpeConfigure :: MpeZone -> Maybe Channel -> [Message]
mpeConfigure zone channels =
    map (ChannelMessage chan) (rpn (0, 6) (fromMaybe 0 channels, 0))
    where
    chan = case zone of
        Lower -> 0
        Upper -> 15

{- | This is an emulation of 'realtime_tuning' for Kontakt KSP, which
    understands NRPNs, but not sysex.

    Each key gets (50, 0) with the source key, (51, 0) with the destination,
    and (52, 0) with tenths of a cent as a 14-bit number.  I have to put
    source and destination NoteNumbers into separate NRPN numbers because
    msb and lsb arrive separately, so they pretty much have to be used as
    msb and lsb of a single number.

    The KSP for this is User/Elaforge/Instrument/Kontakt/ksp/nrpn_tuning.ksp.
-}
nrpnTuning :: [(Key, NoteNumber)] -> [ChannelMessage]
nrpnTuning = concatMap retuneKey
    where
    retuneKey (key, nn) = concat
        [ emit 50 (fromKey key)
        , emit 51 toKey
        , nrpn (0, 52) (msb, lsb)
        ]
        where
        emit key value = nrpn (0, key) (value, 0)
        (lsb, msb) = split14 $ round $ frac * centicent
        -- 2^14 = 16384, so I can fit 10000.  I think KSP only does integer
        -- math, so I can't properly rescale 2^14 to its millicents.
        centicent = 100 * 100
        (toKey, frac) = properFraction nn

rpn :: (ControlValue, ControlValue) -> (ControlValue, ControlValue)
    -> [ChannelMessage]
rpn (keyMsb, keyLsb) (valueMsb, valueLsb) =
    [ ControlChange CC.rpnMsb keyMsb
    , ControlChange CC.rpnLsb keyLsb
    , ControlChange CC.dataEntry valueMsb
    , ControlChange CC.dataEntryLsb valueLsb
    ]

cancelRpn :: [ChannelMessage]
cancelRpn = [ControlChange CC.rpnMsb 127, ControlChange CC.rpnLsb 127]

nrpn :: (ControlValue, ControlValue) -> (ControlValue, ControlValue)
    -> [ChannelMessage]
nrpn (keyMsb, keyLsb) (valueMsb, valueLsb) =
    [ ControlChange CC.nrpnMsb keyMsb
    , ControlChange CC.nrpnLsb keyLsb
    , ControlChange CC.dataEntry valueMsb
    , ControlChange CC.dataEntryLsb valueLsb
    ]

-- * modify

setChannel :: Channel -> Message -> Message
setChannel chan (ChannelMessage _ msg) = ChannelMessage chan msg
setChannel _ msg = msg

-- * predicates

-- | Check to make sure midi msg vals are all in range.
validMsg :: Message -> Bool
validMsg (ChannelMessage chan msg) =
    0 <= chan && chan < 16 && validChanMsg msg
validMsg _ = True

validChanMsg :: ChannelMessage -> Bool
validChanMsg msg = case msg of
    ControlChange cc val -> val7 cc && val7 val
    NoteOn (Key key) vel -> val7 key && val7 vel
    NoteOff (Key key) vel -> val7 key && val7 vel
    PitchBend val -> -1 <= val && val <= 1
    _ -> True
    where val7 v = 0 <= v && v < 128

isCc :: Message -> Bool
isCc (ChannelMessage _ (ControlChange _ _)) = True
isCc _ = False

isSysex :: Message -> Bool
isSysex (CommonMessage (SystemExclusive _ _)) = True
isSysex _ = False

isNote :: Message -> Bool
isNote (ChannelMessage _ (NoteOn _ _)) = True
isNote (ChannelMessage _ (NoteOff _ _)) = True
isNote _ = False

isNoteOn :: Message -> Bool
isNoteOn (ChannelMessage _ (NoteOn _ _)) = True
isNoteOn _ = False

-- | Is this a message that will change the channel state?  These are the
-- messages that will affect subsequent NoteOns.
isState :: Message -> Bool
isState (ChannelMessage _ msg) = case msg of
    NoteOn {} -> False
    NoteOff {} -> False
    _ -> True
isState _ = False

-- | True for messages with a pitch: NoteOn, NoteOff and PitchBend.
isPitched :: Message -> Bool
isPitched msg = isNote msg || case msg of
    ChannelMessage _ (PitchBend _) -> True
    _ -> False

-- * projections

channelMessage :: Message -> Maybe ChannelMessage
channelMessage (ChannelMessage _ m) = Just m
channelMessage _ = Nothing

messageChannel :: Message -> Maybe Channel
messageChannel (ChannelMessage chan _) = Just chan
messageChannel _ = Nothing

-- * types

data Message
    = ChannelMessage !Channel !ChannelMessage
    | CommonMessage !CommonMessage
    | RealtimeMessage !RealtimeMessage
    | UnknownMessage !Word8 !Word8 !Word8
    deriving (Eq, Ord, Show, Read)

instance Pretty Message where
    pretty (CommonMessage (SystemExclusive manuf bytes)) =
        "sysex " <> manufacturerName manuf
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
    put key = Serialize.put (fromKey key :: Word8)
    get = (toKey :: Word8 -> Key) <$> Serialize.get

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

fromKey :: Num a => Key -> a
fromKey (Key k) = fromIntegral k

toKey :: Integral a => a -> Key
toKey = Key . fromIntegral . min 127 . max 0

toClosestKey :: RealFrac a => a -> (Key, a)
toClosestKey nn
    | frac >= 0.5 = (toKey (d+1), frac - 1)
    | otherwise = (toKey d, frac)
    where (d, frac) = properFraction nn

data ChannelMessage
    = NoteOff !Key !Velocity
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

data CommonMessage
    -- | manufacturer id, data including eox
    = SystemExclusive !Manufacturer !ByteString.ByteString
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

rateFps :: FrameRate -> Double
rateFps Frame24 = 24
rateFps Frame25 = 25
rateFps Frame29_97df = 29.97
rateFps Frame30 = 30

data Smpte = Smpte
    { smpteHours :: !Word8
    , smpteMinutes :: !Word8
    , smpteSeconds :: !Word8
    , smpteFrames :: !Word8
    } deriving (Eq, Show)

type Seconds = Double
type Frames = Int

secondsToFrame :: FrameRate -> Seconds -> Frames
secondsToFrame rate = floor . (* rateFps rate)

frameToSeconds :: FrameRate -> Frames -> Seconds
frameToSeconds rate = (/ rateFps rate) . fromIntegral

frameToSmpte :: FrameRate -> Frames -> Smpte
frameToSmpte rate frame = Smpte (w7 hours) (w7 mins) (w7 secs) (w7 t3)
    where
    fps = case rate of
        Frame29_97df -> 30
        _ -> floor (rateFps rate)
    (hours, t1) = undropFrames rate frame `divMod` (60 * 60 * fps)
    (mins, t2) = t1 `divMod` (60 * fps)
    (secs, t3) = t2 `divMod` fps
    w7 :: Int -> Word7
    w7 = fromIntegral . Num.clamp 0 0x7f

secondsToSmpte :: FrameRate -> Seconds -> Smpte
secondsToSmpte rate = frameToSmpte rate . secondsToFrame rate

-- | Add dropped frames back in.  When converted back into SMPTE the effect is
-- that the dropped frames are skipped.
undropFrames :: FrameRate -> Frames -> Frames
undropFrames Frame29_97df frames =
    frames + 2 * (frames `div` (30 * 60)) - 2 * (frames `div` (10 * 30 * 60))
undropFrames _ frames = frames

-- | Send full MTC sync code.  This is supposed to happen every time there is
-- a time dicontinuity.
mtcSync :: FrameRate -> Smpte -> Message
mtcSync rate (Smpte hours mins secs frames) =
    realtimeSysex $ ByteString.pack
        [chan, 01, 01, rateCode .|. hours, mins, secs, frames]
    where
    chan = 0x7f -- send to all devices
    rateCode = Bits.shiftL (fromIntegral (fromEnum rate)) 5
    -- TODO is chan the same as MMC DeviceId?

-- | Generate MTC starting at the given time and going on until the well of
-- time runs dry.  Or 7 bits overflow.
--
-- Since MTC can only start on a frame, the first returned time might be
-- slightly before the requested time.
--
-- One MtcQuarterFrame is transmitted per quarter frame.  Since it takes 8
-- to make a complete SMPTE frame, you wind up getting every other frame.
generateMtc :: FrameRate -> Frames -> [(Double, Message)]
generateMtc rate frame = zip times (concatMap msgs (Lists.range_ frame 2))
    where
    -- Round up to the previous whole frame, then step forward frames and time
    -- together.  frame_to_smpte will take care of drop frame.
    msgs frame = map (RealtimeMessage . MtcQuarterFrame) $
        mtcFragments rate (frameToSmpte rate frame)
    times = Lists.range_ start fragment
    start = fromIntegral frame / rateFps rate
    fragment = 1 / rateFps rate / 4

mtcFragments :: FrameRate -> Smpte -> [Mtc]
mtcFragments rate (Smpte hours minutes seconds frames) = map (uncurry Mtc)
    [ (FrameLsb, frameLsb), (FrameMsb, frameMsb)
    , (SecondLsb, secLsb), (SecondMsb, secMsb)
    , (MinuteLsb, minLsb), (MinuteMsb, minMsb)
    , (HourLsb, hourLsb), (RateHourMsb, rateCode .|. hourMsb)
    ]
    where
    (frameMsb, frameLsb) = split4 frames
    (secMsb, secLsb) = split4 seconds
    (minMsb, minLsb) = split4 minutes
    (hourMsb, hourLsb) = split4 hours
    rateCode = Bits.shiftL (fromIntegral (fromEnum rate)) 1

-- * tuning

type NoteNumber = Double

-- | Create a realtime tuning msg.  Few synthesizers support this.
--
-- Based on <http://www.midi.org/techspecs/midituning.php>
realtimeTuning :: [(Key, NoteNumber)] -> Message
realtimeTuning nns = realtimeSysex $ ByteString.pack $
    [genericDevice, 8, 2, 0, fromIntegral (length nns)]
        ++ concatMap (uncurry retuneKey) nns
    where
    retuneKey :: Key -> NoteNumber -> [Word7]
    retuneKey key nn = [fromKey key, nnKey, msb, lsb]
        where
        (lsb, msb) = split14 $ round $ frac * 2^14
        (nnKey, frac) = properFraction nn

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
genericDevice :: Word7
genericDevice = 0x7f

manufacturerName :: Manufacturer -> Text
manufacturerName code = Maybe.fromMaybe (showt code) $
    Map.lookup code manufacturerCodes

korgCode, yamahaCode :: Manufacturer
korgCode = 0x42
yamahaCode = 0x43

-- | TODO get a more complete list
manufacturerCodes :: Map Manufacturer Text
manufacturerCodes = Map.fromList
    [(korgCode, "korg"), (yamahaCode, "yamaha")]
