module Midi.Parse (decode, encode) where
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as Unsafe
import Data.Word (Word8)

import Midi.Midi


decode :: B.ByteString -> Message
decode bytes = case B.length bytes of
        0 -> UnknownMessage 0 0 0
        1 -> decode1 b0
        2 -> UnknownMessage b0 0 0
        _ -> decode3 bytes
    where
    b0 = Unsafe.unsafeIndex bytes 0

decode1 :: Word8 -> Message
decode1 status
    | st == 0xf = case code of
        0x6 -> CommonMessage TuneRequest
        0x7 -> CommonMessage EOX -- this shouldn't happen by itself
        0x8 -> RealtimeMessage TimingClock
        0xa -> RealtimeMessage Start
        0xb -> RealtimeMessage Continue
        0xc -> RealtimeMessage Stop
        0xe -> RealtimeMessage ActiveSense
        0xf -> RealtimeMessage Reset
        _ -> RealtimeMessage $ UndefinedRealtime code
    | otherwise = UnknownMessage status 0 0
    where (st, code) = split4 status

decode3 :: B.ByteString -> Message
decode3 bytes
    | st == 0xf && chan < 0x8 = CommonMessage common_msg
    | st == 0xb && d1 >= 0x78 = ChannelMessage chan channel_mode_msg
    | st >= 0x8 = ChannelMessage chan channel_msg
    | otherwise = UnknownMessage status d1 d2
    where
    (st, chan) = split4 status
    status = Unsafe.unsafeIndex bytes 0
    d1 = Unsafe.unsafeIndex bytes 1
    d2 = Unsafe.unsafeIndex bytes 2
    channel_msg = case st of
        0x8 -> NoteOff (Key d1) d2
            -- Hide this bit of midi irregularity from clients.
        0x9 | d2 == 0 -> NoteOff (Key d1) d2
            | otherwise -> NoteOn (Key d1) d2
        0xa -> Aftertouch (Key d1) d2
        0xb -> ControlChange d1 d2
        0xc -> ProgramChange d1
        0xd -> ChannelPressure d1
        0xe -> PitchBend (decode_pb d1 d2)
        _ -> error $ "not reached: " ++ show st
    channel_mode_msg = case d1 of
        0x78 -> AllSoundOff
        0x79 -> ResetAllControls
        0x7a -> LocalControl (d2 /= 0)
        0x7b -> AllNotesOff
        _ -> UndefinedChannelMode d1 d2
    common_msg = case chan of
        -- It's the caller's responsibility to end it with EOX.
        0x0 -> SystemExclusive d1 (B.drop 2 bytes)
        0x2 -> SongPositionPointer (join14 d1 d2)
        0x3 -> SongSelect d1
        _ -> UndefinedCommon chan

encode :: Message -> B.ByteString
encode (ChannelMessage chan msg) = B.pack [join4 st chan, d1, d2]
    where
    (st, d1, d2) = case msg of
        NoteOff (Key n) v -> (0x8, n, v)
        NoteOn (Key n) v -> (0x9, n, v)
        Aftertouch (Key n) v -> (0xa, n, v)
        ControlChange c v -> (0xb, c, v)
        ProgramChange v -> (0xc, v, 0)
        ChannelPressure v -> (0xd, v, 0)
        PitchBend v -> let (d1, d2) = encode_pb v in (0xe, d1, d2)
        -- channel mode msgs
        AllSoundOff -> (0xb, 0x78, 0)
        ResetAllControls -> (0xb, 0x79, 0)
        LocalControl on -> (0xb, 0x7a, if on then 0xff else 0)
        AllNotesOff -> (0xb, 0x7b, 0)
        _ -> error $ "unknown ChannelMessage " ++ show msg

encode (RealtimeMessage msg) = B.pack [join4 0xf st]
    where
    st = case msg of
        TimingClock -> 0x8
        Start -> 0xa
        Continue -> 0xb
        Stop -> 0xc
        ActiveSense -> 0xe
        Reset -> 0xf
        _ -> error $ "unknown RealtimeMessage " ++ show msg

encode (CommonMessage msg) = case msg of
    SystemExclusive manuf bytes -> B.append (B.pack [0xf0, manuf]) bytes
    SongPositionPointer d ->
        let (d1, d2) = split14 d in B.pack [0xf2, d1, d2]
    SongSelect d -> B.pack [0xf3, d]
    TuneRequest -> B.pack [0xf6]
    EOX -> B.pack [0xf7] -- this should have been in SystemExclusive
    _ -> error $ "unknown CommonMessage " ++ show msg

encode (UnknownMessage st d1 d2)
    = error $ "UnknownMessage: " ++ show (st, d1, d2)

-- | I map a 2s complement range to inclusive -1--1, so this is a little
-- tricky.
decode_pb :: Word8 -> Word8 -> PitchBendValue
decode_pb d1 d2
    | v < 0x2000 = (v - 0x2000) / 0x2000
    | otherwise = (v - 0x2000) / (0x2000-1)
    where v = fromIntegral (join14 d1 d2)

encode_pb :: PitchBendValue -> (Word8, Word8)
encode_pb v = split14 (floor (v*m + 0x2000))
    where m = if v < 0 then 0x2000 else 0x2000 - 1

-- | Split a Word8 into (msb, lsb) nibbles, and join back.
split4 :: Word8 -> (Word8, Word8)
split4 word = (shiftR word 4 .&. 0xf, word .&. 0xf)

-- | Join msb and lsb into a Word8.
join4 :: Word8 -> Word8 -> Word8
join4 d1 d2 = (shiftL d1 4 .&. 0xf0) .|. (d2 .&. 0x0f)
