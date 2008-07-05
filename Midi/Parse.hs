module Midi.Parse where
import Data.Word (Word8)
import Data.Bits
import Data.List

import Midi.Midi


decode :: [Word8] -> Message
decode (status:d1:d2:bytes)
    | st == 0xf && chan < 0x8 = CommonMessage common_msg
    | st == 0xf = RealtimeMessage realtime_msg
    | st == 0xb && d1 >= 0x78 = ChannelMessage chan channel_mode_msg
    | st >= 0x8 = ChannelMessage chan channel_msg
    | otherwise = UnknownMessage status d1 d2
    where
    (st, chan) = split4 status
    channel_msg = case st of
        0x8 -> NoteOff d1 d2
        0x9 -> NoteOn d1 d2
        0xa -> Aftertouch d1 d2
        0xb -> ControlChange d1 d2
        0xc -> ProgramChange d1
        0xd -> ChannelPressure d1
        0xe -> PitchBend (join14 d2 d1 - 0x2000)
        _ -> error $ "not reached: " ++ show st
    channel_mode_msg = case d1 of
        0x78 -> AllSoundOff
        0x79 -> ResetAllControllers
        0x7a -> LocalControl (d2 /= 0)
        0x7b -> AllNotesOff
        _ -> UndefinedChannelMode d1 d2
    common_msg = case chan of
        0x0 -> SystemExclusive d1 (take_include (not.is_eox) (d2:bytes))
        0x2 -> SongPositionPointer (join14 d2 d1)
        0x3 -> SongSelect d1
        0x6 -> TuneRequest
        0x7 -> EOX -- this shouldn't happen by itself
        _ -> UndefinedCommon chan
    realtime_msg = case chan of
        0x8 -> TimingClock
        0xa -> Start
        0xb -> Continue
        0xc -> Stop
        0xe -> ActiveSense
        0xf -> Reset
        _ -> UndefinedRealtime chan
decode _ = UnknownMessage 0 0 0

is_sysex = (== 0x80)
is_eox = (== 0xf7)
is_status = (>= 0x80)
is_realtime = (>= 0xf8)

take_include _f [] = []
take_include f (x:xs)
    | f x = x : take_include f xs
    | otherwise = [x]

encode :: Message -> [Word8]
encode (ChannelMessage chan msg) = [join4 st chan, d1, d2]
    where
    (st, d1, d2) = case msg of
        NoteOff n v -> (0x8, n, v)
        NoteOn n v -> (0x9, n, v)
        Aftertouch n v -> (0xa, n, v)
        ControlChange c v -> (0xb, c, v)
        ProgramChange v -> (0xc, v, 0)
        -- channel mode msgs
        AllSoundOff -> (0xb, 0x78, 0)
        ResetAllControllers -> (0xb, 0x79, 0)
        LocalControl on -> (0xb, 0x7a, if on then 0xff else 0)
        AllNotesOff -> (0xb, 0x7b, 0)
        _ -> error $ "unknown ChannelMessage " ++ show msg

encode (RealtimeMessage msg) = [join4 0xf st]
    where
    st = case msg of
        TimingClock -> 0x8
        Start -> 0xa
        Continue -> 0xb
        Stop -> 0xc
        ActiveSense -> 0xe
        Reset -> 0xf
        _ -> error $ "unknown RealtimeMessage " ++ show msg

encode (CommonMessage msg) = join4 0xf code : bytes
    where
    (code, bytes) = case msg of
        SystemExclusive manufacturer bytes -> (0x0, manufacturer : bytes)
        SongPositionPointer d -> let (msb, lsb) = split14 d in (0x2, [lsb, msb])
        SongSelect d -> (0x3, [d])
        TuneRequest -> (0x6, [])
        EOX -> (0x7, []) -- this should have been in SystemExclusive
        _ -> error $ "unknown CommonMessage " ++ show msg

encode (UnknownMessage st d1 d2)
    = error $ "UnknownMessage: " ++ show (st, d1, d2)

-- | Split a Word8 into (msb, lsb) nibbles, and join back.
split4 :: Word8 -> (Word8, Word8)
split4 word = (shiftR word 4 .&. 0xf, word .&. 0xf)
-- | Join msb and lsb into a Word8.
join4 :: Word8 -> Word8 -> Word8
join4 d1 d2 = (shiftL d1 4 .&. 0xf0) .|. (d2 .&. 0x0f)
