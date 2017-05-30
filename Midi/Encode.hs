-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.Encode (decode, encode, sox_byte, eox_byte) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as Unsafe
import Data.Monoid ((<>))
import Data.Word (Word8)

import Midi.Midi


type Word7 = Word8

decode :: B.ByteString -> Message
decode bytes = case B.length bytes of
        0 -> UnknownMessage 0 0 0
        1 -> decode3 b0 0 0 bytes
        2 -> decode3 b0 b1 0 bytes
        _ -> decode3 b0 b1 b2 bytes
    where
    b0 = Unsafe.unsafeIndex bytes 0
    b1 = Unsafe.unsafeIndex bytes 1
    b2 = Unsafe.unsafeIndex bytes 2

decode3 :: Word8 -> Word8 -> Word8 -> B.ByteString -> Message
decode3 status d1 d2 bytes
    | st == 0xf && chan < 0x8 = CommonMessage common_msg
    | st == 0xf = RealtimeMessage (realtime_msg d1)
    | st == 0xb && d1 >= 0x78 = ChannelMessage chan channel_mode_msg
    | st >= 0x8 = ChannelMessage chan channel_msg
    | otherwise = UnknownMessage status d1 d2
    where
    (st, chan) = split4 status
    channel_msg = case st of
        0x8 -> NoteOff (to_key d1) d2
            -- Hide this bit of midi irregularity from clients.
        0x9 | d2 == 0 -> NoteOff (to_key d1) d2
            | otherwise -> NoteOn (to_key d1) d2
        0xa -> Aftertouch (to_key d1) d2
        0xb -> ControlChange d1 d2
        0xc -> ProgramChange d1
        0xd -> ChannelPressure d1
        0xe -> PitchBend (decode_pb d1 d2)
        _ -> error $ "Midi decode: not reached: " ++ show st
    channel_mode_msg = case d1 of
        0x78 -> AllSoundOff
        0x79 -> ResetAllControls
        0x7a -> LocalControl (d2 /= 0)
        0x7b -> AllNotesOff
        _ -> UndefinedChannelMode d1 d2
    common_msg = case chan of
        0x0 -> SystemExclusive d1 $ B.drop 2 $ drop_eox bytes
        0x2 -> SongPositionPointer (join14 d1 d2)
        0x3 -> SongSelect d1
        0x6 -> TuneRequest
        0x7 -> EOX -- this shouldn't happen by itself
        _ -> UndefinedCommon chan
    drop_eox bytes
        | not (B.null bytes) && B.last bytes == eox_byte =
            B.take (B.length bytes - 1) bytes
        | otherwise = bytes
    realtime_msg byte = case chan of
        0x1 -> MtcQuarterFrame (decode_mtc byte)
        0x8 -> TimingClock
        0xa -> Start
        0xb -> Continue
        0xc -> Stop
        0xe -> ActiveSense
        0xf -> Reset
        _ -> UndefinedRealtime chan

encode :: Message -> B.ByteString
encode (ChannelMessage chan msg) = B.pack $ join1 $ case msg of
        NoteOff key v -> [0x8, encode_key key, v]
        NoteOn key v -> [0x9, encode_key key, v]
        Aftertouch key v -> [0xa, encode_key key, v]
        ControlChange c v -> [0xb, c, v]
        ProgramChange v -> [0xc, v]
        ChannelPressure v -> [0xd, v]
        PitchBend v -> let (d1, d2) = encode_pb v in [0xe, d1, d2]
        PitchBendInt d1 d2 -> [0xe, d1, d2]
        -- channel mode msgs
        AllSoundOff -> [0xb, 0x78, 0]
        ResetAllControls -> [0xb, 0x79, 0]
        LocalControl on -> [0xb, 0x7a, if on then 0xff else 0]
        AllNotesOff -> [0xb, 0x7b, 0]
        UndefinedChannelMode d1 d2 -> [0xb, d1, d2]
    where
    join1 (b:bs) = join4 b chan : bs
    join1 [] = []

encode (RealtimeMessage (MtcQuarterFrame timing)) =
    B.pack [0xf1, encode_mtc timing]
encode (RealtimeMessage msg) = B.pack [join4 0xf st]
    where
    st = case msg of
        MtcQuarterFrame _ -> 0x1 -- not reached, due to pattern match above
        TimingClock -> 0x8
        Start -> 0xa
        Continue -> 0xb
        Stop -> 0xc
        ActiveSense -> 0xe
        Reset -> 0xf
        UndefinedRealtime _ ->
            error $ "Midi encode: unknown RealtimeMessage " ++ show msg

encode (CommonMessage msg) = case msg of
    SystemExclusive manuf bytes ->
        B.pack [sox_byte, manuf] <> bytes <> B.pack [eox_byte]
    SongPositionPointer d ->
        let (d1, d2) = split14 d in B.pack [0xf2, d1, d2]
    SongSelect d -> B.pack [0xf3, d]
    TuneRequest -> B.pack [0xf6]
    EOX -> B.pack [eox_byte] -- this should have been in SystemExclusive
    _ -> error $ "Midi encode: unknown CommonMessage " ++ show msg

encode (UnknownMessage st d1 d2) =
    error $ "Midi encode: UnknownMessage: " ++ show (st, d1, d2)

sox_byte, eox_byte :: Word7
sox_byte = 0xf0
eox_byte = 0xf7

-- * util

decode_mtc :: Word7 -> Mtc
decode_mtc byte = Mtc (toEnum (fromIntegral frag)) val
    where (frag, val) = split4 byte

encode_mtc :: Mtc -> Word7
encode_mtc (Mtc frag val) = join4 (fromIntegral (fromEnum frag)) val

-- | I map a 2s complement range to inclusive -1--1, so this is a little
-- tricky.
decode_pb :: Word7 -> Word7 -> PitchBendValue
decode_pb d1 d2
    | v < 0x2000 = (v - 0x2000) / 0x2000
    | otherwise = (v - 0x2000) / (0x2000-1)
    where v = fromIntegral (join14 d1 d2)

encode_pb :: PitchBendValue -> (Word7, Word7)
encode_pb v = split14 (floor (v*m + 0x2000))
    where m = if v < 0 then 0x2000 else 0x2000 - 1

encode_key :: Key -> Word7
encode_key = from_key . min 127 . max 0
