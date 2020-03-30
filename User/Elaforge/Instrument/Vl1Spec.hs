-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- In ghc 8 change to:
-- {-# OPTIONS_GHC -Wno-warn-unused-top-binds #-}

-- | Utilities to encode and generate vl1 sysex dumps.
--
-- TODO
-- Basic parsing and modification works, but some fields, like effects, are not
-- parsed.
module User.Elaforge.Instrument.Vl1Spec (
    encode, decode
    , vl1_header
    , patch_spec

    , decode_num, encode_num
) where
import qualified Data.Bits as Bits
import Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8

import qualified Midi.Midi as Midi
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex (Specs, Spec(..), unsigned, bool, enum, ranged, signed)


vl1_header :: Int -> ByteString
vl1_header nbytes = B.pack
    -- spec sheet says there's a 'device number' after yamaha_code
    [ 0xf0, Midi.yamaha_code, 0, 0x7a
    , msb, lsb -- size counts from 'L' char until the checksum
    ] <> magic
    where
    (lsb, msb) = Midi.split14 (nbytes + B.length magic)
    magic = Char8.pack "LM 20117VC"


-- * config

-- | From the Vl1 MIDI spec:
--
-- > 0-127 => 00 - 7f
-- > 0-127, 128-16383      => 0000 - 007f, 0100 - 7f7f
-- > -64 - -1, 0, 1 - 63   => 40 - 7f, 00, 01 - 3f (2s complement)
-- > -128 - -1, 0, 1 - 127 => 0100 - 017f, 0000, 0001 - 007f
config :: Sysex.Config
config = Sysex.Config decode_num encode_num range_bytes

decode_num :: Sysex.NumRange -> ByteString -> Int
decode_num (low, high) bytes
    | low >= 0 = val
    | high <= 0x3f = Sysex.to_signed 7 val
    | otherwise = val .&. 0x7f - val .&. 0x80
    where
    val = B.foldl' (\val b -> Bits.shiftL val 7 .|. fromIntegral b) 0 bytes

encode_num :: Sysex.NumRange -> Int -> ByteString
encode_num (low, high) num
    | low >= 0 = B.reverse $ B.unfoldr unfold (num, range_bytes (low, high))
    | range_bytes (low, high) == 1 = B.singleton $ Sysex.from_signed 7 num
    | otherwise =
        let b = Sysex.from_signed 8 num
        in B.pack [Bits.shiftR b 7, b .&. 0x7f]
    where
    unfold (num, left)
        | left > 0 =
            Just (fromIntegral (num .&. 0x7f), (Bits.shiftR num 7, left-1))
        | otherwise = Nothing

range_bytes :: Sysex.NumRange -> Int
range_bytes (low, high) = ceiling $ logBase 2 (fromIntegral range + 1) / 7
    where range = if low < 0 then high - low else high

decode :: Specs -> ByteString -> Either String (Sysex.RMap, ByteString)
decode = Sysex.decode config

encode :: Specs -> Sysex.RMap -> Either String ByteString
encode = Sysex.encode config

assert_valid :: String -> Int -> Specs -> Specs
assert_valid = Sysex.assert_valid config

-- * specs

patch_spec :: Specs
patch_spec = assert_valid "patch_spec" size $
    [ ("header", Constant (vl1_header (size - B.length (vl1_header 0))))
    , ("memory type", unsigned 127) -- 0 = memory, 7f = buffer
    , ("memory number", unsigned 127)
    , ("", Unparsed 14)
    ] ++ common_spec ++ [("element", List 2 element_spec)]
    where size = 3100

common_spec :: Specs
common_spec = assert_valid "common_spec" 108
    [ ("name", Str 10)
    , ("", Unparsed 6)
    , ("key mode", enum ["mono", "unison", "poly", "part"])
    , ("voice mode", enum ["single", "dual"])
    , ("split mode", unsigned 2)
    , ("split point", unsigned 127)
    , ("split interval", unsigned 24)
    , ("elem1 midi rch", unsigned 15)
    , ("elem2 midi rch", unsigned 15)
    , ("poly expand mode", unsigned 31)
    , ("poly expand no", unsigned 31)
    , ("pb, at & mod mode", unsigned 2)
    , ("polyphony control", unsigned 119)
    , ("sustain", bool)
    , ("pitch bend mode", unsigned 2)
    , ("assign mode", unsigned 2)
    , ("breath attack time", unsigned 127)
    , ("breath attack gain", unsigned 127)
    , ("touch eg time", unsigned 127)
    , ("touch eg gain", unsigned 127)
    , ("portamento time midi control", bool)
    , ("portamento mode", unsigned 1)
    , ("portamento time", unsigned 127)
    , ("elem1 portamento", bool)
    , ("elem2 portamento", bool)
    , ("elem1 detune", signed 7)
    , ("elem2 detune", signed 7)
    , ("elem1 note shift", ranged (-64) 63)
    , ("elem2 note shift", ranged (-64) 63)
    , ("elem1 rand pitch", unsigned 7)
    , ("elem2 rand pitch", unsigned 7)
    , ("elem1 microtuning", unsigned 86)
    , ("elem2 microtuning", unsigned 86)
    , ("elem1 level", unsigned 127)
    , ("elem2 level", unsigned 127)
    , ("elem1 pan l", ranged (-64) 63)
    , ("elem1 pan r", ranged (-64) 63)
    , ("elem2 pan l", ranged (-64) 63)
    , ("elem2 pan r", ranged (-64) 63)
    , ("cs1 class", unsigned 4)
    , ("cs1 assign", unsigned 150)
    , ("cs2 class", unsigned 4)
    , ("cs2 assign", unsigned 150)
    , ("destination effect", unsigned 3)
    , ("destination controller", unsigned 122)
    , ("effect type", enum
        [ "flanger", "pitch change", "distortion", "chorus", "phaser"
        , "symphonic", "celeste", "distortion+flanger", "distortion+wah"
        ])
    , ("elem1 on", enum ["on", "off"]) -- naturally 0 is on, 1 is off
    , ("elem2 on", enum ["on", "off"])
    , ("effect", Unparsed 10)
    , ("feedback/reverb mode", bool)
    -- TODO just guessing about off
    , ("feedback type", enum ["off", "mono", "l/r", "l/c/r"])
    , ("feedback return", unsigned 100)
    , ("feedback data", Unparsed 18)
    , ("reverb type", unsigned 8) -- presumably an enum
    , ("reverb data", Unparsed 10)
    , ("", Unparsed 2)
    ]

-- | One unsigned byte.
unsigned1 :: Spec
unsigned1 = unsigned 127

signed64 :: Spec
signed64 = ranged (-64) 63

element_spec :: Specs
element_spec = assert_valid "element_spec" 1480
    [ ("control", SubSpec controls_spec)
    , ("trigger mode", unsigned 1) -- enum?  starting at 62
    , ("xfade speed", unsigned 96)
    , ("interpolate speed", unsigned 50)
    , ("breath noise", SubSpec
        [ ("level", unsigned1)
        , ("level break", breakpoints64 6)
        , ("hpf", unsigned 125)
        , ("hpf break", breakpoints64 2)
        , ("lpf", unsigned1)
        , ("lpf break", breakpoints64 2)
        , ("noise", unsigned 22)
        , ("key on reset", bool)
        , ("slit drive", unsigned 32)
        , ("control balance", signed64)
        ])
    , ("throat formant", SubSpec
        [ ("pitch tracking", bool)
        -- , ("pitch", unsigned 176) -- TODO -128 127 if pitch tracking == off
        , ("pitch", ranged (-128) 127)
        , ("break", breakpoints127 8)
        , ("intensity", signed 127)
        , ("intensity break", breakpoints127 4)
        , ("amount", signed64)
        , ("amount break", breakpoints64 4)
        , ("hpf", unsigned 125)
        , ("hpf break", breakpoints64 3)
        , ("lpf", unsigned1)
        , ("lpf break", breakpoints64 3)
        ])
    , ("driver", SubSpec
        [ ("output", unsigned1)
        , ("break", breakpoints64 6)
        ])
    , ("pipe/string", SubSpec
        [ ("output", unsigned1)
        , ("break", breakpoints64 6)
        ])
    , ("tap", SubSpec
        [ ("output", unsigned1)
        , ("break", breakpoints64 6)
        , ("sign", unsigned 1)
        , ("setting", unsigned 4) -- enum?
        , ("location", unsigned1)
        , ("location break", breakpoints64 8)
        ])
    , ("amplitude", SubSpec
        [ ("level", unsigned1)
        , ("break", breakpoints64 8)
        ])
    , ("name", Str 10)
    , ("", Unparsed 467)
    -- 708 -> 1479
    -- TODO
    , ("unparsed2", Unparsed 772)
    ]

breakpoints64 :: Int -> Spec
breakpoints64 n = List n
    [ ("point", unsigned1)
    , ("offset", signed64)
    ]

breakpoints127 :: Int -> Spec
breakpoints127 n = List n
    [ ("point", unsigned1)
    , ("offset", signed 127)
    ]

controls_spec :: Specs
controls_spec =
    [ ("pressure", c_simple)
    , ("embouchure", SubSpec
        [c_control, c_mode, ("upper depth", depth), ("lower depth", depth)])
    , ("pitch", SubSpec
        [ c_control, c_mode
        , ("upper depth", signed 12), ("lower depth", signed 12)
        ])
    , ("vibrato", SubSpec [c_control, ("", Unparsed 1), c_depth])
    , ("tonguing", c_simple)
    , ("amplitude", c_simple)
    , ("scream", c_complete)
    , ("breath noise", c_complete)
    , ("growl", c_complete)
    , ("throat formant", c_complete)
    , ("dynamic filter", c_simple)
    , ("harmonic enhancer", c_simple)
    , ("damping", c_simple)
    , ("absorption", c_simple)
    ]
    where
    c_simple = SubSpec [c_control, c_curve, c_depth]
    c_complete = SubSpec [c_control, c_value, c_curve, c_depth]

    -- I think if the control is 127, then this control is disabled and
    -- the rest can be ignored.
    -- c_control = ("control", unsigned 127) -- 124)
    c_control = ("control", unsigned 124)
    c_value = ("value", unsigned 127)
    c_mode = ("mode", enum ["mode1", "mode2"]) -- TODO what are these really?
    c_curve = ("curve", signed 16)
    c_depth = ("depth", depth)
    depth = signed 127


-- * modulation effects

effect_flanger :: Specs
effect_flanger =
    [ ("wave", unsigned 2)
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("phase", signed 8)
    , ("fb gain", signed 100)
    , ("high", unsigned 9)
    , ("analog feel", unsigned 10)
    , wet_dry
    ]

effect_pitch_change :: Specs
effect_pitch_change =
    [ ("mode", unsigned 1) -- enum
    , ("pitch 1", signed 12)
    , ("fine 1", signed 100)
    , ("out 1", unsigned 100)
    , ("pitch 2", signed 12)
    , ("fine 2", signed 100)
    , ("out 2", unsigned 100)
    , wet_dry
    ]

effect_distortion :: Specs
effect_distortion =
    [ ("overdrive", unsigned 100)
    , ("", Unparsed 2)
    , ("device", unsigned 4)
    , ("speaker", unsigned 5)
    , ("presence", signed 10)
    , ("output level", unsigned 100)
    ]

effect_chorus :: Specs
effect_chorus =
    [ ("mode", unsigned 1) -- enum
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("fb gain", signed 100)
    , ("high", unsigned 9)
    , wet_dry
    ]

effect_phaser :: Specs
effect_phaser =
    [ ("mode", unsigned 1) -- enum
    , ("stage", unsigned 3) -- enum
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("offset", unsigned 100)
    , ("phase", signed 8) -- diffusion 0--1 when mode=0
    , ("fb gain", signed 100)
    , wet_dry
    ]

effect_symphonic :: Specs
effect_symphonic =
    [ ("mode", unsigned 1) -- enum
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("diffusion", unsigned 10)
    , ("lo-fi", unsigned 12)
    , wet_dry
    ]

effect_celeste :: Specs
effect_celeste =
    [ ("mode", unsigned 1)
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("fb gain", signed 100)
    , ("lo-fi", unsigned 12)
    , wet_dry
    ]

effect_distortion_flanger :: Specs
effect_distortion_flanger =
    [ ("overdrive", unsigned 100)
    , ("speaker", unsigned 5)
    , ("output level", unsigned 100)
    , ("freq", unsigned 127)
    , ("depth", unsigned 100)
    , ("delay", unsigned 126)
    , ("phase", signed 8)
    , ("fb gain", unsigned 100)
    , ("high", unsigned 9)
    , ("flanger balance", unsigned 100)
    ]

effect_distortion_wah :: Specs
effect_distortion_wah =
    [ ("overdrive", unsigned 100)
    , ("speaker", unsigned 5)
    , ("output level", unsigned 100)
    , ("mode", unsigned 3)
    , ("wah pre/post", unsigned 1)
    , ("cutoff freq", unsigned 127)
    , ("resonance", unsigned 127)
    , ("sensitivity", unsigned 100)
    ]

wet_dry :: (String, Sysex.Spec)
wet_dry = ("wet/dry balance", unsigned 100)
