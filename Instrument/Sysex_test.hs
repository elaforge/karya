{-# LANGUAGE OverloadedStrings #-}
module Instrument.Sysex_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex (Record(..))


test_signed_conversion = do
    let f bits = Sysex.to_signed bits . Sysex.from_signed bits
    equal (map (f 4) [0..31]) $ [0..7] ++ replicate (32-8) 7
    equal (map (f 4) [0, -1 .. -31]) $ [0, -1 .. -8] ++ replicate (32-9) (-8)
    equal (map (f 8) [0..31]) [0..31]
    equal (map (f 8) [0, -1 .. -31]) [0, -1 .. -31]

test_decode_encode_bits = do
    let f widths = Sysex.encode_bits widths . Sysex.decode_bits widths
    equal (map (f [1, 3]) [0..15]) [0..15]
    equal (map (f [1, 1, 1, 1]) [0..15]) [0..15]
    equal (map (Sysex.decode_bits [1, 1, 1]) [0..7])
        [ [0, 0, 0], [0, 0, 1], [0, 1, 0], [0, 1, 1]
        , [1, 0, 0], [1, 0, 1], [1, 1, 0], [1, 1, 1]
        ]
    equal (map (Sysex.decode_bits [1, 2]) [0..7])
        [[0, 0], [0, 1], [0, 2], [0, 3], [1, 0], [1, 1], [1, 2], [1, 3]]
    equal (map (Sysex.decode_bits [2, 1]) [0..7])
        [[0, 0], [0, 1], [1, 0], [1, 1], [2, 0], [2, 1], [3, 0], [3, 1]]

test_encode_decode = do
    let f specs = Sysex.decode specs <=< Sysex.encode specs
    let success specs record = (f specs record, Right (record, ""))

    let str_spec = [Sysex.Str "name" 4]
    uncurry equal (success str_spec (rmap [("name", RStr "ho")]))
    left_like (f str_spec (rmap [("name", RStr "too long")]))
        "too many characters"
    left_like (f str_spec (rmap [])) "not found"
    left_like (f str_spec (RStr "foo")) "can't lookup name in non-map"

    let bits_spec = [Sysex.Bits
            [ ("a", Sysex.bits 1)
            , ("b", Sysex.ranged_bits 3 (0, 1))
            , ("c", Sysex.ranged_bits 3 (-1, 1))
            , ("", Sysex.bits 1)
            ]]
        bits_rmap a b c = rmap [("a", RNum a), ("b", RNum b), ("c", RNum c)]
    uncurry equal (success bits_spec (bits_rmap 1 1 1))
    uncurry equal (success bits_spec (bits_rmap 1 1 (-1)))
    left_like (f bits_spec (bits_rmap 3 1 1)) "val out of range"
    left_like (f bits_spec (bits_rmap 1 2 1)) "val out of range"

    let enum_spec = [Sysex.Bits [("a", (1, Sysex.Enum ["x", "y"]))]]
        enum_rmap a = rmap [("a", REnum a)]
    uncurry equal (success enum_spec (enum_rmap "x"))
    uncurry equal (success enum_spec (enum_rmap "y"))
    left_like (f enum_spec (enum_rmap "z")) "unknown enum"

test_union = do
    let f specs = Sysex.decode specs <=< Sysex.encode specs
    let success specs record = (f specs record, Right (record, ""))

    let union_spec =
            [ Sysex.enum_byte "type" ["a", "b"]
            , Sysex.Union "field" "type" 8
                [ ("a", [Sysex.Str "name" 4])
                , ("b", [Sysex.byte "val" 255])
                ]
            ]
        union_rmap typ field = rmap
            [("type", REnum typ), ("field", RUnion (rmap field))]
    uncurry equal (success union_spec (union_rmap "a" [("name", RStr "abc")]))
    uncurry equal (success union_spec (union_rmap "b" [("val", RNum 42)]))
    left_like (f union_spec (union_rmap "c" [("val", RNum 42)]))
        "unknown enum: c"
    left_like (f union_spec (union_rmap "a" [("xyz", RStr "abc")]))
        "field.name: not found"

rmap :: [(String, Record)] -> Record
rmap = RMap . Map.fromList
