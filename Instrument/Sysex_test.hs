-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Sysex_test where
import Data.ByteString (ByteString)
import qualified Data.Map as Map

import Util.Test
import qualified Instrument.Sysex as Sysex
import Instrument.Sysex (Record(..))
import Global


test_signed_conversion :: Test
test_signed_conversion = do
    let f bits = Sysex.to_signed bits . Sysex.from_signed bits
    equal (map (f 4) [0..31]) $ [0..7] ++ replicate (32-8) 7
    equal (map (f 4) [0, -1 .. -31]) $ [0, -1 .. -8] ++ replicate (32-9) (-8)
    equal (map (f 8) [0..31]) [0..31]
    equal (map (f 8) [0, -1 .. -31]) [0, -1 .. -31]

test_decode_encode_bits :: Test
test_decode_encode_bits = do
    let f widths = Sysex.encode_bits widths . Sysex.decode_bits widths
    equal (map (f [1, 3]) [0..15]) [0..15]
    equal (map (f [1, 1, 1, 1]) [0..15]) [0..15]
    equal (map (Sysex.encode_bits [1, 1, 1])
            [[0, 0, 0], [1, 0, 0], [0, 1, 0], [1, 1, 0], [0, 0, 1]])
        [0, 1, 2, 3, 4]
    equal (map (Sysex.decode_bits [1, 1, 1]) [0..4])
        [[0, 0, 0], [1, 0, 0], [0, 1, 0], [1, 1, 0], [0, 0, 1]]
    equal (map (Sysex.decode_bits [2, 1]) [0..7])
        [[0, 0], [1, 0], [2, 0], [3, 0], [0, 1], [1, 1], [2, 1], [3, 1]]
    equal (map (Sysex.decode_bits [1, 2]) [0..7])
        [[0, 0], [1, 0], [0, 1], [1, 1], [0, 2], [1, 2], [0, 3], [1, 3]]

test_encode_decode :: Test
test_encode_decode = do
    let f specs = decode specs <=< encode specs
    let success specs record = (f specs record, Right (record, ""))

    let str_spec = [("name", Sysex.Str 4)]
    uncurry equal (success str_spec (rmap [("name", RStr "ho")]))
    left_like (f str_spec (rmap [("name", RStr "too long")]))
        "too many characters"
    left_like (f str_spec (rmap [])) "not found"

    let bits_spec = [("", Sysex.Bits
            [ ("a", Sysex.bits 1)
            , ("b", Sysex.ranged_bits 3 (0, 1))
            , ("c", Sysex.ranged_bits 3 (-1, 1))
            ])]
        bits_rmap a b c = rmap [("a", RNum a), ("b", RNum b), ("c", RNum c)]
    uncurry equal (success bits_spec (bits_rmap 1 1 1))
    uncurry equal (success bits_spec (bits_rmap 1 1 (-1)))
    left_like (f bits_spec (bits_rmap 3 1 1)) "a: num out of range"
    left_like (f bits_spec (bits_rmap 1 2 1)) "b: num out of range"

    let enum_spec =
            [ ("", Sysex.Bits [("a", (1, Sysex.Enum ["x", "y"]))])
            , ("b", Sysex.enum ["c", "d"])
            ]
        enum_rmap a b = rmap [("a", RStr a), ("b", RStr b)]
    uncurry equal (success enum_spec (enum_rmap "x" "c"))
    uncurry equal (success enum_spec (enum_rmap "y" "d"))
    left_like (f enum_spec (enum_rmap "z" "c")) "unknown enum"

test_union :: Test
test_union = do
    let f specs = decode specs <=< encode specs
    let success specs record = (f specs record, Right (record, ""))

    let union_spec =
            [ ("type", Sysex.enum ["a", "b"])
            , ("field", Sysex.Union "type" 8
                [ ("a", [("name", Sysex.Str 4)])
                , ("b", [("val", Sysex.unsigned 255)])
                ])
            ]
        union_rmap typ fields = rmap
            [("type", RStr typ), ("field", RUnion $ rmap fields)]
    uncurry equal (success union_spec (union_rmap "a" [("name", RStr "abc")]))
    uncurry equal (success union_spec (union_rmap "b" [("val", RNum 42)]))
    left_like (f union_spec (union_rmap "c" [("val", RNum 42)]))
        "unknown enum: c"
    left_like (f union_spec (union_rmap "a" [("xyz", RStr "abc")]))
        "field.name: not found"

type Val = Either String

test_lookup_put_rmap :: Test
test_lookup_put_rmap = do
    let make str num substr = rmap
            [ ("str", RStr str), ("num", RNum num)
            , ("rmap", RMap $ rmap [("a", RStr substr)])
            ]
        rm = make "str" 0 "substr"
    let get :: (Sysex.RecordVal a) => String -> Val a
        get k = Sysex.get_rmap k rm
        put :: (Show a, Sysex.RecordVal a) => String -> a -> Val Sysex.RMap
        put k v = Sysex.put_rmap k v rm
    equal (get "str" :: Val Text) (Right "str")
    equal (get "rmap.a" :: Val Text) (Right "substr")
    left_like (get "str" :: Val Int) "str: expected a TNum"
    left_like (get "rmap.a" :: Val Int) "rmap.a: expected a TNum"
    left_like (get "rmap.z" :: Val Int) "rmap.z: not found"

    equal (put "str" ("new" :: Text)) $ Right (make "new" 0 "substr")
    equal (put "num" (10 :: Int)) $ Right (make "str" 10 "substr")
    equal (put "rmap.a" ("new" :: Text)) $ Right (make "str" 0 "new")
    left_like (put "str" (0 :: Int)) "str: *is a different type"
    left_like (put "rmap.a" (0 :: Int)) "rmap.a: *is a different type"
    left_like (put "rmap.z" (0 :: Int)) "rmap.z: not found"
    left_like (put "str.a" (0 :: Int))
        "str: can't lookup field \"a\" in non-map"

rmap :: [(String, Record)] -> Sysex.RMap
rmap = Map.fromList

encode :: Sysex.Specs -> Sysex.RMap -> Either Sysex.Error ByteString
encode = Sysex.encode Sysex.config_8bit

decode :: Sysex.Specs -> ByteString
    -> Either Sysex.Error (Sysex.RMap, ByteString)
decode = Sysex.decode Sysex.config_8bit
