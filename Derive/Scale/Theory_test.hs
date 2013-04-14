module Derive.Scale.Theory_test where
import qualified Data.Vector.Unboxed as Vector

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.TwelveScales as TwelveScales

import qualified Perform.Pitch as Pitch


test_diatonic_to_chromatic_church = do
    -- Expected intervals.
    let intervals n offset start_on = map fromIntegral $
            take n $ map (subtract offset) $ drop start_on $ scanl (+) 0 semis
            where semis = drop 5 $ cycle major_intervals
        major_intervals = [2, 2, 1, 2, 2, 2, 1]
    let f = Theory.diatonic_to_chromatic

    let cmaj = key "c-maj"
    equal (map (f cmaj (n "a")) [0..7]) (intervals 8 0 0)
    equal (map (f cmaj (n "b")) [0..7]) (intervals 8 2 1)
    equal (map (f cmaj (n "c")) [0..7]) (intervals 8 3 2)
    equal (map (f cmaj (n "d")) [0..7]) (intervals 8 5 3)
    equal (map (f cmaj (n "e")) [0..7]) (intervals 8 7 4)
    equal (map (f cmaj (n "f")) [0..7]) (intervals 8 8 5)
    equal (map (f cmaj (n "g")) [0..7]) (intervals 8 10 6)
    equal (f cmaj (n "b") 15) 25 -- 2 octave + b -> c = 25

    -- fractional transposition
    equal (f cmaj (n "c") 1.5) 3

    -- negative transposition
    equal (map (f cmaj (n "c")) [0, -1, -2]) [0, -1, -3]
    equal (map (f cmaj (n "c")) [-6, -7, -8]) [-10, -12, -13]
    equal (f cmaj (n "c") (-0.5)) (-0.5)
    equal (f cmaj (n "c") (-1.5)) (-2)

    equal (map (f (key "c-min") (n "c")) [0..3]) [0, 2, 3, 5]
    equal (map (f (key "d-min") (n "d")) [0..3]) [0, 2, 3, 5]
    equal (map (f (key "d-min") (n "d")) [0, -1, -2, -3]) [0, -2, -4, -5]

test_diatonic_to_chromatic_other = do
    let f = Theory.diatonic_to_chromatic
    -- octatonic and whole tone
    equal (map (f (key "a-octa21") (n "a")) [0..3]) [0, 2, 3, 5]
    let octa_notes = ["a", "b", "c", "d", "d#", "f", "f#", "g#"]
    equal [f (key "a-octa21") (n note) 1 | note <- octa_notes]
        [2, 1, 2, 1, 2, 1, 2, 1]
    equal (map (f (key "a-octa12") (n "a")) [0..3]) [0, 1, 3, 4]
    let whole_notes = ["a", "b", "c#", "d#", "f", "g"]
    equal [f (key "a-whole") (n note) 1 | note <- whole_notes]
        [2, 2, 2, 2, 2, 2]
    equal (map (f (key "a-whole") (n "a")) [0..3]) [0, 2, 4, 6]
    equal (map (f (key "a-whole") (n "a#")) [0..3]) [0, 2, 4, 6]

test_transpose_diatonic = do
    let f key steps pitch = Pretty.pretty $
            Theory.transpose_diatonic key steps pitch
    equal [(f (key "a-min") n (p "1a")) | n <- [0..7]]
        ["1a", "1b", "2c", "2d", "2e", "2f", "2g", "2a"]
    equal [(f (key "a-maj") n (p "1a")) | n <- [0..7]]
        ["1a", "1b", "2c#", "2d", "2e", "2f#", "2g#", "2a"]
    equal [(f (key "a-maj") n (p "1a#")) | n <- [0..7]]
        ["1a#", "1b#", "2cx", "2d#", "2e#", "2fx", "2gx", "2a#"]

    equal [(f (key "a-octa21") n (p "1a")) | n <- [0..8]]
        ["1a", "1b", "2c", "2d", "2d#", "2f", "2f#", "2g#", "2a"]
    equal [(f (key "bb-octa21") n (p "1bb")) | n <- [0..8]]
        ["1bb", "2c", "2db", "2eb", "2e", "2gb", "2g", "2a", "2bb"]

test_pitch_to_semis = do
    let semis = Theory.semis_to_nn . Theory.pitch_to_semis Twelve.layout
        pitch k = Theory.semis_to_pitch (key k) . Theory.nn_to_semis
    equal (map (semis . p) ["0c", "0c#", "0cx"]) [12, 13, 14]
    equal (map (semis . p) ["0c", "0cb", "0cbb"]) [12, 11, 10]
    equal (map (semis . p) ["1c", "1d", "1e", "1f", "1g", "1a"])
        [24, 26, 28, 29, 31, 33]
    equal (map (semis . p) ["1c", "1d", "2c"]) [24, 26, 36]

    let notes = map p ["1a", "1a#", "1ax", "1ab", "1abb"]
    equal (map (Pretty.pretty . pitch "c-maj" . semis) notes)
        ["1a", "1a#", "1b", "1g#", "1g"]

    equal (map (Pretty.pretty . pitch "a-maj") [24..36])
        ["1c", "1c#", "1d", "1d#", "1e", "1f", "1f#", "1g", "1g#", "1a",
            "1a#", "1b", "2c"]
    equal (map (Pretty.pretty . pitch "cb-maj") [23..35])
        ["1cb", "1c", "1db", "1d", "1eb", "1fb", "1f", "1gb", "1g", "1ab",
            "1a", "1bb", "2cb"]
    -- hijaz has both a flat and a sharp in D
    equal (map (Pretty.pretty . pitch "d-hijaz") [26, 27, 28, 29, 30, 31])
        ["1d", "1eb", "1e", "1f", "1f#", "1g"]

test_calculate_signature = do
    let f note ints = Vector.toList $
            Theory.calculate_signature note
                (Theory.layout_intervals Twelve.layout)
                (Vector.fromList ints)
    equal (f (n "a") [2, 1, 2, 2, 1, 2, 2]) -- minor
        [0, 0, 0, 0, 0, 0, 0]
    equal (f (n "a") [2, 1, 2, 2, 1, 3, 1]) -- harmonic minor
        [0, 0, 0, 0, 0, 0, 1]
    equal (f (n "a") [1, 3, 1, 2, 1, 2, 2]) -- hijaz
        [0, -1, 1, 0, 0, 0, 0]
    equal (f (n "a") [2, 2, 1, 2, 2, 2, 1]) -- a-major
        [0, 0, 1, 0, 0, 1, 1]
    equal (f (n "c") [2, 2, 1, 2, 2, 2, 1]) -- c-major
        [0, 0, 0, 0, 0, 0, 0]
    equal (f (n "c#") [2, 2, 1, 2, 2, 2, 1]) -- c#-major
        [1, 1, 1, 1, 1, 1, 1]
    equal (f (n "cb") [2, 2, 1, 2, 2, 2, 1]) -- cb-major
        [-1, -1, -1, -1, -1, -1, -1]

test_enharmonics_of = do
    let f = map Pretty.pretty . Theory.enharmonics_of Twelve.layout . p
    equal (f "1e") ["1fb", "1dx"]
    equal (f "1f") ["1gbb", "1e#"]
    equal (f "1b#") ["2c", "2dbb"]
    equal (f "1dbb") ["0b#", "1c"]
    equal (f "1g#") ["1ab"]
    let cycle_en = map Pretty.pretty . take 4
            . iterate (head . Theory.enharmonics_of Twelve.layout) . p
    equal (cycle_en "1b") ["1b", "2cb", "1ax", "1b"]
    equal (cycle_en "1c") ["1c", "1dbb", "0b#", "1c"]
    equal (cycle_en "1g#") ["1g#", "1ab", "1g#", "1ab"]

test_degree_of = do
    let f k note = Theory.degree_of (key k) (n note)
    -- Diatonic scales care about the letter.
    equal (map (f "a-min") ["a", "b", "c", "d", "d#", "eb", "e"])
        [0, 1, 2, 3, 3, 4, 4]
    -- But chromatic scales just care about the pitch.
    equal (map (f "a-octa21") ["a", "a#", "bb", "b"])
        [0, 0, 0, 1]
    equal (map (f "a-octa21") ["a", "b", "c", "d", "d#"])
        [0, 1, 2, 3, 4]

test_nn_to_semis = do
    let f = Theory.semis_to_pitch (key "c-maj") . Theory.nn_to_semis
    equal (Pretty.pretty (f 0)) "-1c"
    equal (Pretty.pretty (f 60)) "4c"
    equal (Pretty.pretty (f 59)) "3b"


-- * util

key :: Text -> Theory.Key
key name = either (error $ "can't parse key: " ++ show name) id $
    TwelveScales.read_key Twelve.scale_map (Just (Pitch.Key name))

p :: Text -> Theory.Pitch
p s = fromMaybe (error $ "can't parse pitch: " ++ show s) $
    Theory.read_pitch Twelve.layout s

n :: String -> Theory.Note
n s = fromMaybe (error $ "can't parse note: " ++ show s) $
    Theory.read_note Twelve.layout s
