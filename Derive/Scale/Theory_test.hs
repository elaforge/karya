-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Theory_test where
import qualified Data.Vector.Unboxed as Vector

import Util.Test
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch
import Global


test_diatonic_to_chromatic_church :: Test
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

test_diatonic_to_chromatic_other :: Test
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

test_transpose_diatonic :: Test
test_transpose_diatonic = do
    let f key steps pitch = show_pitch $
            Theory.transpose_diatonic key steps pitch
        fs key_ base steps = [f (key key_) step (p base) | step <- [0..steps]]
    equal (fs "a-min" "1a" 7)
        ["1a", "1b", "2c", "2d", "2e", "2f", "2g", "2a"]
    equal (fs "a-maj" "1a" 7)
        ["1a", "1b", "2c#", "2d", "2e", "2f#", "2g#", "2a"]
    equal (fs "a-maj" "1a#" 7)
        ["1a#", "1b#", "2cx", "2d#", "2e#", "2fx", "2gx", "2a#"]

    equal (fs "f#-min" "2f#" 7)
        ["2f#", "2g#", "2a", "2b", "3c#", "3d", "3e", "3f#"]
    equal (fs "bb-maj" "2bb" 7)
        ["2bb", "3c", "3d", "3eb", "3f", "3g", "3a", "3bb"]

    equal (fs "a-octa21" "1a" 8)
        ["1a", "1b", "2c", "2d", "2d#", "2f", "2f#", "2g#", "2a"]
    equal (fs "bb-octa21" "1bb" 8)
        ["1bb", "2c", "2db", "2eb", "2e", "2gb", "2g", "2a", "2bb"]

test_pitch_to_semis :: Test
test_pitch_to_semis = do
    let semis = Theory.pitch_to_semis Twelve.layout
        pitch k = Theory.semis_to_pitch (key k)
        mkp oct pc accs = Pitch.Pitch oct (Pitch.Degree pc accs)

    -- If the pc is too high, it wraps the octave.
    equal [semis (mkp 4 pc 0) | pc <- [6, 7, 8, 9]] [59, 60, 62, 64]

    equal (map (semis . p) ["0c", "0c#", "0cx"]) [0, 1, 2]
    equal (map (semis . p) ["0c", "0cb", "0cbb"]) [0, -1, -2]
    equal (map (semis . p) ["1c", "1d", "1e", "1f", "1g", "1a"])
        [12, 14, 16, 17, 19, 21]
    equal (map (semis . p) ["1c", "1d", "2c"]) [12, 14, 24]

    let notes = map p ["1a", "1a#", "1ax", "1ab", "1abb"]
    equal (map (show_pitch . pitch "c-maj" . semis) notes)
        ["1a", "1a#", "1b", "1g#", "1g"]

    equal (map (show_pitch . pitch "a-maj") [12..24])
        ["1c", "1c#", "1d", "1d#", "1e", "1f", "1f#", "1g", "1g#", "1a",
            "1a#", "1b", "2c"]
    equal (map (show_pitch . pitch "cb-maj") [11..23])
        ["1cb", "1c", "1db", "1d", "1eb", "1fb", "1f", "1gb", "1g", "1ab",
            "1a", "1bb", "2cb"]
    -- hijaz has both a flat and a sharp in D
    equal (map (show_pitch . pitch "d-hijaz") [14..19])
        ["1d", "1eb", "1e", "1f", "1f#", "1g"]

test_calculate_signature :: Test
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

test_enharmonics_of :: Test
test_enharmonics_of = do
    let f = map show_pitch . Theory.enharmonics_of Twelve.layout . p
    equal (f "1e") ["1fb", "1dx"]
    equal (f "1f") ["1gbb", "1e#"]
    equal (f "1b#") ["2c", "2dbb"]
    equal (f "1dbb") ["0b#", "1c"]
    equal (f "1g#") ["1ab"]
    let cycle_en = map show_pitch . take 4
            . iterate (head . Theory.enharmonics_of Twelve.layout) . p
    equal (cycle_en "1b") ["1b", "2cb", "1ax", "1b"]
    equal (cycle_en "1c") ["1c", "1dbb", "0b#", "1c"]
    equal (cycle_en "1g#") ["1g#", "1ab", "1g#", "1ab"]

test_step_of :: Test
test_step_of = do
    let f k note = Theory.step_of (key k) (n note)
    -- Diatonic scales care about the letter.
    equal (map (f "a-min") ["a", "b", "c", "d", "d#", "eb", "e"])
        [0, 1, 2, 3, 3, 4, 4]
    -- But chromatic scales just care about the pitch.
    equal (map (f "a-octa21") ["c", "c#", "db", "d"])
        [0, 0, 0, 1]
    equal (map (f "a-octa21") ["c", "d", "eb", "f", "f#"])
        [0, 1, 2, 3, 4]


-- * util

key :: Text -> Theory.Key
key name = either (const $ error $ "can't parse key: " ++ show name) id $
    ChromaticScales.read_key Twelve.absolute_scale_map (Just (Pitch.Key name))

p :: Text -> Pitch.Pitch
p s = either (const $ error $ "can't parse pitch: " ++ show s)
    TheoryFormat.relative_to_absolute $
        TheoryFormat.read_relative_pitch TheoryFormat.absolute_c (Pitch.Note s)

n :: Text -> Pitch.Degree
n s = Pitch.pitch_degree $
    either (const $ error $ "can't parse degree: " ++ show s)
        TheoryFormat.relative_to_absolute $
            TheoryFormat.read_relative_pitch TheoryFormat.absolute_c $
            Pitch.Note ("0" <> s)

show_pitch :: Pitch.Pitch -> Text
show_pitch p = maybe (error $ "can't show pitch: " ++ show p) Pitch.note_text $
    Twelve.show_pitch p
