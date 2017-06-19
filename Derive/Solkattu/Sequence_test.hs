-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances #-}
module Derive.Solkattu.Sequence_test where
import Util.Test
import qualified Derive.Solkattu.Sequence as Sequence
import Derive.Solkattu.Sequence (Note(..), default_tempo, faster, slower)
import qualified Derive.Solkattu.Tala as Tala

import Global


test_flatten = do
    let f = map fst . Sequence.flatten
    equal (f [note]) [default_tempo]
    equal (f [note, faster [note], note])
        [default_tempo, Sequence.Tempo 1 4, default_tempo]

test_tempo_to_state = do
    let f = map (e_state . fst) . snd
            . Sequence.tempo_to_state Tala.adi_tala . Sequence.flatten
    equal (f [note, note, note, note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 3/4), (1, 0)]

    -- Mixed nadai.
    equal (f [note, note, nadai 6 [note, note, note], note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 4/6), (0, 5/6), (1, 0), (1, 1/4)]

    -- Change speed.
    equal (f [TempoChange (Sequence.ChangeSpeed (-1)) [note, note],
            TempoChange (Sequence.ChangeSpeed 2) [note, note]])
        [(0, 0), (0, 1/2), (1, 0), (1, 1/16)]

test_normalize_speed = do
    let f = map (first e_state . second pretty_stroke)
            . Sequence.normalize_speed Tala.adi_tala
            . Sequence.flatten
        n matras = Sequence.Note (matras :: Sequence.Matra)
    equal (f [n 1, n 1]) [((0, 0), '+'), ((0, 1/4), '+')]
    equal (f [slower [n 1, n 1]])
        [((0, 0), '+'), ((0, 1/4), '_'), ((0, 2/4), '+'), ((0, 3/4), '_')]
    equal (f [faster [n 1, n 1], n 1, n 1])
        [ ((0, 0), '+'), ((0, 1/8), '+')
        , ((0, 2/8), '+'), ((0, 3/8), '_')
        , ((0, 4/8), '+'), ((0, 5/8), '_')
        ]
    equal (f [nadai 5 [n 1, n 1]]) [((0, 0), '+'), ((0, 1/5), '+')]
    equal (f [n 2, n 1]) [((0, 0), '+'), ((0, 1/4), '-'), ((0, 2/4), '+')]
    equal (f [faster [n 1], n 2])
        [ ((0, 0), '+')
        , ((0, 1/8), '+'), ((0, 2/8), '-'), ((0, 3/8), '-'), ((0, 4/8), '-')
        ]
    equal (map snd $ f [faster [faster [n 1]], n 1]) "++___"

    equal (map snd $ f [slower [n 1, n 2, n 1]]) "+_+---+_"

pretty_stroke :: Sequence.Stroke a -> Char
pretty_stroke s = case s of
    Sequence.Attack _ -> '+'
    Sequence.Sustain -> '-'
    Sequence.Rest -> '_'


e_state :: Sequence.State -> (Tala.Akshara, Sequence.Duration)
e_state state = (Sequence.state_akshara state, Sequence.state_matra state)

note :: Note Int
note = Sequence.Note 1

instance Sequence.HasMatras Sequence.Matra where
    matras_of = id

nadai :: Sequence.Nadai -> [Note a] -> Note a
nadai = TempoChange . Sequence.Nadai
