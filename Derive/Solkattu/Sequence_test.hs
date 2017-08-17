-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances #-}
module Derive.Solkattu.Sequence_test where
import Util.Test
import qualified Derive.Solkattu.Sequence as Sequence
import Derive.Solkattu.Sequence (Note(..), Meta(..), default_tempo)
import qualified Derive.Solkattu.Tala as Tala

import Global


test_flatten_with = do
    let f = map (extract . fst) . Sequence.flatten_with default_tempo
        extract (Meta g (Sequence.Tempo speed nadai)) = (g, (speed, nadai))
    equal (f [note]) [(Nothing, (0, 4))]
    equal (f [Group 'a' [note, Group 'b' [note], note]])
        [(Just (g 3 'a'), (0, 4)), (Just (g 1 'b'), (0, 4)), (Nothing, (0, 4))]

test_tempo_to_state = do
    let f = map (e_state . fst) . snd
            . Sequence.tempo_to_state Tala.adi_tala
            . map (first Sequence._tempo) . Sequence.flatten
    equal (f [note, note, note, note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 3/4), (1, 0)]

    -- Mixed nadai.
    equal (f [note, note, nadai 6 [note, note, note], note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 4/6), (0, 5/6), (1, 0), (1, 1/4)]

    -- Change speed.
    equal (f [speed (-1) [note, note], speed 2 [note, note]])
        [(0, 0), (0, 1/2), (1, 0), (1, 1/16)]

test_normalize_speed = do
    let f = map (first e_state . second pretty_stroke . snd)
            . Sequence.normalize_speed Tala.adi_tala
            . Sequence.flatten
        n matras = Sequence.Note (matras :: Sequence.Matra)
    equal (f [n 1, n 1]) [((0, 0), '+'), ((0, 1/4), '+')]
    equal (f [sd [n 1, n 1]])
        [((0, 0), '+'), ((0, 1/4), '_'), ((0, 2/4), '+'), ((0, 3/4), '_')]
    equal (f [su [n 1, n 1], n 1, n 1])
        [ ((0, 0), '+'), ((0, 1/8), '+')
        , ((0, 2/8), '+'), ((0, 3/8), '_')
        , ((0, 4/8), '+'), ((0, 5/8), '_')
        ]
    equal (f [nadai 5 [n 1, n 1]]) [((0, 0), '+'), ((0, 1/5), '+')]
    equal (f [n 2, n 1]) [((0, 0), '+'), ((0, 1/4), '-'), ((0, 2/4), '+')]
    equal (f [su [n 1], n 2])
        [ ((0, 0), '+')
        , ((0, 1/8), '+'), ((0, 2/8), '-'), ((0, 3/8), '-'), ((0, 4/8), '-')
        ]
    equal (map snd $ f [su [su [n 1]], n 1]) "++___"
    equal (map snd $ f [sd [n 1, n 2, n 1]]) "+_+---+_"

test_normalize_speed_groups = do
    let f = extract . Sequence.normalize_speed Tala.adi_tala . Sequence.flatten
        n = Note (1 :: Sequence.Matra)
        extract ns = zip (map fst ns) (map (pretty.snd.snd) ns)
    -- Make sure groups are expanded correctly.
    equal (f [Group 'a' [n, n], n])
        [(Just (g 2 'a'), "1"), (Nothing, "1"), (Nothing, "1")]
    equal (f [sd [Group 'a' [n, n]], n])
        [ (Just (g 4 'a'), "1"), (Nothing, "_"), (Nothing, "1"), (Nothing, "_")
        , (Nothing, "1")
        ]
    equal (f [Group 'a' [n, Group 'b' [n]]])
        [(Just (g 2 'a'), "1"), (Just (g 1 'b'), "1")]

test_expand_groups = do
    let f = Sequence.expand_groups
    let a = Sequence.Attack ()
        r = Sequence.Rest
    equal (f [Just (g 1 'a'), Nothing] [a, r, a, r, r, a]) [(0, g 2 'a')]
    equal (f [Just (g 2 'a'), Nothing] [a, r, a, r, r, a]) [(0, g 5 'a')]
    equal (f [Just (g 2 'a'), Nothing, Just (g 1 'b')] [a, r, a, r, a, r, r])
        [(0, g 4 'a'), (4, g 3 'b')]

test_simplify = do
    let f = Sequence.simplify
    equal (f [su [note, note]]) [su [note, note]]
    equal (f [su [note], su [note]]) [su [note, note]]

    equal (f [su [su [note]], note]) [speed 2 [note], note]
    equal (f [su [sd [note]], note]) [note, note]
    equal (f [su [], note]) [note]
    equal (f [su [sd [note], sd [note]]]) [note, note]

    equal (f [nadai 1 [nadai 2 [note], nadai 1 [note]]])
        [nadai 2 [note], nadai 1 [note]]

    equal (f [speed (-2) [speed 1 [note], speed 2 [note]]])
        [speed (-1) [note], note]

pretty_stroke :: Sequence.Stroke a -> Char
pretty_stroke s = case s of
    Sequence.Attack _ -> '+'
    Sequence.Sustain {} -> '-'
    Sequence.Rest -> '_'

g :: Int -> g -> Sequence.GroupMark g
g = Sequence.GroupMark

e_state :: Sequence.State -> (Tala.Akshara, Sequence.Duration)
e_state state = (Sequence.state_akshara state, Sequence.state_matra state)

note :: Note Char Int
note = Sequence.Note 1

instance Sequence.HasMatras Sequence.Matra where
    matras_of = id
    has_duration n = n > 1

nadai :: Sequence.Nadai -> [Note g a] -> Note g a
nadai = TempoChange . Sequence.Nadai

speed :: Sequence.Speed -> [Note g a] -> Note g a
speed = Sequence.change_speed

su, sd :: [Note g a] -> Note g a
su = speed 1
sd = speed (-1)
