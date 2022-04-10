-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances #-}
module Solkattu.S_test where
import qualified Solkattu.S as S
import Solkattu.S (Note(..), Flat(..), defaultTempo)
import qualified Solkattu.Tala as Tala

import Global
import Util.Test


test_flattenWith :: Test
test_flattenWith = do
    let f = S.flattenWith defaultTempo
        -- extract n = n -- fmap (const ()) n
        -- extract (Meta g (S.Tempo speed nadai _)) = (g, (speed, nadai))
    equal (f [su [note]]) [FNote (tempo 1 4) 1]
    equal (f [Group 'a' [note, su [Group 'b' [note]], note]])
        [ FGroup (tempo 0 4) 'a'
            [ FNote (tempo 0 4) 1
            , FGroup (tempo 1 4) 'b'
                [ FNote (tempo 1 4) 1 ]
            , FNote (tempo 0 4) 1
            ]
        ]
    equal (f [Group 'a' [Group 'b' [note], note]])
        [ FGroup (tempo 0 4) 'a'
            [ FGroup (tempo 0 4) 'b'
                [ FNote (tempo 0 4) 1 ]
            , FNote (tempo 0 4) 1
            ]
        ]

test_tempoToState :: Test
test_tempoToState = do
    let f = map (eState . fst) . snd
            . S.tempoToState Tala.adi_tala 0
            . S.tempoNotes . S.flatten
    equal (f [note, note, note, note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 3/4), (1, 0)]

    -- Mixed nadai.
    equal (f [note, note, nadai 6 [note, note, note], note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 4/6), (0, 5/6), (1, 0), (1, 1/4)]

    -- Change speed.
    equal (f [speed (-1) [note, note], speed 2 [note, note]])
        [(0, 0), (0, 1/2), (1, 0), (1, 1/16)]

    -- Stride.
    equal (f [stride 3 (replicate 5 note)])
        [(0, 0), (0, 3/4), (1, 1/2), (2, 1/4), (3, 0)]
    equal (f [stride 3 [speed 1 (replicate 4 note)]])
        [(0, 0), (0, 3/8), (0, 6/8), (1, 1/8)]

test_normalizeSpeed :: Test
test_normalizeSpeed = do
    let f = map (bimap eState prettyStroke)
            . S.flattenedNotes
            . normalizeSpeed Tala.adi_tala
            . S.flatten
        n matras = S.Note (matras :: S.Matra)
    equal (f [n 1, n 1]) [((0, 0), '+'), ((0, 1/4), '+')]
    -- It would omit the rests, but 1/nadai is the minimum dur.
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

    equal (map snd $ f [stride 3 [note, note]]) "+__+__"
    equal (map fst $ f [stride 3 [note, note]])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 3/4), (1, 0), (1, 1/4)]
    equal (map snd $ f [stride 3 [note, su [note, note]]]) "+_____+__+__"

test_normalizeSpeedGroups :: Test
test_normalizeSpeedGroups = do
    let f = map (fmap extract) . normalizeSpeed Tala.adi_tala . S.flatten
        n = Note (1 :: S.Matra)
        extract = pretty . snd
    let t0 = tempo 0 4
        t1 = tempo 1 4
    -- Make sure groups are expanded correctly.
    equal (f [Group 'a' [n, n], n])
        [ FGroup t0 'a'
            [ FNote t0 "1", FNote t0 "1" ]
        , FNote t0 "1"
        ]
    equal (f [sd [Group 'a' [n, n]], n])
        [ FGroup t0 'a'
            [ FNote t0 "1", FNote t0 "_", FNote t0 "1", FNote t0 "_" ]
        , FNote t0 "1"
        ]
    equal (f [su [Group 'a' [n, n]], n])
        [ FGroup t1 'a'
            [ FNote t1 "1", FNote t1 "1" ]
        , FNote t1 "1", FNote t1 "_"
        ]
    equal (f [Group 'a' [n, Group 'b' [n]]])
        [ FGroup t0 'a'
            [ FNote t0 "1"
            , FGroup t0 'b' [ FNote t0 "1" ]
            ]
        ]

normalizeSpeed :: S.HasMatras a => Tala.Tala -> [Flat g a]
    -> [Flat g (S.State, S.Stroke a)]
normalizeSpeed tala notes = S.normalizeSpeed (S.maxSpeed notes) tala notes

test_simplify :: Test
test_simplify = do
    let f = S.simplify
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

test_noteFMatra :: Test
test_noteFMatra = do
    let f = S.noteFMatra
    equal (f (tempo 0 4) note) 1
    equal (f (tempo 0 6) note) 1
    equal (f (tempo 1 4) note) (1/2)
    equal (f (tempo 1 6) note) (1/2)
    equal (f (tempo 0 4) $ su [note, note]) 1
    equal (f (tempo 1 4) $ su [note, note]) (1/2)
    equal (f (tempo 0 4) $ speed 0 [note, nadai 6 [note, note, note]]) 3

prettyStroke :: S.Stroke a -> Char
prettyStroke s = case s of
    S.Attack _ -> '+'
    S.Sustain {} -> '-'
    S.Rest -> '_'

eState :: S.State -> (Tala.Akshara, S.Duration)
eState state = (S.stateAkshara state, S.stateMatra state)

note :: Note Char Int
note = S.Note 1

instance S.HasMatras S.Matra where
    matrasOf = id
    hasSustain n = n > 1

nadai :: S.Nadai -> [Note g a] -> Note g a
nadai = TempoChange . S.Nadai

speed :: S.Speed -> [Note g a] -> Note g a
speed = S.changeSpeed

stride :: S.Stride -> [Note g a] -> Note g a
stride = TempoChange . S.Stride

su, sd :: [Note g a] -> Note g a
su = speed 1
sd = speed (-1)

tempo s n = S.Tempo s n 1
