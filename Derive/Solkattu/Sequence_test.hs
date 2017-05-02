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
    let f = map (e_state . fst)
            . Sequence.tempo_to_state (const 1) Tala.adi_tala . Sequence.flatten
    -- equal (f [note, note, note, note, note])
    --     [(0, 0), (0, 1/4), (0, 2/4), (0, 3/4), (1, 0)]

    -- Mixed nadai.
    equal (f [note, note, nadai 6 [note, note, note], note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 4/6), (0, 5/6), (1, 0), (1, 1/4)]
    pprint (f [note, note, note, nadai 6 [note, note, note]])

    -- -- pprint (f [note, TempoChange (Sequence.Nadai 6) [note, note, note], note, note])
    -- -- Change speed.
    -- equal (f [TempoChange (Sequence.SpeedChange (-1)) [note, note],
    --         TempoChange (Sequence.SpeedChange 2) [note, note]])
    --     [(0, 0), (0, 1/2), (1, 0), (1, 1/16)]

test_normalize_speed = do
    let f = map (first e_state . second pretty_stroke)
            . Sequence.normalize_speed id Tala.adi_tala
            . Sequence.flatten
        n matras = Sequence.Note matras
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

note :: Note ()
note = Sequence.Note ()

nadai :: Sequence.Nadai -> [Note a] -> Note a
nadai = TempoChange . Sequence.Nadai
