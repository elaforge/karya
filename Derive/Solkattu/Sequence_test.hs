module Derive.Solkattu.Sequence_test where
import Util.Test
import qualified Derive.Solkattu.Sequence as Sequence
import Derive.Solkattu.Sequence (Note(..), Tempo(..), default_tempo)
import qualified Derive.Solkattu.Tala as Tala


test_flatten = do
    let f = map fst . Sequence.flatten
    equal (f [note]) [default_tempo]
    equal (f [note, TempoChange (Sequence.SpeedChange 1) [note], note])
        [default_tempo, Tempo 1 4, default_tempo]

test_tempo_to_state = do
    let f = map (e_state . fst)
            . Sequence.tempo_to_state (const 1) Tala.adi_tala . Sequence.flatten
    -- equal (f [note, note, note, note, note])
    --     [(0, 0), (0, 1/4), (0, 2/4), (0, 3/4), (1, 0)]

    let nadai n = TempoChange (Sequence.Nadai n)
    -- Mixed nadai.
    equal (f [note, note, nadai 6 [note, note, note], note, note])
        [(0, 0), (0, 1/4), (0, 2/4), (0, 4/6), (0, 5/6), (1, 0), (1, 1/4)]
    pprint (f [note, note, note, nadai 6 [note, note, note]])

    -- -- pprint (f [note, TempoChange (Sequence.Nadai 6) [note, note, note], note, note])
    -- -- Change speed.
    -- equal (f [TempoChange (Sequence.SpeedChange (-1)) [note, note],
    --         TempoChange (Sequence.SpeedChange 2) [note, note]])
    --     [(0, 0), (0, 1/2), (1, 0), (1, 1/16)]


e_state :: Sequence.State -> (Tala.Akshara, Sequence.Duration)
e_state state = (Sequence.state_akshara state, Sequence.state_matra state)

note :: Note ()
note = Sequence.Note ()
