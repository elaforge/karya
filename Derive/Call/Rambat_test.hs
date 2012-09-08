module Derive.Call.Rambat_test where
import Util.Test
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime


test_tick = do
    -- This also tests some error checking and absolute warp functions.
    let extract = DeriveTest.extract $ \e ->
            (Score.event_start e, Score.event_duration e, Score.initial_nn e,
                Score.initial_dynamic e)
    let run invert evts pitches = extract $ DeriveTest.linear_derive_tracks id
            (("tempo", [(0, 0, ".5")])
            : if invert then [note, ("*twelve", pitches)]
                else [("*twelve", pitches), note])
            where note = (DeriveTest.default_inst_title, evts)
    let vel = Derive.default_dynamic

    let (_evts, logs) = run False
            [(0, 1, "tick"), (1, 1, "tick"), (2, 1, "")]
            [(0, 0, "4c"), (2, 0, "4d")]
    strings_like logs ["previous event"]

    -- tick is a constant time before second note regardless of tempo
    let (evts, logs) = run False [(0, 1, ""), (1, 1, "tick .5"), (2, 1, "")]
            [(0, 0, "4c"), (2, 0, "4d")]
    equal logs []
    equal evts
        [ (0, 2, Just 60, vel)
        , (RealTime.seconds 3.5, RealTime.seconds 0.5, Just 61, vel*0.5)
        , (4, 2, Just 62, vel)
        ]

    -- Tick works under inversion as well.
    equal (run True
            [(0, 1, ""), (1, 1, "tick .5"), (2, 1, "")]
            [(0, 0, "4c"), (2, 0, "4d")])
        ([ (0, 2, Just 60, vel)
        , (RealTime.seconds 3.5, RealTime.seconds 0.5, Just 61, vel*0.5)
        , (4, 2, Just 62, vel)
        ], [])

    -- a tick that doesn't have room for the requested duration will go halfway
    -- between the two notes
    let (evts, logs) = run False
            [(0, 0.5, ""), (0.5, 0.5, "tick 10 1"), (1, 0.5, "")]
            [(0, 0, "4d"), (1, 0, "4c")]
    equal logs []
    equal evts
        [ (0, 1, Just 62, vel)
        , (1, 1, Just 61, vel)
        , (2, 1, Just 60, vel)
        ]

test_neighbor = do
    let run = DeriveTest.extract DeriveTest.e_note2 $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , (">s/1", [(2, 8, "up .15s 1s")])
            , ("*", [(0, 0, "4c")])
            ]
    equal run ([(0.85, 1, "4d"), (1, 4, "4c")], [])
