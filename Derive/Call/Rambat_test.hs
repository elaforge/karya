module Derive.Call.Rambat_test where
import Util.Test

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Derive as Derive

import qualified Perform.Pitch as Pitch


test_tick = do
    -- This also tests some error checking and absolute warp functions.
    let extract = DeriveTest.extract_events $ \e ->
            (Score.event_start e, Score.event_duration e, pitch e,
                Score.initial_velocity e)
        pitch e = let Pitch.Degree p = Score.initial_pitch e in p
    let run evts tracks = extract $ DeriveTest.derive_tracks $
            ("tempo", [(0, 0, ".5")])
            : (DeriveTest.default_inst_title, evts)
            : tracks
    let vel = Derive.default_velocity

    let (_evts, logs) = run
            [(0, 1, "tick"), (1, 1, "tick"), (2, 1, "")]
            [("*twelve", [(0, 0, "4c"), (2, 0, "4d")])]
    strings_like logs ["no previous event"]

    -- tick is a constant time before second note regardless of tempo
    let (evts, logs) = run [(0, 1, ""), (1, 1, "tick .5"), (2, 1, "")]
            [("*twelve", [(0, 0, "4c"), (2, 0, "4d")])]
    equal logs []
    equal evts $ Right
        [ (0, 2, 60, vel)
        , (3.5,  0.5, 61, vel*0.5)
        , (4, 2, 62, vel)
        ]

    -- tick won't go between halfway between the two notes
    let (evts, logs) = run [(0, 0.5, ""), (0.5, 0.5, "tick 10 1"), (1, 0.5, "")]
            [("*twelve", [(0, 0, "4d"), (1, 0, "4c")])]
    equal logs []
    equal evts $ Right
        [ (0, 1, 62, vel)
        , (1, 1, 61, vel)
        , (2, 1, 60, vel)
        ]
