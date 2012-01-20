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
                Score.initial_velocity e)
    let run evts tracks = extract $ DeriveTest.derive_tracks $
            ("tempo", [(0, 0, ".5")])
            : tracks ++ [(DeriveTest.default_inst_title, evts)]
    let vel = Derive.default_velocity

    let (_evts, logs) = run
            [(0, 1, "tick"), (1, 1, "tick"), (2, 1, "")]
            [("*twelve", [(0, 0, "4c"), (2, 0, "4d")])]
    strings_like logs ["no previous event"]

    -- tick is a constant time before second note regardless of tempo
    let (evts, logs) = run [(0, 1, ""), (1, 1, "tick .5"), (2, 1, "")]
            [("*twelve", [(0, 0, "4c"), (2, 0, "4d")])]
    equal logs []
    equal evts
        [ (0, 2, Just 60, vel)
        , (RealTime.seconds 3.5, RealTime.seconds 0.5, Just 61, vel*0.5)
        , (4, 2, Just 62, vel)
        ]

    -- a tick that doesn't have room for the requested duration will go halfway
    -- between the two notes
    let (evts, logs) = run [(0, 0.5, ""), (0.5, 0.5, "tick 10 1"), (1, 0.5, "")]
            [("*twelve", [(0, 0, "4d"), (1, 0, "4c")])]
    equal logs []
    equal evts
        [ (0, 1, Just 62, vel)
        , (1, 1, Just 61, vel)
        , (2, 1, Just 60, vel)
        ]
