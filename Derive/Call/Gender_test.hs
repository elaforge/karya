module Derive.Call.Gender_test where
import Util.Test
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_tick = do
    -- This also tests some error checking and absolute warp functions.
    let extract = DeriveTest.extract $ \e ->
            (Score.event_start e, Score.event_duration e,
                DeriveTest.e_twelve e, Score.initial_dynamic e)
    let run = extract . DeriveTest.linear_derive_tracks id
    let dyn = Derive.default_dynamic
        ctod evt1 evt2 evt3 =
            [ (">", [(0, 1, evt1), (1, 1, evt2), (2, 1, evt3)])
            , ("*", [(0, 0, "4c"), (2, 0, "4d")])
            ]

    strings_like (snd $ run $ ctod "'" "'" "") ["previous event"]

    -- tick is a constant time before second note regardless of tempo
    let (evts, logs) = run $
            ("tempo", [(0, 0, ".5")]) : ctod "" "' .5 .5" ""
    equal logs []
    equal evts
        [ (0, 2, "4c", dyn)
        , (3.5, 1, "4c#", dyn*0.5)
        , (4, 2, "4d", dyn)
        ]

    -- tick damp time doesn't go past second note
    let (evts, logs) = run $ ctod "" "' .5 5 1" ""
    equal logs []
    equal evts
        [ (0, 1, "4c", dyn)
        , (1.5, 1.5, "4c#", dyn) -- dyn_scale arg is 1
        , (2, 1, "4d", dyn)
        ]

    -- If it doesn't have room for the requested duration it will go halfway
    -- between the two notes
    let (evts, logs) = run $ ctod "" "' 10 1 1" ""
    equal logs []
    equal evts
        [ (0, 1, "4c", dyn)
        , (1, 2, "4c#", dyn)
        , (2, 1, "4d", dyn)
        ]

    -- Tick works when not inverted as well.
    let (evts, logs) = run
            [ ("*", [(0, 0, "4c"), (2, 0, "4d")])
            , (">", [(0, 1, ""), (1, 1, "' .5 1 1"), (2, 1, "")])
            ]
    equal logs []
    equal evts
        [ (0, 1, "4c", dyn)
        , (1.5, 1.5, "4c#", dyn)
        , (2, 1, "4d", dyn)
        ]

test_neighbor = do
    let run = DeriveTest.extract DeriveTest.e_note2
            .  DeriveTest.derive_tracks . (++ [("*", [(0, 0, "4c")])])
    let result = run
            [ ("tempo", [(0, 0, "2")])
            , (">s/1", [(2, 8, "up .15s 2s")])
            ]
    equal result ([(0.85, 2.15, "3b"), (1, 4, "4c")], [])
    -- Starting at zero means the grace note is negative, but it gets mashed up
    -- to 0.
    equal (run [(">s/1", [(0, 1, "up .15 .5")])])
        ([(0, 0.5, "3b"), (0, 1, "4c")], [])
    -- Stops when main note does.
    equal (run [(">s/1", [(1, 1, "up .5 4")])])
        ([(0.5, 1.5, "3b"), (1, 1, "4c")], [])
