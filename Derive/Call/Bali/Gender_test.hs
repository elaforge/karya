-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Gender_test where
import Util.Test
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_tick = do
    -- This also tests some error checking and absolute warp functions.
    let extract = DeriveTest.extract $ \e ->
            (Score.event_start e, Score.event_duration e,
                DeriveTest.e_pitch e, Score.initial_dynamic e)
    let run = extract . DeriveTest.derive_tracks_linear "import bali.gender"
    let dyn = Derive.default_dynamic
        c_to_e evt1 evt2 =
            [ (">", [(0, 1, evt1), (2, 1, evt2)])
            , ("*", [(0, 0, "4c"), (2, 0, "4e")])
            ]

    strings_like (snd $ run $ c_to_e "'" "'") ["previous event"]

    -- Starting at 0 will emit an event at negative time.
    -- Thanks to the "x <= 0 means constant" hack the pitch is accurate even
    -- though TimeVector.constant starts it at 0.
    equal (run $ c_to_e "'^ .5 .5 1" "")
        ([(-0.5, 1, "3b", dyn), (0, 1, "4c", dyn), (2, 1, "4e", dyn)], [])
    -- tick is a constant time before second note regardless of tempo
    let (evts, logs) = run $ ("tempo", [(0, 0, ".5")]) : c_to_e "" "' .5 .5"
    equal logs []
    equal evts
        [ (0, 2, "4c", dyn)
        , (3.5, 1, "4d", dyn*0.75)
        , (4, 2, "4e", dyn)
        ]

    -- tick damp time doesn't go past second note
    let (evts, logs) = run $ c_to_e "" "' .5 5 1"
    equal logs []
    equal evts
        [ (0, 1, "4c", dyn)
        , (1.5, 1.5, "4d", dyn) -- dyn_scale arg is 1
        , (2, 1, "4e", dyn)
        ]

    -- If it doesn't have room for the requested duration it will go halfway
    -- between the two notes
    let (evts, logs) = run $ c_to_e "" "' 10 1 1"
    equal logs []
    equal evts
        [ (0, 1, "4c", dyn)
        , (1, 2, "4d", dyn)
        , (2, 1, "4e", dyn)
        ]

    -- Tick works when not inverted as well.
    let (evts, logs) = run
            [ ("*", [(0, 0, "4c"), (2, 0, "4e")])
            , (">", [(0, 1, ""), (2, 1, "' .5 1 1")])
            ]
    equal logs []
    equal evts
        [ (0, 1, "4c", dyn)
        , (1.5, 1.5, "4d", dyn)
        , (2, 1, "4e", dyn)
        ]

    -- Explicit down tick.
    equal (run $ c_to_e "" "'_ .5 .5 1")
        ([(0, 1, "4c", dyn), (1.5, 1, "4f", dyn), (2, 1, "4e", dyn)], [])

test_tick_damp = do
    let run notes pitches = extract $
            DeriveTest.derive_tracks "import bali.gender"
            [("> | realize-damp", notes), ("*", pitches)]
        extract = DeriveTest.extract DeriveTest.e_note
    equal (run [(0, 1, ""), (1, 1, "' .1 .5")] [(0, 0, "4c"), (1, 0, "4e")])
        ([(0, 1.5, "4c"), (0.9, 0.6, "4d"), (1, 1, "4e")], [])
