module Derive.Call.PitchHigh_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_drop = do
    let run pitches dyn = extract $ DeriveTest.derive_tracks
            [(">", [(0, 10, "")]), ("*", pitches), ("dyn", dyn)]
        extract = head . (DeriveTest.extract_events $ \e ->
            (DeriveTest.e_nns e, DeriveTest.e_dyn e))
    equal (run [(0, 0, "4c"), (1, 0, "drop 2 2")] [(0, 0, "1")])
        ( [(0, 60), (2, 59), (3, 58)]
        , [(0, 1), (1, 1), (2, 0.5), (3, 0)]
        )
    equal (run [(0, 0, "4c"), (1, 0, "drop 2 2")] [(0, 0, ".5")])
        ( [(0, 60), (2, 59), (3, 58)]
        , [(0, 0.5), (1, 0.5), (2, 0.25), (3, 0)]
        )

test_drop_noninverted = do
    let run ps ns = DeriveTest.extract DeriveTest.e_dyn $
            DeriveTest.derive_tracks [("*", ps), (">", ns)]
    equal (run [(0, 0, "4c"), (1, 0, "drop 2 2"), (4, 4, "4c")]
            [(0, 4, ""), (4, 4, "")])
        ([[(0, 1), (1, 1), (2, 0.5), (3, 0)], [(4, 1)]], [])

test_drop_lift_note = do
    let run note = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
            [("*", [(0, 0, "4c")]), (">", [(0, 4, note)])]
    equal (run "drop 2 2 |") ([[(0, 60), (3, 59), (4, 58)]], [])
    equal (run "lift 2 2 |") ([[(0, 60), (3, 61), (4, 62)]], [])
    equal (run "lift (5c) 2 |") ([[(0, 60), (3, 66), (4, 72)]], [])
