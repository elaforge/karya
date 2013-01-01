module Derive.Call.Val_test where
import qualified Data.Map as Map
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Ui.UiTest as UiTest
import qualified Cmd.Meter as Meter
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_timestep = do
    let run start vcall = DeriveTest.extract extract $
            DeriveTest.derive_tracks_with_ui id (DeriveTest.set_ruler ruler)
                [(">", [(start, 0, ("d (t " ++ vcall ++ ") |"))])]
        extract = Score.event_start
        ruler = UiTest.ruler [(Meter.meter, mlist)]
        mlist = Ruler.marklist $ Map.fromList
            (zip [0, 1, 2, 3, 4, 6, 8, 10, 12] UiTest.m44)
    let (evts, logs) = run 0 "'r:z'"
    equal evts []
    strings_like logs ["parsing timestep"]
    -- Whole note in the first measure is 4.
    equal (run 0 "'w'") ([4], [])
    equal (run 1 "'w'") ([5], [])

    -- Whole note in the second measure is 8.
    equal (run 4 "'w'") ([12], [])
    equal (run 6 "'w'") ([14], [])
    equal (run 4 "'q'") ([6], []) -- quarter is 2

    -- Test various steps args.
    equal (run 0 "'q'") ([1], [])
    equal (run 0 "'q' 2") ([2], [])
    equal (run 1 "'q' 0") ([1], [])
    equal (run 1 "'q' -1") ([0], [])
    let (evts, logs) = run 1 "'q' 1.5"
    equal evts []
    strings_like logs ["expected Num (integral) but got"]

    -- TODO should be an error, there are no sixteenths
    equal (run 0 "'s'") ([1], [])

test_prev_next_val = do
    let runc control = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks [(">", [(0, 10, "")]), ("c", control)]
    equal (runc [(0, 0, ".5"), (1, 0, "set (<)")])
        ([[(0, 0.5), (1, 0.5)]], [])
    equal (runc [(0, 0, "set (>)"), (1, 0, ".75")])
        ([[(0, 0.75), (1, 0.75)]], [])

    let runp notes pitch = DeriveTest.extract DeriveTest.e_note2 $
            DeriveTest.derive_tracks [(">", notes), ("*", pitch)]
    -- TODO doesn't work :(
    -- equal (runp [(0, 1, "4a"), (1, 1, "<")])
    --     ([(0, 1, "4a"), (1, 1, "4a")], [])

    -- Within a note works though.
    equal (runp [(0, 2, "")] [(0, 0, "4a"), (1, 0, "<")])
        ([(0, 2, "4a")], [])
    -- Next also works, because the next event is always included.
    equal (runp [(0, 1, ""), (1, 1, "")] [(0, 0, ">"), (1, 0, "4a")])
        ([(0, 1, "4a"), (1, 1, "4a")], [])
