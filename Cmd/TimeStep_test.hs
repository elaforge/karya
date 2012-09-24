module Cmd.TimeStep_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.TimeStep as TimeStep
import Cmd.TimeStep (Step(..), MarklistMatch(..), Tracks(..))


mk = TimeStep.time_step
merge = TimeStep.merge

test_show_step = do
    let f step = TimeStep.show_step (Just TimeStep.Advance) (mk 0 step)
    equal (f (Absolute 2)) "+abs:2t"
    equal (TimeStep.show_step (Just TimeStep.Advance) (mk 1 (Absolute 2)))
        "+2*abs:2t"

    equal (f (AbsoluteMark AllMarklists 3)) "+mark:r3"
    equal (f (AbsoluteMark (NamedMarklists ["a", "b"]) 3))
        "+mark:a,b/r3"
    equal (f BlockEnd) "+end"
    equal (TimeStep.show_step Nothing
            (merge 0 (EventStart CurrentTrack) (mk 0 (EventEnd CurrentTrack))))
        "start;end"

test_get_points = do
    let ustate = UiTest.exec State.empty $ do
            UiTest.mkblock (UiTest.default_block_name,
                [ (">", [(0, 1, "a"), (2, 1, "b")])
                , ("c", [(0, 0, "1"), (5, 0, "2")])
                ])
            State.modify_ruler UiTest.default_ruler_id $
                const (UiTest.mkruler 7 1)
    let f pos step = UiTest.eval ustate $
            TimeStep.get_points step UiTest.default_block_id 1 pos
    equal (f 0 (mk 0 (Absolute 32))) (Just [0])
    equal (f 3 (mk 0 (Absolute 32))) (Just [3])
    equal (f 0 (mk 0 (Absolute 3))) (Just [0, 3, 6])
    equal (f 1 (mk 0 (Absolute 3))) (Just [1, 4, 7])
    equal (f 0 (mk 1 (Absolute 3))) (Just [0, 6])
    equal (f 0 (mk 0 (AbsoluteMark AllMarklists 1))) (Just [0, 4])
    equal (f 1 (mk 0 (AbsoluteMark AllMarklists 1))) (Just [0, 4])
    equal (f 0 (mk 0 (AbsoluteMark AllMarklists 2))) (Just (Seq.range 0 7 1))
    equal (f 0 (mk 1 (AbsoluteMark AllMarklists 2))) (Just [0, 2, 4, 6])
    equal (f 0 (mk 0 (RelativeMark AllMarklists 1))) (Just [0, 4])
    equal (f 1 (mk 0 (RelativeMark AllMarklists 1))) (Just [1, 5])
    equal (f 0 (mk 0 BlockEnd)) (Just [0, 7])
    equal (f 0 (mk 0 (EventStart CurrentTrack))) (Just [0, 2])
    equal (f 0 (mk 0 (EventStart AllTracks))) (Just [0, 2, 5])
    equal (f 0 (mk 0 (EventStart (TrackNums [1])))) (Just [0, 2])
    equal (f 0 (mk 0 (EventStart (TrackNums [1, 2])))) (Just [0, 2, 5])
    equal (f 0 (mk 0 (EventEnd CurrentTrack))) (Just [1, 3])
    -- merged
    let merged ts1 ts2 = (f 0 (merge 0 ts1 (mk 0 ts2)), Seq.drop_dups id <$>
            (Seq.merge <$> f 0 (mk 0 ts1) <*> f 0 (mk 0 ts2)))
    uncurry equal $ merged (EventEnd AllTracks) (EventStart AllTracks)
    uncurry equal $ merged (EventEnd AllTracks) (EventStart (TrackNums [1]))
    uncurry equal $
        merged (EventStart (TrackNums [1])) (EventEnd (TrackNums [2]))

test_step_from_points = do
    let f n pos = TimeStep.step_from_points n pos (Seq.range 1 4 1)
    equal (f 1 0) (Just 1)
    equal (f 2 0) (Just 2)
    equal (f 1 1) (Just 2)
    equal (f 1 1.5) (Just 2)
    equal (f 2 1) (Just 3)
    equal (f 4 1) Nothing
    equal (f 1 4) Nothing

    equal (f 0 0) (Just 1)
    equal (f 0 1) (Just 1)
    equal (f 0 1.5) (Just 1)
    equal (f 0 5) (Just 4)

    equal (f (-1) 0) Nothing
    equal (f (-1) 1) Nothing
    equal (f (-1) 1.5) (Just 1)
    equal (f (-1) 2) (Just 1)
    equal (f (-2) 2) Nothing
    equal (f (-1) 5) (Just 4)

test_find_before_equal = do
    let f n = TimeStep.find_before_equal n [1..4]
    equal (f 0) Nothing
    equal (f 1) (Just 1)
    equal (f 1.5) (Just 1)
    equal (f 2) (Just 2)
