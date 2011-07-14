module Cmd.TimeStep_test where

import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.UiTest as UiTest

import qualified Cmd.TimeStep as TimeStep
import Cmd.TimeStep (Step(..), MarklistMatch(..), EventEdge(..))


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

test_all_points = do
    let f = TimeStep.all_points mlists [events] 3
        events = [(2, Event.event "" 1), (4, Event.event "" 1)]
        mlists = [Ruler.marklist "meter"
            [(p, Ruler.null_mark) | p <- [0, 5, 10, 15]]]
        start = EventStart CurrentTrack
        end = EventEnd CurrentTrack
    equal (f (mk 0 BlockEnd)) [0, 15]
    equal (f (mk 0 start)) [2, 4]
    equal (f (mk 0 end)) [3, 5]
    equal (f (mk 0 (RelativeMark AllMarklists 0))) [3, 8, 13, 18]
    equal (f (merge 0 BlockEnd (mk 0 start))) [0, 2, 4, 15]
    equal (f (merge 0 BlockEnd (mk 0 (AbsoluteMark AllMarklists 0))))
        [0, 5, 10, 15]

    equal (f (merge 0 end (mk 0 start))) [2, 3, 4, 5]
    -- TODO test different mark ranks

test_get_points = do
    let ustate = snd $ UiTest.run_mkstate
            [ (">", [(0, 1, "a"), (2, 1, "b")])
            , ("c", [(0, 0, "1"), (5, 0, "2")])
            ]
    let f step pos = UiTest.eval ustate $
            TimeStep.get_points step UiTest.default_block_id 1 pos
    equal (f (mk 0 (EventStart CurrentTrack)) 0) (Just [0, 2])
    equal (f (mk 0 (EventStart AllTracks)) 0) (Just [0, 2, 5])
    equal (f (merge 0 (EventEnd AllTracks) (mk 0 (EventStart AllTracks))) 0)
        (Just [0, 1, 2, 3, 5])

test_step_from_points = do
    let f n pos = TimeStep.step_from_points n pos [1..4]
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
