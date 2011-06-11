module Cmd.TimeStep_test where

import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import Cmd.TimeStep
       (TimeStep(..), MarklistMatch(..), MarkMatch(..))
import qualified Cmd.TimeStep as TimeStep


test_show_step = do
    let f = TimeStep.show_step (Just TimeStep.Advance)
    equal (f (Absolute 2)) "+abs:2t"
    equal (f (AbsoluteMark AllMarklists (MatchRank 3 0))) "+mark:r3+0"
    equal (f (AbsoluteMark (NamedMarklists ["a", "b"]) (MatchRank 3 0)))
        "+mark:a,b/r3+0"
    equal (f BlockEnd) "+end"
    equal (f (Merge [BlockEnd, EventEdge]))
        "+end;evt"

test_all_points = do
    let f = TimeStep.all_points mlists events 3
        events = [(2, Event.event "" 1), (4, Event.event "" 1)]
        mlists = [Ruler.marklist "meter"
            [(p, Ruler.null_mark) | p <- [0, 5, 10, 15]]]
    equal (f BlockEnd) [0, 15]
    equal (f EventEdge) [2, 3, 4, 5]
    equal (f (Merge [BlockEnd, EventEdge])) [0, 2, 3, 4, 5, 15]
    equal (f (Merge [BlockEnd, AbsoluteMark AllMarklists (MatchRank 0 0)]))
        [0, 5, 10, 15]
    equal (f (RelativeMark AllMarklists (MatchRank 0 0)))
        [3, 8, 13, 18]
    -- TODO test different mark ranks
