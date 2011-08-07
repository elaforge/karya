module Cmd.Edit_test where
import Util.Control
import Util.Test
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit


test_record_recent = do
    let f note recent = map (second unnote) $ Edit.record_recent
            (Cmd.RecentTransform note)
            (map (second Cmd.RecentTransform) recent)
        unnote (Cmd.RecentNote s _) = s
        unnote (Cmd.RecentTransform s) = s
    equal (f "a" []) [(1, "a")]
    equal (f "b" [(1, "a")]) [(2, "b"), (1, "a")]
    equal (f "b" [(2, "a")]) [(1, "b"), (2, "a")]
    equal (f "z" [(1, "a"), (2, "b"), (3, "c"), (4, "d")])
        [(4, "z"), (1, "a"), (2, "b"), (3, "c")]
    -- adding an existing one brings it to the front, retains old key
    equal (f "b" [(1, "a"), (4, "b")]) [(4, "b"), (1, "a")]
