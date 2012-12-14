module Cmd.Edit_test where
import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit

import Types


test_split_events = do
    let run events = CmdTest.e_tracks
            . run_sel [(">", events)] Edit.cmd_split_events 1
    equal (run [(0, 4, "")] 0) $
        Right ([(">", [(0, 4, "")])], [])
    equal (run [(0, 4, "")] 2) $
        Right ([(">", [(0, 2, ""), (2, 2, "")])], [])
    equal (run [(2, 2, "")] 3) $
        Right ([(">", [(2, 1, ""), (3, 1, "")])], [])
    equal (run [(0, 4, "")] 4) $
        Right ([(">", [(0, 4, "")])], [])

    equal (run [(4, -4, "")] 0) $
        Right ([(">", [(4, -4, "")])], [])
    equal (run [(4, -4, "")] 2) $
        Right ([(">", [(2, -2, ""), (4, -2, "")])], [])
    equal (run [(4, -4, "")] 4) $
        Right ([(">", [(4, -4, "")])], [])

run_sel :: [UiTest.TrackSpec] -> Cmd.CmdId a -> TrackNum -> ScoreTime
    -> CmdTest.Result a
run_sel track_specs cmd tracknum pos = CmdTest.run_tracks track_specs $ do
    CmdTest.set_sel tracknum pos tracknum pos
    cmd

test_record_recent = do
    let f note recent = map (second unnote) $ Edit.record_recent
            (Cmd.RecentTransform note True)
            (map (second (flip Cmd.RecentTransform True)) recent)
        unnote (Cmd.RecentNote s _) = s
        unnote (Cmd.RecentTransform s _) = s
    equal (f "a" []) [(1, "a")]
    equal (f "b" [(1, "a")]) [(2, "b"), (1, "a")]
    equal (f "b" [(2, "a")]) [(1, "b"), (2, "a")]
    equal (f "z" [(1, "a"), (2, "b"), (3, "c"), (4, "d")])
        [(4, "z"), (1, "a"), (2, "b"), (3, "c")]
    -- adding an existing one brings it to the front, retains old key
    equal (f "b" [(1, "a"), (4, "b")]) [(4, "b"), (1, "a")]

test_record_recent_replace = do
    -- "similar" recent notes should replace existing ones
    let f = Edit.record_recent
    let note = Cmd.RecentNote
    equal (f (note "a" False) [(1, (note "a" True))])
        [(1, (note "a" False))]
    equal (f (note "a .2" True) [(1, (note "a .5" True))])
        [(1, (note "a .2" True))]
