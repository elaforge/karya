module Cmd.Lang.LRuler_test where
import Util.Control
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.Lang.LRuler as LRuler
import qualified Cmd.MakeRuler as MakeRuler

import Types


test_extract = do
    let ((bid, tid), ui_state) = UiTest.run State.empty $ do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1", [])
                , ("b2", [])
                ]
            Create.new_ruler top "r.top" $ MakeRuler.empty_ruler
            Create.new_ruler b1 "r.b1" $ MakeRuler.ruler
                [MakeRuler.meter_ruler 1 $ MakeRuler.repeat 4 (MakeRuler.T 1)]
            Create.new_ruler b2 "r.b2" $ MakeRuler.ruler
                [MakeRuler.meter_ruler 1 $ MakeRuler.repeat 3 (MakeRuler.T 1)]
            return (top, UiTest.mk_tid_block top 0)
    let extract = e_ruler bid
    equal (extract ui_state) Nothing
    let res = CmdTest.run_ui ui_state $ LRuler.extract_from bid tid
    equal (extract (CmdTest.result_ui_state res)) $
        Just [(0, 0), (2.5, 1), (5, 1), (7.5, 1),
            (10, 0), (12, 1), (14, 1), (16, 0)]

e_ruler :: BlockId -> State.State -> Maybe [(ScoreTime, Int)]
e_ruler bid ustate = UiTest.eval ustate $ do
    ruler <- State.get_ruler =<< State.ruler_of bid
    return $ map (second Ruler.mark_rank) . Ruler.marks_of <$>
        Ruler.get_marklist "meter" ruler
