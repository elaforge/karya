module Cmd.EditUtil_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.EditUtil as EditUtil
import qualified Derive.Scale.Legong as Legong
import qualified Perform.Pitch as Pitch


test_modify_text_key = do
    let f c = EditUtil.modify_text_key [] (EditUtil.Key c)
    equal (f 'c' "a") (Just "ac")
    equal (f ' ' "a") (Just "a ")
    equal (f ' ' "") (Just "")

test_modify_text_note = do
    let f n = EditUtil.modify_text_note (Pitch.Note n)
    equal (f "abc" "") (Just "(abc)")
    equal (f "abc" "a") (Just "a (abc)")

test_backspace = do
    let f = EditUtil.backspace
    equal (f "") Nothing
    equal (f "a") (Just "")

test_parse_key = do
    let run cmd = CmdTest.extract id $ CmdTest.run_sel 1 note_tracks cmd
        note_tracks = [(">i", [(0, 1, "")]), ("*", [(0, 0, "4d")])]

    -- Fake up a Performance that overrides the default *twelve.
    let set_env = CmdTest.set_scale UiTest.default_block_id
            UiTest.default_block_id (UiTest.mk_tid 2) Legong.scale_id
    let f = EditUtil.parse_key
    equal (run (f (Pitch.InputKey 60)))
        (Right (Just (Pitch.Note "4c"), []))
    equal (run (set_env >> f (Pitch.InputKey 60)))
        (Right (Just (Pitch.Note "1"), []))
