module Cmd.Perf_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Perf as Perf

import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


test_note_to_pitch = do
    let f = Perf.note_to_pitch Twelve.scale_id UiTest.default_block_id
            (UiTest.mk_tid 1) 1 . Pitch.Note
    let run = CmdTest.extract id . CmdTest.run_tracks []
    equal (run (f "4c")) $ Right (Just (Right 60), [])
    -- TODO test a fancier scale that retunes?

run :: Cmd.CmdId val -> Either String (Maybe val, [String])
run = CmdTest.extract id . CmdTest.run_tracks []
