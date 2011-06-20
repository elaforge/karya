module Cmd.Perf_test where
import Util.Test
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Perf as Perf
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


test_note_to_degree = do
    let f = Perf.note_to_degree Twelve.scale_id . Pitch.Note
    let run = CmdTest.extract id . CmdTest.run_tracks []
    equal (run (f "4c")) $ Right (Just (Right 60), [])
    let Right (Just val, []) = run (f "hi")
    left_like val "val call not found: hi"
