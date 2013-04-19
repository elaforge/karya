module Cmd.Perf_test where
import Util.Test
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Perf as Perf

import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


test_note_to_nn = do
    let f = Perf.note_to_nn Twelve.scale_id . Pitch.Note
    let run = CmdTest.extract id . CmdTest.run_tracks []
    equal (run (f "4c")) $ Right (Just (Right 60), [])
    equal (run (f "4q")) $ Right (Just (Left "no call for Note \"4q\""), [])

run :: Cmd.CmdId val -> Either String (Maybe val, [String])
run = CmdTest.extract id . CmdTest.run_tracks []
