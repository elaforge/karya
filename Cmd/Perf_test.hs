module Cmd.Perf_test where
import Util.Test
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Perf as Perf

import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


test_note_to_pitch = do
    let f = Perf.note_to_pitch Twelve.scale_id . Pitch.Note
    let run = CmdTest.extract (fmap PitchSignal.pitch_nn)
            . CmdTest.run_tracks []
    pprint (run (f "4c"))
    equal (run (f "4c")) $ Right (Just (Right (Right 60)), [])

run :: Cmd.CmdId val -> Either String (Maybe val, [String])
run = CmdTest.extract id . CmdTest.run_tracks []
