module Cmd.Repl.LPitch_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Repl.LPitch as LPitch


test_change_scale = do
    let run scale pitches to_scale = e_pitch $
            CmdTest.run_tracks (pitch_track scale pitches) $
                ModifyEvents.all_blocks =<< LPitch.change_scale to_scale
    equal (run "twelve" ["4c", "i (4d)"] "twelve-r") $
        Right (("*twelve", ["4s", "i (4r)"]), [])

test_to_relative = do
    let run scale pitches diatonic base = e_pitch $
            CmdTest.run_tracks (pitch_track scale pitches) $
                ModifyEvents.all_blocks $ LPitch.to_relative diatonic base
    equal (run "twelve" ["4c", "4d", "5c"] True "4c") $
        Right (("*twelve", ["0", "1", "7"]), [])
    equal (run "twelve" ["4c", "4d", "5c"] False "4c") $
        Right (("*twelve", ["0", "2", "12"]), [])

e_pitch :: CmdTest.Result val -> CmdTest.Extracted (String, [String])
e_pitch = CmdTest.extract_ui_state $
    second (map (\(_, _, p) -> p)) . head . UiTest.extract_tracks

pitch_track :: String -> [String] -> [UiTest.TrackSpec]
pitch_track scale ps =
    [("*" <> scale, [(n, 0 , p) | (n, p) <- zip (Seq.range_ 0 1) ps])]
