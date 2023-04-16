-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.LPitch_test where
import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Repl.LPitch as LPitch

import Global


test_change_scale :: Test
test_change_scale = do
    let run scale pitches to_scale = e_pitch $
            CmdTest.run_tracks (pitch_track scale pitches) $ do
                CmdTest.set_sel 1 0 1 0
                ModifyEvents.all_blocks =<< LPitch.change_scale to_scale
    equal (run "twelve" ["4c", "i (4d)"] "twelve-r") $
        Right (("*twelve", ["4s", "i (4r)"]), [])

test_to_relative :: Test
test_to_relative = do
    let run scale pitches diatonic base = e_pitch $
            CmdTest.run_tracks (pitch_track scale pitches) $
                ModifyEvents.all_blocks $ LPitch.to_relative diatonic base
    equal (run "twelve" ["4c", "4d", "5c"] True "4c") $
        Right (("*twelve", ["0", "1", "7"]), [])
    equal (run "twelve" ["4c", "4d", "5c"] False "4c") $
        Right (("*twelve", ["0", "2", "12"]), [])

e_pitch :: CmdTest.Result val -> CmdTest.Extracted (Text, [Text])
e_pitch = CmdTest.extract_ui_state $
    second (map (\(_, _, p) -> p)) . head . UiTest.extract_tracks

pitch_track :: Text -> [Text] -> [UiTest.TrackSpec]
pitch_track scale ps =
    [("*" <> scale, [(n, 0 , p) | (n, p) <- zip (Lists.range_ 0 1) ps])]
