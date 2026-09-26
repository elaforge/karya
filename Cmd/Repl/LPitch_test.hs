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
    let run scale pitches toScale = ePitch $
            CmdTest.runTracks (pitchTrack scale pitches) $ do
                CmdTest.setSel 1 0 1 0
                ModifyEvents.allBlocks =<< LPitch.changeScale toScale
    equal (run "twelve" ["4c", "i (4d)"] "twelve-r") $
        Right (("*twelve", ["4s", "i (4r)"]), [])

test_to_relative :: Test
test_to_relative = do
    let run scale pitches diatonic base = ePitch $
            CmdTest.runTracks (pitchTrack scale pitches) $
                ModifyEvents.allBlocks $ LPitch.toRelative diatonic base
    equal (run "twelve" ["4c", "4d", "5c"] True "4c") $
        Right (("*twelve", ["0", "1", "7"]), [])
    equal (run "twelve" ["4c", "4d", "5c"] False "4c") $
        Right (("*twelve", ["0", "2", "12"]), [])

ePitch :: CmdTest.Result val -> CmdTest.Extracted (Text, [Text])
ePitch = CmdTest.extractUiState $
    second (map (\(_, _, p) -> p)) . head . UiTest.extractTracks

pitchTrack :: Text -> [Text] -> [UiTest.TrackSpec]
pitchTrack scale ps =
    [("*" <> scale, [(n, 0 , p) | (n, p) <- zip (Lists.range_ 0 1) ps])]
