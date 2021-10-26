-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Serialize_test where
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified Util.Serialize
import           Cmd.Serialize () -- for instances
import qualified Midi.Midi as Midi
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Patch as Patch
import qualified Ui.Block as Block
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Types
import           Util.Test


test_serialize :: Test
test_serialize = do
    let (_, state) =
            UiTest.run_mkview [("track", [(0, 1, "e0"), (1, 1, "e1")])]
    let run f = (recode (f state), f state)
    uncurry right_equal $ run Ui.state_config
    uncurry right_equal $ run Ui.state_views
    uncurry right_equal $ run Ui.state_blocks
    uncurry right_equal $ run Ui.state_tracks
    uncurry right_equal $ run Ui.state_rulers
    right_equal (recode Ui.empty) Ui.empty

    -- Performance
    now <- Time.getCurrentTime
    let perf = UiConfig.Performance msgs now "patch-tag"
        msgs = Vector.fromList
            [Midi.WriteMessage (Midi.write_device "wdev") 42 msg]
        msg = Midi.ChannelMessage 1 (Midi.NoteOn 2 3)
    right_equal (recode perf) perf

    let tdest = Block.NoteDestination "key" (UiTest.tid "tid", mempty) mempty
    right_equal (recode tdest) tdest
    let flags = [minBound .. maxBound] :: [Block.TrackFlag]
    right_equal (recode flags) flags
    let sel = Sel.Selection 1 2 3 4 Sel.Positive
    right_equal (recode sel) sel

    let rstyle =
            [ Track.Filled (Just (Track.Control "hi"))
            , Track.Line (Just (Track.Pitch "there"))
            ]
    right_equal (recode rstyle) rstyle
    let config = Patch.config []
    right_equal (recode config) config
    right_equal (recode Lilypond.empty_staff_config) Lilypond.empty_staff_config

test_negative_zero :: Test
test_negative_zero = do
    -- make sure negative zero is encoded properly
    right_equal (recode (0 :: ScoreTime)) 0
    right_equal (recode (-0 :: ScoreTime)) (-0.0)

recode :: Util.Serialize.Serialize a => a -> Either String a
recode = Util.Serialize.decode . Util.Serialize.encode
