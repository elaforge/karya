-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Serialize_test where
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified Util.Serialize
import Util.Test
import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import Cmd.Serialize () -- for instances
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Patch as Patch
import Types


test_serialize = do
    let (_, state) =
            UiTest.run_mkview [("track", [(0, 1, "e0"), (1, 1, "e1")])]
    let run f = (f state, recode (f state))
    uncurry equal $ run Ui.state_config
    uncurry equal $ run Ui.state_views
    uncurry equal $ run Ui.state_blocks
    uncurry equal $ run Ui.state_tracks
    uncurry equal $ run Ui.state_rulers
    equal Ui.empty (recode Ui.empty)

    -- Performance
    now <- Time.getCurrentTime
    let perf = Ui.Performance msgs now "patch-tag"
        msgs = Vector.fromList
            [Midi.WriteMessage (Midi.write_device "wdev") 42 msg]
        msg = Midi.ChannelMessage 1 (Midi.NoteOn 2 3)
    equal perf (recode perf)

    let tdest = Block.NoteDestination (UiTest.tid "tid", mempty) mempty
    equal tdest (recode tdest)
    let flags = [minBound .. maxBound] :: [Block.TrackFlag]
    equal flags (recode flags)
    let sel = Sel.Selection 1 2 3 4 Sel.Positive
    equal sel (recode sel)

    let rstyle =
            [ Track.Filled (Just (Track.Control "hi"))
            , Track.Line (Just (Track.Pitch "there"))
            ]
    equal rstyle (recode rstyle)
    let config = Patch.config mempty []
    equal config (recode config)
    equal Lilypond.empty_staff_config (recode Lilypond.empty_staff_config)

test_negative_zero = do
    -- make sure negative zero is encoded properly
    equal (recode (0 :: ScoreTime)) 0
    equal (recode (-0 :: ScoreTime)) (-0.0)

recode :: Util.Serialize.Serialize a => a -> a
recode = either error id . Util.Serialize.decode . Util.Serialize.encode
