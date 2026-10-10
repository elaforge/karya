-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Serialize_test where
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified Util.Serialize
import           Cmd.Serialize () -- for instances
import qualified Midi.Midi as Midi
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Patch as Patch
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
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
            UiTest.runMkview [("track", [(0, 1, "e0"), (1, 1, "e1")])]
    let run f = (recode (f state), f state)
    let strip c = c
            -- config_allocations is now a cache where the source of truth is
            -- the ky file, so it isn't saved.
            { UiConfig.configAllocations = mempty
            -- And now ky and tscore are in an external file.
            , UiConfig.configKy = ""
            , UiConfig.configTscore = ""
            }
    uncurry rightEqual $ run $ strip . Ui.stateConfig
    uncurry rightEqual $ run Ui.stateViews
    uncurry rightEqual $ run Ui.stateBlocks
    uncurry rightEqual $ run Ui.stateTracks
    uncurry rightEqual $ run Ui.stateRulers
    uncurry rightEqual $
        run (Ruler.getMeter . head . Map.elems . Ui.stateRulers)
    rightEqual (recode Ui.empty) Ui.empty

    -- Performance
    now <- Time.getCurrentTime
    let perf = UiConfig.Performance msgs now "patch-tag"
        msgs = Vector.fromList
            [Midi.WriteMessage (Midi.writeDevice "wdev") 42 msg]
        msg = Midi.ChannelMessage 1 (Midi.NoteOn 2 3)
    rightEqual (recode perf) perf

    let tdest = Block.NoteDestination "key" (UiTest.tid "tid", mempty) mempty
    rightEqual (recode tdest) tdest
    let flags = [minBound .. maxBound] :: [Block.TrackFlag]
    rightEqual (recode flags) flags
    let sel = Sel.Selection 1 2 3 4 Sel.Positive
    rightEqual (recode sel) sel

    let rstyle =
            [ Track.Filled (Just (Track.Control "hi"))
            , Track.Line (Just (Track.Pitch "there"))
            ]
    rightEqual (recode rstyle) rstyle
    let config = Patch.config []
    rightEqual (recode config) config
    rightEqual (recode Lilypond.emptyStaffConfig) Lilypond.emptyStaffConfig

test_negative_zero :: Test
test_negative_zero = do
    -- make sure negative zero is encoded properly
    rightEqual (recode (0 :: ScoreTime)) 0
    rightEqual (recode (-0 :: ScoreTime)) (-0.0)

recode :: Util.Serialize.Serialize a => a -> Either String a
recode = Util.Serialize.decode . Util.Serialize.encode
