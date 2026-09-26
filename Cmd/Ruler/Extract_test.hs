-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Extract_test where
import qualified Cmd.Create as Create
import qualified Cmd.Ruler.Extract as Extract
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import           Cmd.TestInstances ()

import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Util.Test


test_pull_up :: Test
test_pull_up = do
    let ((top, topRid), statePre) = UiTest.run Ui.empty $ do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1", [])
                , ("b2", [])
                ]
            topRid <- Create.newRuler top "r.top" $
                UiTest.mkruler 4 16 (mkmeter 4)
            Create.newRuler b1 "r.b1" $ UiTest.mkruler 4 10 (mkmeter 4)
            Create.newRuler b2 "r.b2" $ UiTest.mkruler 3 6 (mkmeter 3)
            pure (top, topRid)
    let statePost = UiTest.exec statePre $ do
            meter <- Extract.pullUp top (UiTest.mkTidBlock top 1)
            RulerUtil.setMeter topRid meter
        -- extract = map (second Mark.mark_rank) . Meter.Make.make_measures
        -- extract (t, m) = (Mark.mark_rank m, Mark.mark_duration m)
    equal (UiTest.eRulers statePre)
        [ ("b1", "1 .2 .3 .4 2 .2 .3 .4 3 .2 4")
        , ("b2", "1 .2 .3 2 .2 .3 3")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]
    equal (UiTest.eRulers statePost)
        [ ("b1",  "1 .2 .3 .4 2 .2 .3 .4 3 .2 4")
        , ("b2",  "1 .2 .3 2 .2 .3 3")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 4 .2 .3 5 .2 .3 6")
        ]

test_push_down :: Test
test_push_down = do
    let make = do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1=ruler", [(">", [(0, 10, "")])])
                , ("b2=ruler", [(">", [(0, 6, "")])])
                ]
            Create.newRuler top "r.top" $ UiTest.mkruler 4 16 (mkmeter 4)
            mapM_ (Create.setBlockRuler Ui.noRuler) [b1, b2]
            pure top
    let (top, statePre) = UiTest.run Ui.empty make
    let statePost = UiTest.exec statePre $
            Extract.pushDown True top (UiTest.mkTidBlock top 1)
    equal (UiTest.eRulers statePre)
        [ ("b1", "")
        , ("b2", "")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]
    equal (UiTest.eRulers statePost)
        [ ("b1", "1 .2 .3 .4 2 .2 .3 .4 3 .2 4")
        -- TODO previously would set Meter.config_start_measure
        -- , (UiTest.bid "b2", ".3 .4 4 .2 .3 .4 5")
        , ("b2", "1 .2 2 .2 .3 .4 3")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]

mkmeter :: Int -> Meter.AbstractMeter
mkmeter n = Meter.repeat n Meter.T
