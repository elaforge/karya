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
    let ((top, top_rid), state_pre) = UiTest.run Ui.empty $ do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1", [])
                , ("b2", [])
                ]
            top_rid <- Create.new_ruler top "r.top" $
                UiTest.mkruler 4 16 (mkmeter 4)
            Create.new_ruler b1 "r.b1" $ UiTest.mkruler 4 10 (mkmeter 4)
            Create.new_ruler b2 "r.b2" $ UiTest.mkruler 3 6 (mkmeter 3)
            return (top, top_rid)
    let state_post = UiTest.exec state_pre $ do
            meter <- Extract.pull_up top (UiTest.mk_tid_block top 1)
            RulerUtil.set_meter top_rid meter
        -- extract = map (second Mark.mark_rank) . Meter.Make.make_measures
        -- extract (t, m) = (Mark.mark_rank m, Mark.mark_duration m)
    equal (UiTest.e_rulers state_pre)
        [ ("b1", "1 .2 .3 .4 2 .2 .3 .4 3 .2 4")
        , ("b2", "1 .2 .3 2 .2 .3 3")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]
    equal (UiTest.e_rulers state_post)
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
            Create.new_ruler top "r.top" $ UiTest.mkruler 4 16 (mkmeter 4)
            mapM_ (Create.set_block_ruler Ui.no_ruler) [b1, b2]
            return top
    let (top, state_pre) = UiTest.run Ui.empty make
    let state_post = UiTest.exec state_pre $
            Extract.push_down True top (UiTest.mk_tid_block top 1)
    equal (UiTest.e_rulers state_pre)
        [ ("b1", "")
        , ("b2", "")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]
    equal (UiTest.e_rulers state_post)
        [ ("b1", "1 .2 .3 .4 2 .2 .3 .4 3 .2 4")
        -- TODO previously would set Meter.config_start_measure
        -- , (UiTest.bid "b2", ".3 .4 4 .2 .3 .4 5")
        , ("b2", "1 .2 2 .2 .3 .4 3")
        , ("top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]

mkmeter :: Int -> Meter.AbstractMeter
mkmeter n = Meter.repeat n Meter.T
