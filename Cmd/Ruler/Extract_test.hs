-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Extract_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Cmd.Create as Create
import qualified Cmd.Ruler.Extract as Extract
import qualified Cmd.Ruler.Meter as Meter
import           Cmd.TestInstances ()

import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_pull_up = do
    let meter_ruler = Meter.fit_ruler Meter.default_config
    let (top, ui_state) = UiTest.run Ui.empty $ do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1", [])
                , ("b2", [])
                ]
            Create.new_ruler top "r.top" $ meter_ruler 16 []
            Create.new_ruler b1 "r.b1" $ meter_ruler 16 [Meter.repeat 4 Meter.T]
            Create.new_ruler b2 "r.b2" $ meter_ruler 16 [Meter.repeat 3 Meter.T]
            return top
    let run = extract $ UiTest.eval ui_state $
            Extract.pull_up top (UiTest.mk_tid_block top 1)
        extract = map $ \m -> (Meter.m_rank m, Meter.m_duration m)
    equal (e_ruler top ui_state) "1"
    equal run
        [ (0, 2.5), (1, 2.5), (1, 2.5), (1, 2.5)
        , (0, 2), (1, 2), (1, 2)
        , (0, 0)
        ]

test_push_down = do
    let make = do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1=ruler", [(">", [(0, 10, "")])])
                , ("b2=ruler", [(">", [(0, 6, "")])])
                ]
            Create.new_ruler top "r.top" $
                meter_ruler 16 [Meter.repeat 4 (Meter.repeat 4 Meter.T)]
            mapM_ (Create.set_block_ruler Ui.no_ruler) [b1, b2]
            return top
        meter_ruler = Meter.fit_ruler Meter.default_config
    let state = UiTest.exec ui_state $
            Extract.push_down True top (UiTest.mk_tid_block top 1)
            where (top, ui_state) = UiTest.run Ui.empty make
        extract st =
            [(bid, e_ruler bid st) | bid <- Map.keys (Ui.state_blocks st)]
    equal (extract state)
        [ (UiTest.bid "b1", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3")
        , (UiTest.bid "b2", ".3 .4 4 .2 .3 .4 5")
        , (UiTest.bid "top", "1 .2 .3 .4 2 .2 .3 .4 3 .2 .3 .4 4 .2 .3 .4 5")
        ]
    let b2_ruler = UiTest.eval state $
            Ui.get_ruler =<< Ui.ruler_of (UiTest.bid "b2")
    equal (Ruler.config_start_measure $ fst $ Ruler.get_meter b2_ruler) 3

e_ruler :: BlockId -> Ui.State -> Text
e_ruler bid ustate = UiTest.eval ustate $
    extract . Meter.ruler_meter <$> (Ui.get_ruler =<< Ui.ruler_of bid)
    where extract = Text.unwords . map (Meter.strip_markup . Meter.m_label)
