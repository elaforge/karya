-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Play_test where
import qualified Instrument.InstTypes as InstTypes
import qualified Solkattu.Dsl.Mridangam as M
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Play as Play
import qualified Solkattu.Tala as Tala

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import Global
import Util.Test


test_realize = do
    equal (extract <$> realize (M.k <> M.t <> M.__ <> M.d)) $
        Right [(">", [(0, 0, "k"), (0.25, 0, "t"), (0.75, 0, "d")])]

extract :: Ui.State -> [UiTest.TrackSpec]
extract = snd . fst . snd . head . UiTest.dump_blocks

realize :: M.Sequence -> Either Text Ui.State
realize =
    Play.realize Korvai.mridangam
        (InstTypes.Qualified "sampler" "mridangam-d") "" 1
    . M.korvaiS1 tala4

tala4 :: Tala.Tala
tala4 = Tala.Tala "tala4" [Tala.O, Tala.O] 0
