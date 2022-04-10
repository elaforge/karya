-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Instrument.ToScore_test where
import qualified Solkattu.Dsl.Solkattu as G
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Tala as Tala

import Util.Test


test_toScore :: Test
test_toScore = do
    equal (run [] mempty G.p5)
        [ (0, 0, "k"), (1/4, 0, "t"), (2/4, 0, "k"), (3/4, 0, "n")
        , (4/4, 0, "o")
        ]
    equal (run [] Format.defaultAbstraction G.p5) [(0, 5/4, "p 5")]
    equal (run [] mempty (G.sarvaD_ 2)) [(0, 8/4, "sarva")]
    equal (run [] Format.defaultAbstraction (G.sarvaD_ 2)) [(0, 8/4, "sarva")]
    equal (run [] mempty (G.sarvaD_ (2+1/4))) [(0, 9/4, "sarva")]
    equal (run [] Format.defaultAbstraction (G.sarvaD_ (2+1/4)))
        [(0, 9/4, "sarva")]

-- TODO wow that's a lot of manual plumbing.

run :: G.StrokeMap Mridangam.Stroke -> Format.Abstraction -> G.Sequence
    -> [ToScore.Event]
run smap abstraction =
    fst . ToScore.fromStrokes ToScore.toScore
    . Format.makeGroupsAbstractScore abstraction
    . realize smap

realize :: G.StrokeMap Mridangam.Stroke -> G.Sequence
    -> [Korvai.Flat Mridangam.Stroke]
realize smap = fst . expect_right . head . Korvai.realize Korvai.IMridangam
    . korvai smap

korvai :: G.StrokeMap Mridangam.Stroke -> G.Sequence -> G.Korvai
korvai smap = G.korvaiS1 Tala.adi_tala (G.makeMridangam smap)
