-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Instrument.ToScore_test where
import qualified Solkattu.Dsl as Dsl
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.SolkattuGlobal as SolkattuGlobal
import qualified Solkattu.Tala as Tala

import Util.Test


test_toScore = do
    equal (run [] mempty Dsl.p5)
        [ (0, 0, "k"), (1/4, 0, "t"), (2/4, 0, "k"), (3/4, 0, "n")
        , (4/4, 0, "o")
        ]
    equal (run [] Format.defaultAbstraction Dsl.p5) [(0, 5/4, "p 5")]
    equal (run [] mempty (Dsl.sarvaD_ 2)) [(0, 8/4, "sarva")]
    equal (run [] Format.defaultAbstraction (Dsl.sarvaD_ 2)) [(0, 8/4, "sarva")]
    equal (run [] mempty (Dsl.sarvaD_ (2+1/4))) [(0, 9/4, "sarva")]
    equal (run [] Format.defaultAbstraction (Dsl.sarvaD_ (2+1/4)))
        [(0, 9/4, "sarva")]

-- TODO wow that's a lot of manual plumbing.

run :: SolkattuGlobal.StrokeMap Mridangam.Stroke
    -> Format.Abstraction -> SolkattuGlobal.Sequence
    -> [ToScore.Event]
run smap abstraction =
    fst . ToScore.fromStrokes ToScore.toScore
    . Format.makeGroupsAbstractScore abstraction
    . realize smap

realize :: SolkattuGlobal.StrokeMap Mridangam.Stroke
    -> SolkattuGlobal.Sequence -> [Korvai.Flat Mridangam.Stroke]
realize smap = fst . expect_right . head . Korvai.realize Korvai.mridangam
    . korvai smap

korvai :: SolkattuGlobal.StrokeMap Mridangam.Stroke
    -> SolkattuGlobal.Sequence -> SolkattuGlobal.Korvai
korvai smap = SolkattuGlobal.korvaiS1 Tala.adi_tala
    (SolkattuGlobal.makeMridangam smap)
