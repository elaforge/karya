-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.MridangamGlobal_test where
import Prelude hiding ((.))

import Util.Test
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.MridangamGlobal as MridangamGlobal
import Solkattu.MridangamGlobal ((&), o, k, __, (.), su)
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence

import Global


test_merge = do
    let f a = map pretty <$> realize a
    equal (f $ o & k) $ Right ["K"]
    equal (f $ k . o&k) $ Right ["k", "K"]
    equal (f $ (k.k.k) & (o.__.o)) $ Right ["K", "k", "K"]
    equal (f $ (k . su (k.k)) & (o . su (o.__))) $ Right ["K", "K", "k"]
    throws (f $ (k.k) & (o . su (o.__))) "expected sollu"
    throws (f $ k & k) "requires thoppi & valantalai"
    throws (f $ o & (k.k)) "trailing strokes"

realize :: MridangamGlobal.Sequence
    -> Either Text [Realize.Note Mridangam.Stroke]
realize seq = fmap Sequence.flattenedNotes $ fmap fst $ head $
    Korvai.realize Korvai.mridangam False
        (MridangamGlobal.korvai1 MridangamGlobal.adi seq)