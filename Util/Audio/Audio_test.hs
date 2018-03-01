-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
module Util.Audio.Audio_test where
import qualified Control.Monad.Identity as Identity
import qualified Data.Vector.Storable as V

import qualified Util.Audio.Audio as Audio
import Util.Test
import Global


test_mix = do
    let f = concat . toSamples . Audio.mix . map (second fromSamples)
    equal (f []) []
    equal (f [(0, [])]) []
    equal (f [(0, [[1]])]) [1]
    equal (f [(1, [[1]])]) [0, 1]
    equal (f [(0, [[1, 2]]), (0, [[3]])]) [4, 2]
    equal (f [(0, [[1, 2]]), (1, [[3]])]) [1, 5]
    equal (f [(0, [[1, 2]]), (4, [[3]])]) [1, 2, 0, 0, 3]
    equal (f [(2, [[1, 2]]), (2, [[3]])]) [0, 0, 4, 2]

test_mix2 = do
    let f = concat . toSamples . Audio.mix . map (second fromSamples2)
    equal (f [(0, [[0, 1], [2, 3]]), (1, [[4, 5]])])
        [0, 1, 2+4, 3+5]
    equal (f [(0, [[0, 1, 2, 3]]), (1, [[4, 5]])])
        [0, 1, 2+4, 3+5]

fromSamples :: [[Audio.Sample]] -> Audio.AudioId 10 1
fromSamples = Audio.fromSamples . map V.fromList

fromSamples2 :: [[Audio.Sample]] -> Audio.AudioId 10 2
fromSamples2 = Audio.fromSamples . map V.fromList

toSamples :: Audio.AudioId rate channels -> [[Audio.Sample]]
toSamples = map V.toList . Identity.runIdentity . Audio.toSamples
