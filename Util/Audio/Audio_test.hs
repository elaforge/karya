-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Util.Audio.Audio_test where
import qualified Control.Monad.Identity as Identity
import qualified Data.Vector.Storable as V
import qualified Streaming.Prelude as S

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

    -- Deal with empty chunks.
    equal (f [(0, [[1], [], [3]]), (0, [[], [2], []])]) [3, 3]

test_mix2 = do
    let f = concat . toSamples . Audio.mix . map (second fromSamples2)
    equal (f [(0, [[0, 1], [2, 3]]), (1, [[4, 5]])])
        [0, 1, 2+4, 3+5]
    equal (f [(0, [[0, 1, 2, 3]]), (1, [[4, 5]])])
        [0, 1, 2+4, 3+5]

test_gain = do
    let f n = concat . toSamples . Audio.gain n . fromSamples
    equal (f 0.5 [[1, 2], [3]]) [0.5, 1, 1.5]

test_mergeChannels = do
    let f a1 a2 = toSamples $ Audio.mergeChannels a1 a2
    equal (f (fromSamples []) (fromSamples [])) []
    equal (f (fromSamples [[1, 3]]) (fromSamples [[2], [4]])) [[1, 2], [3, 4]]

test_synchronize = do
    let f a1 a2 = map (fmap V.toList *** fmap V.toList) $ unstream $
            Audio.synchronize a1 a2
    equal (f (fromSamples []) (fromSamples [])) []
    equal (f (fromSamples [[1]]) (fromSamples [])) [(Just [1], Nothing)]
    equal (f (fromSamples [[1]]) (fromSamples [[2, 3]]))
        [(Just [1], Just [2]), (Nothing, Just [3])]
    equal (f (fromSamples [[1]]) (fromSamples [[2], [], [3]]))
        [(Just [1], Just [2]), (Nothing, Just []), (Nothing, Just [3])]
    equal (f (fromSamples [[1]]) (fromSamples2 [[2, 3]]))
        [(Just [1], Just [2, 3])]

test_synchronizeBy = do
    let f bps = toSamples . Audio.synchronizeBy bps . fromSamples2
    equal (f [] [[1, 2], [3, 4]]) [[1, 2], [3, 4]]
    equal (f [0, 1] [[1, 2], [3, 4]]) [[1, 2], [3, 4]]
    equal (f [1, 2] [[1, 2, 3, 4]]) [[1, 2], [3, 4]]

unstream :: S.Stream (S.Of a) Identity.Identity () -> [a]
unstream = Identity.runIdentity . S.toList_

test_deinterleave = do
    let f chans = map V.toList . Audio.deinterleave chans . V.fromList @Float
    equal (f 1 [1, 2, 3, 4]) [[1, 2, 3, 4]]
    equal (f 2 [1, 2, 3, 4]) [[1, 3], [2, 4]]
    equal (f 3 [1, 2, 3, 4]) [[1], [2], [3]]

test_interleave = do
    let f = V.toList . Audio.interleave . map (V.fromList @Float)
    equal (f [[1, 2, 3, 4]]) [1, 2, 3, 4]
    equal (f [[1, 3], [2, 4]]) [1, 2, 3, 4]
    equal (f [[1, 3], [2, 4], [5]]) [1, 2, 5, 3, 4]


fromSamples :: [[Audio.Sample]] -> Audio.AudioId 10 1
fromSamples = Audio.fromSamples . map V.fromList

fromSamples2 :: [[Audio.Sample]] -> Audio.AudioId 10 2
fromSamples2 = Audio.fromSamples . map V.fromList

toSamples :: Audio.AudioId rate channels -> [[Audio.Sample]]
toSamples = map V.toList . Identity.runIdentity . Audio.toSamples
