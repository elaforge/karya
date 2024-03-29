-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Util.Audio.Audio_test where
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified GHC.TypeLits as TypeLits
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Lists as Lists

import           Global
import           Util.Test


test_mix :: Test
test_mix = do
    let f = concat . toSamples . mixOffset . map (second fromSamples)
    equal (f []) []
    equal (f [(0, [])]) []
    equal (f [(0, [[1]])]) [1]
    equal (f [(1, [[1]])]) [0, 1]
    equal (f [(0, [[1, 2]]), (0, [[3]])]) [4, 2]
    equal (f [(0, [[1, 2]]), (1, [[3]])]) [1, 5]
    equal (f [(0, [[1, 2]]), (4, [[3]])]) [1, 2, 0, 0, 3]
    equal (f [(2, [[1, 2]]), (2, [[3]])]) [0, 0, 4, 2]

    -- Deal with empty blocks.
    equal (f [(0, [[1], [], [3]]), (0, [[], [2], []])]) [3, 3]

test_mix2 :: Test
test_mix2 = do
    let f = concat . toSamples . mixOffset . map (second fromSamples2)
    equal (f [(0, [[0, 1], [2, 3]]), (1, [[4, 5]])])
        [0, 1, 2+4, 3+5]
    equal (f [(0, [[0, 1, 2, 3]]), (1, [[4, 5]])])
        [0, 1, 2+4, 3+5]

mixOffset :: TypeLits.KnownNat chan
    => [(Audio.Frames, Audio.AudioId rate chan)] -> Audio.AudioId rate chan
mixOffset = Audio.mix . map (\(f, audio) -> Audio.take f Audio.silence <> audio)

test_monoid :: Test
test_monoid = do
    equal (toSamples mempty) []
    equal (toSamples (mempty <> mempty)) []
    equal (toSamples (mempty <> fromSamples [[2]])) [[2]]
    equal (toSamples (fromSamples [[1]] <> fromSamples [[2]])) [[1], [2]]
    equal (toSamples (fromSamples [[1], [2]] <> fromSamples [[3, 4]]))
        [[1], [2], [3, 4]]

test_nonInterleaved :: Test
test_nonInterleaved = do
    let f = map (map (V.toList . Audio.blockVector)) . unstream . Audio._nstream
            . Audio.nonInterleaved 0 2 . map fromSamples
    equal (f []) []
    equal (f [[[1, 2, 3, 4]], [[5], [6], [7, 8]]])
        [[[1, 2], [5, 6]], [[3, 4], [7, 8]]]
    equal (f [[[1]], [[5, 6, 7, 8]]])
        [[[1], [5, 6]], [[], [7, 8]]]

test_interleaved :: Test
test_interleaved = do
    equal (interleaved (Proxy @3) [[1, 2], [3, 4]]) $
        Left "can't convert 2 channels to 3"
    equal (interleaved (Proxy @3) [[1, 2]]) $ Right [1, 1, 1, 2, 2, 2]
    equal (interleaved (Proxy @2) [[1, 2], [3, 4]]) $ Right [1, 3, 2, 4]

interleaved :: forall outChan. (TypeLits.KnownNat outChan)
    => Proxy outChan -> [[Audio.Sample]] -> Either Text [Audio.Sample]
interleaved Proxy = fmap (concat . toSamples @10 @outChan)
    . Audio.interleaved . Audio.nonInterleaved 0 Audio.blockSize
    . map fromSamples . map (:[])

test_synchronizeToSize :: Test
test_synchronizeToSize = do
    let f now = toSamples . Audio.synchronizeToSize now 3 . fromSamples
    equal (f 1 []) []
    equal (f 1 [[1]]) [[1]]
    equal (f 1 [[1, 2, 3]]) [[1, 2], [3]]
    equal (f 2 [[1, 2, 3]]) [[1], [2, 3]]
    equal (f 3 [[1, 2, 3, 4]]) [[1, 2, 3], [4]]

test_gain :: Test
test_gain = do
    let f n = concat . toSamples . Audio.gain n . fromSamples
    equal (f 0.5 [[1, 2], [3]]) [0.5, 1, 1.5]

test_multiply :: Test
test_multiply = do
    let f a1 a2 = concat $ toSamples $
            Audio.multiply (fromSamples a1) (fromSamples a2)
    equal (f [] []) []
    equal (f [[2]] [[4, 5, 6]]) [2*4]
    equal (f [[2], [3]] [[4, 5, 6]]) [2*4, 3*5]

test_multiply2 :: Test
test_multiply2 = do
    let f a1 a2 = concat $ toSamples $
            Audio.multiply (fromSamples2 a1) (fromSamples2 a2)
    equal (f [] []) []
    equal (f [[2, 3], [4, 5]] [[4, 5, 6, 7]]) [2*4, 3*5, 4*6, 5*7]

test_mergeChannels :: Test
test_mergeChannels = do
    let f a1 a2 = toSamples $ Audio.mergeChannels a1 a2
    equal (f (fromSamples []) (fromSamples [])) []
    equal (f (fromSamples [[1, 3]]) (fromSamples [[2], [4]])) [[1, 2], [3, 4]]

test_expandChannels :: Test
test_expandChannels = do
    let f :: TypeLits.KnownNat chan => [[Audio.Sample]] -> Audio.AudioId 10 chan
        f = Audio.expandChannels . fromSamples
    equal (toSamples @10 @1 $ f []) []
    equal (toSamples @10 @1 $ f [[1], [2]]) [[1], [2]]
    equal (toSamples @10 @2 $ f [[1], [2]]) [[1, 1], [2, 2]]
    equal (toSamples @10 @2 $ f [[1, 2]]) [[1, 1, 2, 2]]
    equal (toSamples @10 @3 $ f [[1], [2]]) [[1, 1, 1], [2, 2, 2]]

test_mixChannels :: Test
test_mixChannels = do
    let f :: TypeLits.KnownNat chan => Audio.AudioId 10 chan -> [[Audio.Sample]]
        f = toSamples . Audio.mixChannels
    equal (f $ fromSamples [[1], [2]]) [[1], [2]]
    equal (f $ fromSamples2 [[1, 2], [3, 4]]) [[3], [7]]

test_synchronize :: Test
test_synchronize = do
    let f a1 a2 = map (bimap (fmap unblock) (fmap unblock)) $ unstream $
            Audio.synchronize a1 a2
        unblock = V.toList . Audio.blockVector
    equal (f (fromSamples []) (fromSamples [])) []
    equal (f (fromSamples [[1]]) (fromSamples [])) [(Just [1], Nothing)]
    equal (f (fromSamples [[1]]) (fromSamples [[2, 3]]))
        [(Just [1], Just [2]), (Nothing, Just [3])]
    equal (f (fromSamples [[1]]) (fromSamples [[2], [], [3]]))
        [(Just [1], Just [2]), (Nothing, Just []), (Nothing, Just [3])]
    equal (f (fromSamples [[1]]) (fromSamples2 [[2, 3]]))
        [(Just [1], Just [2, 3])]

test_linear :: Test
test_linear = do
    let f wanted = concat . toSamples @1 @1 . Audio.take wanted
            . Audio.linear True
    equal (f 2 []) [0, 0]
    equal (f 7 [(0, 4), (4, 0)]) [4, 3, 2, 1, 0, 0, 0]
    -- Implicit leading 0.
    equal (f 7 [(2, 4), (4, 0)]) [0, 0, 4, 2, 0, 0, 0]
    -- Discontinuity.
    equal (f 6 [(0, 0), (2, 0), (2, 1), (3, 1), (3, 0)]) [0, 0, 1, 0, 0, 0]
    equal (f 6 [(0, 2), (2, 0), (2, 3), (5, 0)]) [2, 1, 3, 2, 1, 0]
    -- Infinite final sample.
    equal (f 7 [(0, 0), (4, 4)]) [0, 1, 2, 3, 4, 4, 4]

test_linear_not_forever :: Test
test_linear_not_forever = do
    let f wanted = concat . toSamples @1 @1 . Audio.take wanted
            . Audio.linear False
    equal (f 2 []) [0]
    equal (f 2 [(0, 1)]) [1]
    equal (f 2 [(0, 1), (1, 1)]) [1, 1]
    equal (f 6 [(0, 0), (0, 1), (2, 0)]) [1, 0.5, 0]

_test_linear_big = do
    let f wanted = toSamples @44100 @1 . Audio.takeS wanted
            . Audio.synchronizeToSize 0 Audio.blockSize
            . Audio.linear True
    let blocks = f 0.55
            [ (0, 0), (0, 1), (0.25, 1), (0.25, 0), (0.5, 0)
            , (0.5, 1), (0.75, 1), (0.75, 0)
            ]
    mapM_ (putStrLn . untxt) $ snd $ List.mapAccumL annotate 0 blocks
    where
    annotate frame block = (frame + len,) $ Text.unwords
        [ showt frame, showt len
        , pretty (fromIntegral frame / 44100 :: Double)
        , pretty (take 4 block), "...", pretty (Lists.takeEnd 4 block)
        ]
        where len = length block

unstream :: S.Stream (S.Of a) Identity.Identity () -> [a]
unstream = Identity.runIdentity . S.toList_

test_deinterleaveV :: Test
test_deinterleaveV = do
    let f chans = map V.toList . Audio.deinterleaveV chans . V.fromList @Float
    equal (f 1 [1, 2, 3, 4]) [[1, 2, 3, 4]]
    equal (f 2 [1, 2, 3, 4]) [[1, 3], [2, 4]]
    equal (f 3 [1, 2, 3, 4]) [[1], [2], [3]]

test_interleaveV :: Test
test_interleaveV = do
    let f = V.toList . Audio.interleaveV . map (V.fromList @Float)
    equal (f [[1, 2, 3, 4]]) [1, 2, 3, 4]
    equal (f [[1, 3], [2, 4]]) [1, 2, 3, 4]
    equal (f [[1, 3], [2, 4], [5]]) [1, 2, 5, 3, 4]

fromSamples :: Monad m => [[Audio.Sample]] -> Audio.Audio m 10 1
fromSamples = Audio.fromSamples . map V.fromList

fromSamples2 :: [[Audio.Sample]] -> Audio.AudioId 10 2
fromSamples2 = Audio.fromSamples . map V.fromList

fromSamplesN :: TypeLits.KnownNat chan => [[Audio.Sample]]
    -> Audio.AudioId 10 chan
fromSamplesN = Audio.fromSamples . map V.fromList

toSamples :: Audio.AudioId rate channels -> [[Audio.Sample]]
toSamples = map V.toList . Identity.runIdentity . Audio.toSamples


-- * util

test_breakAfter :: Test
test_breakAfter = do
    let f check = extract . Audio.breakAfter (+) 0 check . S.each
        extract xs = Identity.runIdentity $ do
            as S.:> bs <- S.toList xs
            return (as, Identity.runIdentity $ S.toList_ bs)
    equal (f (>=5) [5, 5]) ([5], [5])
    equal (f (>=5) [3, 2, 3]) ([3, 2], [3])
