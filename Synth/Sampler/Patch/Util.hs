-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Util where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


-- * convert

articulation :: a -> Common.AttributeMap a -> Attrs.Attributes -> a
articulation deflt attributeMap  =
    maybe deflt snd . (`Common.lookup_attributes` attributeMap)

dynamic :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Note.Note
    -> (dyn, Signal.Y)
dynamic dynamicRange =
    findDynamic dynamicRange . fromMaybe 0 . Note.initial Control.dynamic

-- | Convert to (Dynamic, DistanceFromPrevDynamic)
findDynamic :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Signal.Y
    -> (dyn, Signal.Y)
findDynamic dynamicRange y =
    find 0 (Num.clamp 0 127 (round (y * 127))) rangeDynamics
    where
    find low val ((high, dyn) : rest)
        | null rest || val < high =
            (dyn, Num.normalize (int low) (int high) (int val))
        | otherwise = find high val rest
    find _ _ [] = error "empty rangeDynamics"
    int = fromIntegral
    rangeDynamics = Seq.key_on (snd . dynamicRange) enumAll

type Variation = Int

variation :: Variation -> Note.Note -> Variation
variation variations =
    pick . fromMaybe 0 . Note.initial Control.variation
    where
    pick var = round (var * fromIntegral (variations - 1))

-- * util

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound .. maxBound]

findBelow :: Ord k => (a -> k) -> k -> [a] -> a
findBelow _ _ [] = error "empty list"
findBelow key val (x:xs) = go x xs
    where
    go x0 (x:xs)
        | val < key x = x0
        | otherwise = go x xs
    go x0 [] = x0
