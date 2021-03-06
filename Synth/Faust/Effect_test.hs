-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Faust.Effect_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V

import qualified Util.Audio.Audio as Audio
import qualified Util.Num as Num
import qualified Synth.Faust.Effect as Effect
import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Lib.AUtil as AUtil

import           Util.Test


test_process :: Test
test_process = do
    let Just (Right patch) = Map.lookup "test-delay" EffectC.patches
    let f = process patch
    io_equal (f [] [[1..8]]) [1..8]
    -- Round up to block size.
    io_equal (f [[1]] [[1..8]]) ([0, 0] ++ [1..8] ++ replicate (8-2) 0)

process :: EffectC.Patch -> [[Audio.Sample]] -> [[Audio.Sample]]
    -> IO [Audio.Sample]
process patch delay input =
    toSamples $ Effect.process config patch Nothing (const (return ()))
        (Map.singleton "delay" (Audio.fromSampleLists delay))
        (Audio.fromSampleLists input)

config :: Effect.Config
config = Effect.Config
    { _blockSize = 4
    , _controlSize = 4 `Num.assertDiv` controlsPerBlock
    , _controlsPerBlock = controlsPerBlock
    , _maxDecay = 8
    }
    where controlsPerBlock = 2

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap V.toList) . Resource.runResourceT
    . Audio.toSamples . Audio.take 500 -- protect against infinite stream
