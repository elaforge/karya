-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
module Util.Audio.Resample_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified GHC.TypeLits as TypeLits

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample

import qualified Perform.Signal as Signal


test_me = do
    print 1

t1 = resampleBy "out.wav" [(0, 2), (0.5, 1)]

resampleBy out curve = write out $ Audio.gain 0.5 $ Audio.mix
    [ -- (0, Audio.sine (44100 * 2) 440)
    (0, Resample.resampleBy Resample.SincBestQuality
        (Signal.from_pairs curve) $ Audio.sine (44100 * 2) 440)
    ]
    -- Resample.SincBestQuality

resample out = write out $
    Resample.resample Resample.SincBestQuality 2 $
    File.read44k "g1.wav"

resampleRate out = writeRate out $
    Resample.resampleRate Resample.SincBestQuality $
    File.read44k "g1.wav"
    where
    writeRate :: FilePath -> Audio.AudioIO 22100 2 -> IO ()
    writeRate fname = Resource.runResourceT . File.write File.wavFormat fname

write :: TypeLits.KnownNat chan => FilePath -> Audio.AudioIO 44100 chan -> IO ()
write fname = Resource.runResourceT . File.write File.wavFormat fname
