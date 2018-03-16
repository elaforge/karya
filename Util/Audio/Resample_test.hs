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


test_compile_me = do
    print 1

{- It's hard to test these automatically, so I tested by ear.
    Test for each of:

    [ test
    -- sine for pitch accuracy, file for continuity
    | source <- [Sine 2, File "test.wav"]
    , quality <- [Resample.ZeroOrderHold, Resample.SincFastest,
        Resample.SincBestQuality]
    ]
-}
generate = resampleBy (File "test.wav") -- (Sine 2)
    "out.wav" Resample.SincBestQuality

-- Should be 220hz, *2 long.
t_constant = generate [(0, 2)]

-- Should reach 440hz at 0.5s, > *1 long.
t_linear = generate [(0, 2), (0.5, 1)]

-- Should go 880 to 440 to 880 at breakpoints.
t_change_direction = generate [(0, 0.5), (0.5, 1), (1, 0.5)]

-- Shoud go to 440 at 0.5 and jump to 220.
t_discontinuity = generate [(0, 2), (0.5, 1), (0.5, 2), (1, 1)]

data Source = Sine Double | File FilePath
    deriving (Show)

resampleBy :: Source -> FilePath -> Resample.ConverterType
    -> [(Signal.X, Signal.Y)] -> IO ()
resampleBy source out quality curve = write out $ Audio.gain 0.5 $ Audio.mix $
    -- (Audio.Frames 0, takes 2 $ Audio.sine 440) :
    (Audio.Frames 0, Resample.resampleBy quality (Signal.from_pairs curve) $
        case source of
            Sine secs -> takes secs $ Audio.sine 440
            -- Sine secs -> Audio.mergeChannels
            --     (takes secs $ Audio.sine 440)
            --     (takes secs $ Audio.sine 440)
            File fname -> File.read fname)
    : []
    where
    takes = Audio.take . Audio.Seconds

resampleRate out = writeRate out $
    Resample.resampleRate Resample.SincBestQuality $
    File.read44k "test.wav"
    where
    writeRate :: FilePath -> Audio.AudioIO 22100 2 -> IO ()
    writeRate fname = Resource.runResourceT . File.write File.wavFormat fname

write :: TypeLits.KnownNat chan => FilePath -> Audio.AudioIO 44100 chan -> IO ()
write fname = Resource.runResourceT . File.write File.wavFormat fname
