-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
module Util.Audio.File_test where
import qualified Control.Monad.Trans.Resource as Resource

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample


resample out = write out $
    Resample.resample Resample.SincBestQuality 2 $
    File.read44k "g1.wav"

resampleRate out = writeRate out $
    Resample.resampleRate Resample.SincBestQuality $
    File.read44k "g1.wav"
    where
    writeRate :: FilePath -> Audio.AudioIO 22100 2 -> IO ()
    writeRate fname = Resource.runResourceT . File.write File.wavFormat fname

mix out = write out $ Audio.mix
    [ (0, File.read44k "g1.wav")
    , (11000, File.read44k "g1.wav")
    ]

copy :: FilePath -> FilePath -> IO ()
copy input output = write output $ File.read44k input

write :: FilePath -> Audio.AudioIO 44100 2 -> IO ()
write fname = Resource.runResourceT . File.write File.wavFormat fname
