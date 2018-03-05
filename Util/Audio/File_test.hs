-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
module Util.Audio.File_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified GHC.TypeLits as TypeLits

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File



mix out = write out $ Audio.mix
    [ (0, File.read44k "g1.wav")
    , (11000, File.read44k "g1.wav")
    ]

writeSine = write "sine.wav" $ Audio.sine 44100 440

copy :: FilePath -> FilePath -> IO ()
copy input output = write output $ File.read44k input

write :: TypeLits.KnownNat chan => FilePath -> Audio.AudioIO 44100 chan -> IO ()
write fname = Resource.runResourceT . File.write File.wavFormat fname
