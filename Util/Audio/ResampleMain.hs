-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | This is a simple utility to resample audio files.  It keeps the format and
-- channels the same.
module Util.Audio.ResampleMain where
import qualified Control.Monad.Trans.Resource as Resource
import qualified GHC.TypeLits as TypeLits
import qualified System.Environment as Environment
import System.FilePath ((</>))
import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample

import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    (outDir, srate, fnames) <- case args of
        outDir : (Read.readMaybe -> Just srate) : fnames ->
            return (outDir, srate, fnames)
        _ -> errorIO "usage: resample outDir srate fname fname ..."
    mapM_ (resampleFile outDir srate) fnames

resampleFile :: FilePath -> Int -> FilePath -> IO ()
resampleFile outDir srate input = do
    putStrLn input
    resample srate input (outDir </> input)

resample :: Int -> FilePath -> FilePath -> IO ()
resample srate input output = case Audio.someNat srate of
    TypeLits.SomeNat (_ :: Proxy outRate) -> File.readUnknown input >>= \case
        (format, Audio.UnknownAudio (audio :: Audio.AudioIO inRate inChan)) ->
            Resource.runResourceT $
            File.write @outRate @inChan format output $
            Resample.resampleRate Resample.SincBestQuality audio
