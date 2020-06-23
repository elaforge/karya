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
import qualified Data.List as List
import qualified Data.Text as Text
import qualified GHC.TypeLits as TypeLits
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample

import           Global


main :: IO ()
main = do
    (flags, args) <- List.partition ("--" `List.isPrefixOf`) <$>
        Environment.getArgs
    (srate, input, output) <- case args of
        [Read.readMaybe -> Just srate, input, output] ->
            return (srate, input, output)
        _ -> errorIO "usage: resample [ --set ] srate input output"
    let replace resample = do
            process resample srate input (output <> ".tmp")
            Directory.renameFile (output <> ".tmp") output
    case flags of
        [] -> replace True
        ["--set"] -> replace False
        _ -> errorIO $ "unknown flags: " <> Text.unwords (map txt flags)

process :: Bool -> Int -> FilePath -> FilePath -> IO ()
process resample srate input output = case Audio.someNat srate of
    TypeLits.SomeNat (_ :: Proxy outRate) -> File.readUnknown input >>= \case
        (format, Audio.UnknownAudio (audio :: Audio.AudioIO inRate inChan)) ->
            Resource.runResourceT $
            File.write @outRate @inChan format output $ if resample
                then Resample.resampleRate Resample.SincBestQuality audio
                else Audio.castRate audio
