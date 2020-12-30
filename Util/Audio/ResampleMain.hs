-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | This is a simple utility to resample audio files.  It keeps the format and
-- channels the same.
module Util.Audio.ResampleMain where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.QSem as QSem
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource

import qualified GHC.TypeLits as TypeLits
import qualified Sound.File.Sndfile as Sndfile
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import qualified Util.Seq as Seq

import           Global


main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage (unlines errs)
    let srate = fromMaybe defaultSRate $ Seq.last [sr | SRate sr <- flags]
    depth <- traverse parseDepth $ Seq.last [s | Depth s <- flags]
    let write input output = process (Set `elem` flags) depth srate input output
    case args of
        _ | Info `elem` flags -> forM_ args $ \input -> do
            info <- File.getInfo input
            putStrLn $ input <> ": " <> maybe "no file" show info
        _ | InPlace `elem` flags -> for_ 4 args $ \input -> do
            putStrLn input
            -- safe because File.write writes to a tmp file and renames
            write input input
        [input, output] -> write input output
        _ -> usage ""
    where
    usage msg = do
        unless (null msg) $ putStrLn $ "Error: " <> msg
        putStr $ GetOpt.usageInfo "usage: resample [ flags ] input [ output ]"
            options
        Exit.exitFailure

for_ :: Int -> [a] -> (a -> IO ()) -> IO ()
for_ cpus xs f = do
    sem <- QSem.newQSem cpus
    Async.forConcurrently_ xs $ \x ->
        Exception.bracket_ (QSem.waitQSem sem) (QSem.signalQSem sem) (f x)

defaultSRate :: Int
defaultSRate = 44100

data Flag = Set | InPlace | SRate Int | Depth String | Info
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["srate"]
        (GetOpt.ReqArg (SRate . read) (show defaultSRate)) "sampling rate"
    , GetOpt.Option [] ["depth"] (GetOpt.ReqArg Depth "n") "bit depth"
    , GetOpt.Option [] ["set"] (GetOpt.NoArg Set)
        "set srate, instead of resampling"
    , GetOpt.Option [] ["in-place"] (GetOpt.NoArg InPlace)
        "modify file in place"
    , GetOpt.Option [] ["info"] (GetOpt.NoArg Info) "just print info"
    ]

parseDepth :: String -> IO Sndfile.SampleFormat
parseDepth s =
    maybe (errorIO $ "invalid depth: " <> showt s) return $ parseDepth_ s

parseDepth_ :: String -> Maybe Sndfile.SampleFormat
parseDepth_ = \case
    "8" -> Just Sndfile.SampleFormatPcmU8
    "16" -> Just Sndfile.SampleFormatPcm16
    "24" -> Just Sndfile.SampleFormatPcm24
    "32" -> Just Sndfile.SampleFormatPcm32
    "float" -> Just Sndfile.SampleFormatFloat
    _ -> Nothing

process :: Bool -> Maybe Sndfile.SampleFormat -> Int -> FilePath -> FilePath
    -> IO ()
process set depth srate input output = case Audio.someNat srate of
    TypeLits.SomeNat (_ :: Proxy outRate) -> File.readUnknown input >>= \case
        (format, Audio.UnknownAudio (audio :: Audio.AudioIO inRate inChan)) ->
            Resource.runResourceT $
            File.write @outRate @inChan outFormat output $ if set
                then Audio.castRate audio
                else Resample.resampleRate Resample.SincBestQuality audio
            where
            outFormat = case depth of
                Nothing -> format
                Just fmt -> format { Sndfile.sampleFormat = fmt }
