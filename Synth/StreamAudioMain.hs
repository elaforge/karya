-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
module Synth.StreamAudioMain (main) where
import qualified Control.Concurrent.Async as Async
import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Text.Read as Read

import qualified Util.Audio.PortAudio as PortAudio
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Perform.RealTime as RealTime
import qualified Synth.StreamAudio as StreamAudio

import           Global


main :: IO ()
main = PortAudio.initialize $ do
    (flags, (dir, (muted, start))) <- parseArgs =<< Environment.getArgs
    defaultDevice <- PortAudio.getDefaultOutput
    when (List `elem` flags) $ do
        devs <- PortAudio.getOutputDevices
        forM_ (map PortAudio._name devs) $ \dev ->
            putStrLn $ (if dev == PortAudio._name defaultDevice
                then "* " else "  ") <> show dev
        putStrLn "  \"sox\""
        Exit.exitSuccess

    mbDev <- case [dev | Device dev <- flags] of
        [] -> return $ Just defaultDevice
        "sox" : _ -> return Nothing
        name : _ -> do
            devs <- PortAudio.getOutputDevices
            maybe (error $ "no device named: " <> show name) (return . Just) $
                List.find ((==name) . PortAudio._name) devs

    quit <- Thread.flag
    _keyboard <- Async.async $ do
        putStrLn "press return to quit"
        c <- IO.getLine
        putStrLn $ "got " <> show c <> ", asking streamer to stop"
        Thread.set quit
    case mbDev of
        Just dev -> StreamAudio.streamToPortAudio True dev quit dir muted start
        Nothing -> streamSox quit dir muted start

streamSox :: Thread.Flag -> FilePath -> Set Text -> RealTime.RealTime -> IO ()
streamSox quit dir muted start = do
    streamer <- Async.async $
        StreamAudio.streamToSox True (Thread.wait quit) dir muted start
    putStrLn "press return to quit"
    _keyboard <- Async.async $ do
        c <- IO.getLine
        putStrLn $ "got " <> show c <> ", asking streamer to stop"
        Thread.set quit
    Async.wait streamer

data Flag = List | Device String
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["list"] (GetOpt.NoArg List) "list output devices"
    , GetOpt.Option [] ["device"] (GetOpt.ReqArg Device "dev")
        "use named device"
    ]

parseArgs :: [String] -> IO ([Flag], (String, (Set Text, RealTime.RealTime)))
parseArgs args = case GetOpt.getOpt GetOpt.Permute options args of
    (flags, args, [])
        | List `elem` flags -> return ([List], ("", (mempty, 0)))
        | otherwise -> fmap (flags,) $ maybe (usage []) return $ case args of
            dir : args -> (dir,) <$> case args of
                [] -> Just (mempty, 0)
                [muted] -> Just (parseMuted muted, 0)
                [muted, start] -> (parseMuted muted ,) . RealTime.seconds <$>
                    Read.readMaybe start
                _ -> Nothing
            _ -> Nothing
    (_, _, errors) -> usage errors
    where
    parseMuted = Set.fromList . map txt . Seq.split ","
    usage errors = do
        mapM_ putStrLn errors
        putStrLn "usage: stream_audio im/cache/score/path/block/id\
            \ [ mute,mute start ]"
        putStr (GetOpt.usageInfo "" options)
        Exit.exitFailure
