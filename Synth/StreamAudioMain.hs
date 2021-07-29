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

import qualified Derive.ScoreT as ScoreT
import qualified Perform.RealTime as RealTime
import qualified Synth.StreamAudio as StreamAudio

import           Global


main :: IO ()
main = PortAudio.initialize $ do
    (flags, (dir, (muted, start))) <- parseArgs =<< Environment.getArgs

    (devs, defaultDev) <- StreamAudio.getDevices
    when (List `elem` flags) $ do
        forM_ (map fst devs) $ \dev ->
            putStrLn $ (if dev == defaultDev then "* " else "  ") <> show dev
        Exit.exitSuccess
    mbDev <- traverse getDevice $ Seq.last [d | Device d <- flags]
    quit <- Thread.flag
    _keyboard <- Async.async $ do
        putStrLn "press return to quit"
        c <- IO.getLine
        putStrLn $ "got " <> show c <> ", asking streamer to stop"
        Thread.set quit
    StreamAudio.streamDir mbDev quit muted start dir

getDevice :: String -> IO StreamAudio.Device
getDevice name = do
    (devs, _) <- StreamAudio.getDevices
    maybe (errorIO $ "unknown device: " <> showt name) (return . snd) $
        List.find ((==name) . fst) devs

data Flag = List | Device String
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["list"] (GetOpt.NoArg List) "list output devices"
    , GetOpt.Option [] ["device"] (GetOpt.ReqArg Device "dev")
        "use named device"
    ]

parseArgs :: [String]
    -> IO ([Flag], (String, (Set ScoreT.Instrument, RealTime.RealTime)))
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
    parseMuted = Set.fromList . map (ScoreT.Instrument . txt) . Seq.split ","
    usage errors = do
        mapM_ putStrLn errors
        putStrLn "usage: stream_audio im/cache/score/path/block/id\
            \ [ mute,mute start ]"
        putStr (GetOpt.usageInfo "" options)
        Exit.exitFailure
