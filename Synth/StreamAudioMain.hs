-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.StreamAudioMain (main) where
import qualified Control.Concurrent.Async as Async
import qualified Data.Set as Set
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Text.Read as Read

import qualified Util.Seq as Seq
import qualified Util.Thread as Thread
import qualified Perform.RealTime as RealTime
import qualified Synth.StreamAudio as StreamAudio

import           Global


main :: IO ()
main = do
    (dir, (mutes, start)) <- parseArgs =<< Environment.getArgs
    quit <- Thread.flag
    streamer <- Async.async $
        StreamAudio.streamFrom_ True (Thread.wait quit) dir mutes start
    putStrLn "press return to quit"
    _keyboard <- Async.async $ do
        c <- IO.getLine
        putStrLn $ "got " <> show c <> ", asking streamer to stop"
        Thread.set quit
    Async.wait streamer

parseArgs :: [String] -> IO (String, (Set Text, RealTime.RealTime))
parseArgs args = maybe usage return $ case args of
    dir : args -> (dir,) <$> case args of
        [] -> Just (mempty, 0)
        [mutes] -> Just (parseMutes mutes, 0)
        [mutes, start] ->
            (parseMutes mutes ,) . RealTime.seconds <$> Read.readMaybe start
        _ -> Nothing
    _ -> Nothing
    where
    parseMutes = Set.fromList . map txt . Seq.split ","
    usage = error "usage: stream_audio im/cache/score/path/block/id\
        \ [ mute,mute start ]"
