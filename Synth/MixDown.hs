-- | Mix multiple directories of rendered chunks into a single audio file.
module Synth.MixDown (main) where
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Synth.Lib.Mix as Mix

import           Global


main :: IO ()
main = do
    (out, dirs) <- Environment.getArgs >>= \case
        out : dirs
            | ".wav" `List.isSuffixOf` out -> return (out, dirs)
            | otherwise -> usage "out arg should be .wav!"
        [] -> usage ""
    unlessM (allM Directory.doesDirectoryExist dirs) $
        usage "non-directory arg"
    Mix.mix out dirs

usage :: String -> IO a
usage msg = do
    unless (null msg) $
        IO.hPutStrLn IO.stderr $ "error: " <> msg
    IO.hPutStrLn IO.stderr $ "usage: mixdown out.wav [ dir1 dir2 ... ]"
    Exit.exitFailure
