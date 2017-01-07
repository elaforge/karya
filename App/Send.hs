-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Simple command-line tool to connect to the sequencer and send a single
    cmd.  It's a cmdline version of "App.Repl".
-}
module App.Send where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.IO as IO

import qualified Text.Printf as Printf

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import Global


data Flag = Help | Timing | Cmd String
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["timing"] (GetOpt.NoArg Timing)
        "show cmd completion time"
    , GetOpt.Option [] ["cmd"] (GetOpt.ReqArg Cmd "LState.some_cmd")
        "Read text from stdin, quote it as a string, and send it as the\
        \ given cmd's only argument."
    ]

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ unlines errs
    when (Help `elem` flags) (usage "usage:")
    msgs <- case [cmd | Cmd cmd <- flags] of
        [cmd]
            | not (null args) -> usage "--cmd expects stdin"
            | otherwise -> do
                input <- getContents
                return [cmd <> " " ++ show input]
        []
            | null args -> lines <$> getContents
            | otherwise -> return [unwords args]
        _ -> usage "there should be exactly zero or one --cmd"

    forM_ msgs $ \msg -> do
        if Timing `elem` flags then do
            putStrLn $ "send: " ++ msg
            (response, time) <- timed $ query (Text.pack msg)
            Printf.printf "%s - %.3f\n" (Text.unpack response) time
        else do
            response <- query (Text.pack msg)
            unless (Text.null response) $
                Text.IO.putStrLn response
    where
    usage msg = do
        putStrLn $ "ERROR: " ++ msg
        putStrLn "usage: send [ flags ]"
        putStr (GetOpt.usageInfo "" options)
        System.Exit.exitFailure

query :: Text -> IO Text
query = fmap ReplProtocol.format_result
    . ReplProtocol.query_cmd Config.repl_socket

printLogs :: [Text] -> IO ()
printLogs [] = return ()
printLogs logs = do
    Text.IO.hPutStrLn IO.stderr "\nLogs:"
    mapM_ (Text.IO.hPutStrLn IO.stderr) logs

timed :: DeepSeq.NFData a => IO a -> IO (a, Double)
timed action = do
    start <- now
    result <- action
    () <- return $ DeepSeq.rnf result
    end <- now
    return (result, realToFrac (end - start))

now :: IO Time.DiffTime
now = fmap Time.utctDayTime Time.getCurrentTime
