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

import qualified Text.Printf as Printf

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol

import           Global


data Flag = Help | Timing
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["timing"] (GetOpt.NoArg Timing)
        "show cmd completion time"
    ]

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ unlines errs
    when (Help `elem` flags) (usage "usage:")
    when (null args) (usage "usage:")
    args <- substitute $ map Text.strip $ Text.splitOn ";" $ mconcatMap txt args

    forM_ args $ \cmd -> do
        if Timing `elem` flags then do
            Text.IO.putStrLn $ "send: " <> cmd
            (response, time) <- timed $ query cmd
            Printf.printf "%s - %.3f\n" (Text.unpack response) time
        else do
            response <- query cmd
            unless (Text.null response) $
                Text.IO.putStrLn response
    where
    usage msg = do
        putStrLn $ "ERROR: " ++ Seq.strip msg
        putStrLn "usage: send [ flags ] cmd ..."
        let doc = "Cmds are split on ;.  If a cmd has a %s in it, then read\
                \ from stdin, and replace the %s with stdin quoted as a string."
        putStr (GetOpt.usageInfo doc options)
        System.Exit.exitFailure

substitute :: [Text] -> IO [Text]
substitute args
    | any ("%s" `Text.isInfixOf`) args = do
        content <- Text.IO.getContents
        return $ map (Text.replace "%s" (showt content)) args
    | otherwise = return args

query :: Text -> IO Text
query = fmap ReplProtocol.format_result
    . ReplProtocol.query_cmd Config.repl_socket

timed :: DeepSeq.NFData a => IO a -> IO (a, Double)
timed action = do
    start <- now
    result <- action
    () <- return $ DeepSeq.rnf result
    end <- now
    return (result, realToFrac (end - start))

now :: IO Time.DiffTime
now = fmap Time.utctDayTime Time.getCurrentTime
