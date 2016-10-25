-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Simple command-line tool to connect to the given socket and send some
    text, and then receive the response.

    It's used to talk over the seq_language socket.
-}
module App.Send where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.IO as IO

import qualified Text.Printf as Printf

import qualified App.ReplProtocol as ReplProtocol
import Global


data Flag = Timing
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['t'] [] (GetOpt.NoArg Timing) "show cmd completion time"
    ]

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> error $ "flag errors:\n" ++ unlines errs
    msgs <- if null args
        then fmap lines getContents
        else return [unwords args]
    forM_ msgs $ \msg -> do
        putStrLn $ "---> " ++ msg
        if Timing `elem` flags then do
            (response, time) <- timed $ ReplProtocol.query_cmd (Text.pack msg)
            Printf.printf "%s - %.3f\n" (Text.unpack response) time
        else do
            response <- ReplProtocol.query_cmd (Text.pack msg <> "\n")
            unless (Text.null response) $
                Text.IO.putStrLn response

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
