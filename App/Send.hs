{- | Simple command-line tool to connect to the given socket and send some
    text, and then receive the response.

    It's used to talk over the seq_language socket.
-}
module App.Send where
import qualified Control.DeepSeq as DeepSeq
import Control.Monad
import qualified Data.Time as Time
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified Text.Printf as Printf

import qualified App.SendCmd as SendCmd


data Flag = Timing
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['t'] [] (GetOpt.NoArg Timing) "show cmd completion time"
    ]

main :: IO ()
main = SendCmd.initialize $ do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> error $ "flag errors:\n" ++ unlines errs
    print flags
    msgs <- if null args
        then fmap lines getContents
        else return [unwords args]
    forM_ msgs $ \msg -> do
        putStrLn $ "---> " ++ msg
        if Timing `elem` flags then do
            (response, time) <- timed $ SendCmd.send (msg ++ "\n")
            Printf.printf "%s - %.3f\n" response time
        else do
            response <- SendCmd.send (msg ++ "\n")
            unless (null response) $
                putStrLn response

timed :: IO String -> IO (String, Double)
timed action = do
    start <- now
    result <- action
    () <- return $ DeepSeq.rnf result
    end <- now
    return (result, realToFrac (end - start))

now :: IO Time.DiffTime
now = fmap Time.utctDayTime Time.getCurrentTime
