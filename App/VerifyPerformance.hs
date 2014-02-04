-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs as the last saved performance.
module App.VerifyPerformance where
import qualified Data.Map as Map
import qualified System.Environment as Environment
import qualified System.Exit as Exit

import Util.Control
import qualified Util.Log as Log
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.DeriveSaved as DeriveSaved
import Types


main :: IO ()
main = do
    cmd_config <- DeriveSaved.load_cmd_config
    args <- Environment.getArgs
    failures <- forM args $ \fname -> do
        putStrLn $ "verify " <> fname
        failed <- verify cmd_config fname
        return $ if failed then 0 else 1
    Exit.exitWith $ Exit.ExitFailure (sum failures)

verify :: Cmd.Config -> FilePath -> IO Bool
verify cmd_config fname = perform cmd_config fname >>= \x -> case x of
    Left err -> putStrLn (fname <> ": " <> err) >> return False
    Right (performance, msgs) -> case DiffPerformance.verify performance msgs of
        Nothing -> do
            putStrLn "ok!\n"
            return True
        Just err -> do
            putStrLn $ untxt err
            putStrLn ""
            return False

perform :: Cmd.Config -> FilePath
    -> IO (Either String (State.Performance, [Midi.WriteMessage]))
perform cmd_config fname =
    rightm (load_score fname) $ \state ->
    rightm (return $ get_root state) $ \block_id ->
    rightm (return $ get_performance block_id state) $ \performance -> do
    rightm (DeriveSaved.timed_derive fname state
            (Cmd.initial_state cmd_config) block_id) $ \(events, logs) -> do
        mapM_ Log.write logs
        (msgs, logs) <- DeriveSaved.timed_perform cmd_config
            ("perform " <> fname) state events
        mapM_ Log.write logs
        return $ Right (performance, msgs)

get_root :: State.State -> Either String BlockId
get_root state = maybe (Left "no root block") Right $
    State.config#State.root #$ state

get_performance :: BlockId -> State.State -> Either String State.Performance
get_performance block_id state =
    maybe (Left $ "no performance for " ++ show block_id) Right $
        Map.lookup block_id $
            State.config#State.meta#State.performances #$ state

load_score :: FilePath -> IO (Either String State.State)
load_score fname = rightm (Save.infer_save_type fname) $ \save -> case save of
    Save.Git repo -> rightm (SaveGit.load repo Nothing) $ \(state, _, _) ->
        return $ Right state
    Save.State fname -> rightm (Save.read_state_ fname) $ \x -> case x of
        Nothing -> return $ Left "file not found"
        Just state -> return $ Right state
