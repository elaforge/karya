-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs or lilypond code as the last saved performance.
module App.VerifyPerformance where
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified System.Environment as Environment

import Util.Control
import qualified Util.Log as Log
import qualified Util.Process as Process

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Derive.DeriveSaved as DeriveSaved
import Types


main :: IO ()
main = do
    cmd_config <- DeriveSaved.load_cmd_config
    args <- Environment.getArgs
    when (null args) $ do
        putStrLn "usage: verify_performance [ save.state save.git ... ]"
        Process.exit 1
    failures <- fmap sum $ forM args $ \fname -> do
        putStrLn $ "verify " <> fname
        verify_score cmd_config fname
    Process.exit failures

verify_score :: Cmd.Config -> FilePath -> IO Int
verify_score cmd_config fname = (handle =<<) $
    rightm (DeriveSaved.load_score fname) $ \state ->
    rightm (return $ get_root state) $ \block_id -> do
        let meta = State.config#State.meta #$ state
        n <- apply (verify_midi fname cmd_config state block_id) $
            Map.lookup block_id (State.meta_midi_performances meta)
        m <- apply (verify_lilypond fname cmd_config state block_id) $
            Map.lookup block_id (State.meta_lilypond_performances meta)
        return $ case (n, m) of
            (Nothing, Nothing) -> Left "no saved performances!"
            _ -> Right (fromMaybe 0 n + fromMaybe 0 m)
    where handle = either (\err -> putStrLn err >> return 1) return

apply :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
apply = Traversable.mapM

verify_midi :: FilePath -> Cmd.Config -> State.State -> BlockId
    -> State.MidiPerformance -> IO Int
verify_midi fname cmd_config state block_id performance = do
    result <- DeriveSaved.timed_derive fname state
        (Cmd.initial_state cmd_config) block_id
    case result of
        Left err -> do
            putStrLn $ "error deriving: " ++ err
            return 1
        Right (events, logs) -> do
            mapM_ Log.write logs
            (msgs, logs) <- DeriveSaved.timed_perform cmd_config
                ("perform " <> fname) state events
            mapM_ Log.write logs
            case DiffPerformance.diff_midi_performance performance msgs of
                Nothing -> putStrLn "ok!" >> return 0
                Just err -> putStrLn (untxt err) >> return 1

verify_lilypond :: FilePath -> Cmd.Config -> State.State -> BlockId
    -> State.LilypondPerformance -> IO Int
verify_lilypond fname cmd_config state block_id performance = do
    (result, logs) <- DeriveSaved.timed_lilypond fname state
        (Cmd.initial_state cmd_config) block_id
    mapM_ Log.write logs
    case result of
        Left err -> do
            putStrLn $ "error deriving: " ++ err
            return 1
        Right code -> case DiffPerformance.diff_lilypond performance code of
            Nothing -> putStrLn "ok!" >> return 0
            Just err -> putStrLn (untxt err) >> return 1

get_root :: State.State -> Either String BlockId
get_root state = maybe (Left "no root block") Right $
    State.config#State.root #$ state
