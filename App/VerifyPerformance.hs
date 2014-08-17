-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs or lilypond code as the last saved performance.
module App.VerifyPerformance where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Traversable as Traversable
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment
import qualified System.FilePath as FilePath

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Process as Process
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Derive.DeriveSaved as DeriveSaved
import Types


data Flag = Help | Save | Perform | DumpMidi
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["save"] (GetOpt.NoArg Save)
        "Write saved performances to disk as binary."
    , GetOpt.Option [] ["perform"] (GetOpt.NoArg Perform)
        "Perform to MIDI and write to stdout."
    , GetOpt.Option [] ["dump-midi"] (GetOpt.NoArg DumpMidi)
        "pretty print binary saved MIDI to stdout"
    ]

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    when (null args) $ usage "no inputs"
    failures <- case flags of
        _ : _ : _ -> usage $ "only one flag allowed"
        [Help] -> usage ""
        [] -> do
            cmd_config <- DeriveSaved.load_cmd_config
            fmap sum $ forM args $ \fname -> do
                putStrLn $ "------------------------- verify " <> fname
                fails <- verify_performance cmd_config fname
                putStrLn $ if fails == 0
                    then "+++++++++++++++++++++++++ OK!"
                    else "_________________________ FAILED!"
                return fails
        [Save] -> sum <$> mapM save args
        [Perform] -> do
            cmd_config <- DeriveSaved.load_cmd_config
            sum <$> mapM (perform cmd_config) args
        [DumpMidi] -> sum <$> mapM dump_midi args
    Process.exit failures
    where
    usage msg = do
        putStrLn "usage: verify_performance [ flags ]"
        putStr (GetOpt.usageInfo msg options)
        Process.exit 1

handle_left :: Either String Int -> IO Int
handle_left = either (\err -> putStrLn err >> return 1) return

load :: FilePath -> IO (Either String (State.State, BlockId))
load fname = rightm (DeriveSaved.load_score fname) $ \state ->
    rightm (return $ get_root state) $ \block_id ->
        return $ Right (state, block_id)

save :: FilePath -> IO Int
save fname = (handle_left =<<) $ rightm (load fname) $ \(state, block_id) -> do
    let meta = State.config#State.meta #$ state
        look = Map.lookup block_id
    midi <- case look (State.meta_midi_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = FilePath.takeFileName fname <> ".midi"
            putStrLn $ "write " <> out
            DiffPerformance.save_midi out (State.perf_performance perf)
            return True
    ly <- case look (State.meta_lilypond_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = FilePath.takeFileName fname <> ".ly"
            putStrLn $ "write " <> out
            Text.IO.writeFile out (State.perf_performance perf)
            return True
    return $ if midi || ly then Right 0
        else Left $ fname <> ": no midi or ly performance"

perform :: Cmd.Config -> FilePath -> IO Int
perform cmd_config fname = (handle_left =<<) $
    rightm (load fname) $ \(state, block_id) ->
    rightm (perform_block fname cmd_config state block_id) $ \msgs -> do
        let out = FilePath.takeFileName fname <> ".midi"
        putStrLn $ "write " <> out
        DiffPerformance.save_midi out (Vector.fromList msgs)
        return $ Right 0

dump_midi :: FilePath -> IO Int
dump_midi fname = (handle_left =<<) $
    rightm (DiffPerformance.load_midi fname) $ \msgs -> do
        mapM_ Pretty.pprint (Vector.toList msgs)
        return $ Right 0

verify_performance :: Cmd.Config -> FilePath -> IO Int
verify_performance cmd_config fname = (handle_left =<<) $
    rightm (load fname) $ \(state, block_id) -> do
        let meta = State.config#State.meta #$ state
        n <- apply (verify_midi fname cmd_config state block_id) $
            Map.lookup block_id (State.meta_midi_performances meta)
        m <- apply (verify_lilypond fname cmd_config state block_id) $
            Map.lookup block_id (State.meta_lilypond_performances meta)
        return $ case (n, m) of
            (Nothing, Nothing) -> Left "no saved performances!"
            _ -> Right (fromMaybe 0 n + fromMaybe 0 m)

apply :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
apply = Traversable.mapM

verify_midi :: FilePath -> Cmd.Config -> State.State -> BlockId
    -> State.MidiPerformance -> IO Int
verify_midi fname cmd_config state block_id performance = (handle_left =<<) $
    rightm (perform_block fname cmd_config state block_id) $ \msgs ->
    case DiffPerformance.diff_midi_performance performance msgs of
        (Nothing, _, _) -> return $ Right 0
        (Just err, expected, got) -> do
            Text.IO.writeFile (base ++ ".expected") $ Text.unlines expected
            Text.IO.writeFile (base ++ ".got") $ Text.unlines got
            return $ Left $ untxt $ err
                <> "wrote " <> txt base <> ".{expected,got}"
    where base = FilePath.takeFileName fname

perform_block :: FilePath -> Cmd.Config -> State.State -> BlockId
    -> IO (Either String [Midi.WriteMessage])
perform_block fname cmd_config state block_id = do
    result <- DeriveSaved.timed_derive fname state
        (Cmd.initial_state cmd_config) block_id
    case result of
        Left err -> return $ Left $ "error deriving: " ++ err
        Right (events, logs) -> do
            mapM_ Log.write logs
            (msgs, logs) <- DeriveSaved.timed_perform cmd_config
                ("perform " <> fname) state events
            mapM_ Log.write logs
            return $ Right msgs

verify_lilypond :: FilePath -> Cmd.Config -> State.State -> BlockId
    -> State.LilypondPerformance -> IO Int
verify_lilypond fname cmd_config state block_id expected = do
    (result, logs) <- DeriveSaved.timed_lilypond fname state
        (Cmd.initial_state cmd_config) block_id
    mapM_ Log.write logs
    case result of
        Left err -> do
            putStrLn $ untxt $ "error deriving: " <> err
            return 1
        Right got -> case DiffPerformance.diff_lilypond expected got of
            Nothing -> putStrLn "ok!" >> return 0
            Just err -> do
                let base = FilePath.takeFileName fname
                Text.IO.writeFile (base ++ ".expected.ly") $
                    State.perf_performance expected
                Text.IO.writeFile (base ++ ".got.ly") got
                Text.IO.putStrLn err
                putStrLn $ "wrote " <> base <> ".{expected,got}.ly"
                return 1

get_root :: State.State -> Either String BlockId
get_root state = maybe (Left "no root block") Right $
    State.config#State.root #$ state
