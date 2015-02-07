-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs or lilypond code as the last saved performance.
module App.VerifyPerformance where
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Traversable as Traversable
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Process as Process
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import Global
import Types


data Flag = Help | Save | Perform | DumpMidi
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["save"] (GetOpt.NoArg Save)
        "Write saved performances to disk as binary."
    , GetOpt.Option [] ["perform"] (GetOpt.NoArg Perform)
        "Perform to MIDI and write to $input.midi."
    , GetOpt.Option [] ["dump-midi"] (GetOpt.NoArg DumpMidi)
        "pretty print binary saved MIDI to stdout"
    ]

main :: IO ()
main = do
    args <- System.Environment.getArgs
    Log.configure $ \state -> state
        { Log.state_write_msg = Log.write_formatted IO.stderr }
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
                fails <- run $ verify_performance cmd_config fname
                putStrLn $ if fails == 0
                    then "+++++++++++++++++++++++++ OK!"
                    else "_________________________ FAILED!"
                return fails
        [Save] -> run $ sum <$> mapM save args
        [Perform] -> do
            cmd_config <- DeriveSaved.load_cmd_config
            run $ sum <$> mapM (perform cmd_config) args
        [DumpMidi] -> run $ sum <$> mapM dump_midi args
    Process.exit failures
    where
    usage msg = do
        putStrLn "usage: verify_performance [ flags ]"
        putStr (GetOpt.usageInfo msg options)
        Process.exit 1

type Error a = Error.ErrorT Text IO a

run :: Error Int -> IO Int
run = either (\err -> Text.IO.putStrLn err >> return 1) return
    <=< Error.runErrorT

require_right :: IO (Either Text a) -> Error.ErrorT Text IO a
require_right io = either Error.throwError return =<< liftIO io

-- * implementation

-- | Extract saved performances and write them to disk.
save :: FilePath -> Error Int
save fname = do
    (state, _defs_lib, block_id) <- load fname
    let meta = State.config#State.meta #$ state
        look = Map.lookup block_id
    midi <- case look (State.meta_midi_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = basename fname <> ".midi"
            liftIO $ putStrLn $ "write " <> out
            liftIO $ DiffPerformance.save_midi out (State.perf_performance perf)
            return True
    ly <- case look (State.meta_lilypond_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = basename fname <> ".ly"
            liftIO $ putStrLn $ "write " <> out
            liftIO $ Text.IO.writeFile out (State.perf_performance perf)
            return True
    if midi || ly then return 0
        else Error.throwError $ txt fname <> ": no midi or ly performance"

-- | Perform to MIDI and write to disk.
perform :: Cmd.Config -> FilePath -> Error Int
perform cmd_config fname = do
    (state, library, block_id) <- load fname
    msgs <- perform_block fname (make_cmd_state library cmd_config) state
        block_id
    let out = basename fname <> ".midi"
    liftIO $ putStrLn $ "write " <> out
    liftIO $ DiffPerformance.save_midi out (Vector.fromList msgs)
    return 0

dump_midi :: FilePath -> Error Int
dump_midi fname = do
    msgs <- require_right $ DiffPerformance.load_midi fname
    liftIO $ mapM_ Pretty.pprint (Vector.toList msgs)
    return 0

verify_performance :: Cmd.Config -> FilePath -> Error Int
verify_performance cmd_config fname = do
    (state, library, block_id) <- load fname
    let meta = State.config#State.meta #$ state
    let cmd_state = make_cmd_state library cmd_config
    n <- apply (verify_midi fname cmd_state state block_id) $
        Map.lookup block_id (State.meta_midi_performances meta)
    m <- apply (verify_lilypond fname cmd_state state block_id) $
        Map.lookup block_id (State.meta_lilypond_performances meta)
    case (n, m) of
        (Nothing, Nothing) -> Error.throwError "no saved performances!"
        _ -> return (fromMaybe 0 n + fromMaybe 0 m)
    where
    apply :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
    apply = Traversable.mapM

-- | Perform from the given state and compare it to the old MidiPerformance.
verify_midi :: FilePath -> Cmd.State -> State.State -> BlockId
    -> State.MidiPerformance -> Error Int
verify_midi fname cmd_state state block_id performance = do
    msgs <- perform_block fname cmd_state state block_id
    case DiffPerformance.diff_midi_performance performance msgs of
        (Nothing, _, _) -> return 0
        (Just err, expected, got) -> do
            liftIO $ do
                Text.IO.writeFile (base ++ ".expected") $ Text.unlines expected
                Text.IO.writeFile (base ++ ".got") $ Text.unlines got
            Error.throwError $ err <> "wrote " <> txt base <> ".{expected,got}"
    where base = basename fname

perform_block :: FilePath -> Cmd.State -> State.State -> BlockId
    -> Error [Midi.WriteMessage]
perform_block fname cmd_state state block_id = do
    (events, logs) <- require_right $
        DeriveSaved.timed_derive fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    (msgs, logs) <- liftIO $ DeriveSaved.timed_perform cmd_state
        ("perform " <> fname) state events
    liftIO $ mapM_ Log.write logs
    return msgs

verify_lilypond :: FilePath -> Cmd.State -> State.State -> BlockId
    -> State.LilypondPerformance -> Error Int
verify_lilypond fname cmd_state state block_id expected = do
    (result, logs) <- liftIO $
        DeriveSaved.timed_lilypond fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    case result of
        Left err -> Error.throwError $ "error deriving: " <> err
        Right got -> case DiffPerformance.diff_lilypond expected got of
            Nothing -> do
                liftIO $ putStrLn "ok!"
                return 0
            Just err -> do
                let base = basename fname
                liftIO $ do
                    Text.IO.writeFile (base ++ ".expected.ly") $
                        State.perf_performance expected
                    Text.IO.writeFile (base ++ ".got.ly") got
                    Text.IO.putStrLn err
                Error.throwError $ "wrote " <> txt base <> ".{expected,got}.ly"

-- * util

-- | Load a score and get its root block id.
load :: FilePath -> Error (State.State, Derive.Library, BlockId)
load fname = do
     (state, library) <- require_right $ DeriveSaved.load_score fname
     block_id <- require_right $ return $ get_root state
     return (state, library, block_id)

make_cmd_state :: Derive.Library -> Cmd.Config -> Cmd.State
make_cmd_state library cmd_config =
    DeriveSaved.add_library library $ Cmd.initial_state cmd_config

get_root :: State.State -> Either Text BlockId
get_root state = maybe (Left "no root block") Right $
    State.config#State.root #$ state

basename :: FilePath -> FilePath
basename = FilePath.takeFileName . Seq.rdrop_while (=='/')
