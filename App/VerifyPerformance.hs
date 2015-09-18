-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs or lilypond code as the last saved performance.
module App.VerifyPerformance (main) where
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.File as File
import qualified Util.Git as Git
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


data Flag = Help | Mode Mode | Output !FilePath
    deriving (Eq, Show)

data Mode = Verify | Save | Perform | DumpMidi
    deriving (Eq, Show, Bounded, Enum)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["mode"]
        (GetOpt.ReqArg read_mode (show [minBound :: Mode .. maxBound])) $
        "Run in this mode, defaults to Verify.  Modes:\n\
        \  Verify - Check saved performances against current performances.\n\
        \    If you give a directory and it has a file inside called\n\
        \    " ++ verify_me_txt ++ ", use the contents of the file as further\n\
        \   files to verify.\n\
        \  Save - Write saved performances to disk as binary.\n\
        \  Perform - Perform to MIDI and write to $input.midi.\n\
        \  DumpMidi - Pretty print binary saved MIDI to stdout."
    , GetOpt.Option [] ["out"] (GetOpt.ReqArg Output default_out_dir)
        "write output to this directory"
    ]

default_out_dir :: FilePath
default_out_dir = "build/test"

read_mode :: String -> Flag
read_mode s =
    Mode $ fromMaybe (error ("unknown mode: " <> show s)) $ Map.lookup s modes
    where
    modes = Map.fromList
        [(show m, m) | m <- [minBound .. maxBound]]

main :: IO ()
main = Git.initialize $ do
    args <- System.Environment.getArgs
    Log.configure $ \state -> state
        { Log.state_write_msg = Log.write_formatted IO.stderr }
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    when (null args) $ usage "no inputs"
    unless (null [Help | Help <- flags]) $ usage ""
    let out_dir = fromMaybe default_out_dir $ Seq.last [d | Output d <- flags]
    failures <- case fromMaybe Verify $ Seq.last [m | Mode m <- flags] of
        Verify -> do
            cmd_config <- DeriveSaved.load_cmd_config
            fnames <- concatMapM expand_verify_me args
            fmap sum $ forM fnames $ \fname -> do
                putStrLn $ "------------------------- verify " <> fname
                fails <- run $ verify_performance out_dir cmd_config fname
                putStrLn $ if fails == 0
                    then "+++++++++++++++++++++++++ OK!"
                    else "_________________________ FAILED!"
                return fails
        Save -> run $ concat <$> mapM (save out_dir) args
        Perform -> do
            cmd_config <- DeriveSaved.load_cmd_config
            run $ concat <$> mapM (perform out_dir cmd_config) args
        DumpMidi -> run $ concat <$> mapM dump_midi args
    Process.exit failures
    where
    usage msg = do
        putStrLn $ "error: " ++ msg
        putStrLn "usage: verify_performance [ flags ] dirs or filenames"
        putStr (GetOpt.usageInfo "" options)
        Process.exit 1

verify_me_txt :: FilePath
verify_me_txt = "verify-me.txt"

-- | If this is a directory with a 'verify_me_txt', expand to include its
-- contents.
expand_verify_me :: FilePath -> IO [FilePath]
expand_verify_me fname = do
    m_contents <- File.ignoreIO $ Text.IO.readFile (fname </> verify_me_txt)
    return $ case m_contents of
        Nothing -> [fname]
        Just contents -> map ((fname</>) . untxt) $ Text.lines contents

type Error a = Error.ErrorT Text IO a

run :: Error [Text] -> IO Int
run m = do
    errors <- either (\err -> return [err]) return =<< Error.runErrorT m
    mapM_ Text.IO.putStrLn errors
    return (length errors)

require_right :: IO (Either Text a) -> Error.ErrorT Text IO a
require_right io = either Error.throwError return =<< liftIO io

-- * implementation

-- | Extract saved performances and write them to disk.
save :: FilePath -> FilePath -> Error [Text]
save out_dir fname = do
    (state, _defs_lib, block_id) <- load fname
    let meta = State.config#State.meta #$ state
        look = Map.lookup block_id
    midi <- case look (State.meta_midi_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = out_dir </> basename fname <> ".midi"
            liftIO $ putStrLn $ "write " <> out
            liftIO $ DiffPerformance.save_midi out (State.perf_performance perf)
            return True
    ly <- case look (State.meta_lilypond_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = out_dir </> basename fname <> ".ly"
            liftIO $ putStrLn $ "write " <> out
            liftIO $ Text.IO.writeFile out (State.perf_performance perf)
            return True
    return $ if midi || ly then []
        else [txt fname <> ": no midi or ly performance"]

-- | Perform to MIDI and write to disk.
perform :: FilePath -> Cmd.Config -> FilePath -> Error [Text]
perform out_dir cmd_config fname = do
    (state, library, block_id) <- load fname
    msgs <- perform_block fname (make_cmd_state library cmd_config) state
        block_id
    let out = out_dir </> basename fname <> ".midi"
    liftIO $ putStrLn $ "write " <> out
    liftIO $ DiffPerformance.save_midi out (Vector.fromList msgs)
    return []

dump_midi :: FilePath -> Error [Text]
dump_midi fname = do
    msgs <- require_right $ DiffPerformance.load_midi fname
    liftIO $ mapM_ Pretty.pprint (Vector.toList msgs)
    return []

verify_performance :: FilePath -> Cmd.Config -> FilePath -> Error [Text]
verify_performance out_dir cmd_config fname = do
    (state, library, block_id) <- load fname
    let meta = State.config#State.meta #$ state
    let cmd_state = make_cmd_state library cmd_config
    let midi_perf = Map.lookup block_id (State.meta_midi_performances meta)
        ly_perf = Map.lookup block_id (State.meta_lilypond_performances meta)
    midi_err <- maybe (return Nothing)
        (verify_midi out_dir fname cmd_state state block_id) midi_perf
    ly_err <- maybe (return Nothing)
        (verify_lilypond out_dir fname cmd_state state block_id) ly_perf
    return $ case (midi_perf, ly_perf) of
        (Nothing, Nothing) -> ["no saved performances"]
        _ -> Maybe.catMaybes [midi_err, ly_err]


-- | Perform from the given state and compare it to the old MidiPerformance.
verify_midi :: FilePath -> FilePath -> Cmd.State -> State.State -> BlockId
    -> State.MidiPerformance -> Error (Maybe Text)
verify_midi out_dir fname cmd_state state block_id performance = do
    msgs <- perform_block fname cmd_state state block_id
    (maybe_diff, wrote_files) <- liftIO $
        DiffPerformance.diff_midi_performance (basename fname) out_dir
            performance msgs
    return $ (<> ("\nwrote " <> txt (unwords wrote_files))) <$> maybe_diff

perform_block :: FilePath -> Cmd.State -> State.State -> BlockId
    -> Error [Midi.WriteMessage]
perform_block fname cmd_state state block_id = do
    (events, logs) <- liftIO $
        DeriveSaved.timed_derive fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    (msgs, logs) <- liftIO $ DeriveSaved.timed_perform cmd_state
        ("perform " <> fname) state events
    liftIO $ mapM_ Log.write logs
    return msgs

verify_lilypond :: FilePath -> FilePath -> Cmd.State -> State.State
    -> BlockId -> State.LilypondPerformance -> Error (Maybe Text)
verify_lilypond out_dir fname cmd_state state block_id performance = do
    (result, logs) <- liftIO $
        DeriveSaved.timed_lilypond fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    case result of
        Left err -> return $ Just $ "error deriving: " <> err
        Right got -> do
            (maybe_diff, wrote_files) <- liftIO $
                DiffPerformance.diff_lilypond (basename fname ++ ".ly") out_dir
                    performance got
            return $ (<> ("\nwrote " <> txt (unwords wrote_files))) <$>
                maybe_diff

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
