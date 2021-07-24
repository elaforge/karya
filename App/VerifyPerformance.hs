-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmdline program to verify that a saved score still derives the same MIDI
-- msgs or lilypond code as the last saved performance.
module App.VerifyPerformance (main) where
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.Exceptions as Exceptions
import qualified Util.Git as Git
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Processes as Processes
import qualified Util.Seq as Seq
import qualified Util.SourceControl as SourceControl
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Midi.Midi as Midi
import qualified Perform.Im.Convert as Im.Convert
import qualified Synth.Shared.Note as Shared.Note
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


data Flag = Help | Mode Mode | Output !FilePath
    deriving (Eq, Show)

data Mode =
    Verify | Save | Perform | Profile | ProfileDerive | DumpMidi | CommitInfo
    deriving (Eq, Show, Bounded, Enum)

data PerformTo = ToDerive | ToMidi
    deriving (Eq, Show)

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
        \  Profile - Like Perform, but don't write any output.\n\
        \  ProfileDerive - Like Profile, but derive only, don't render midi.\n\
        \  DumpMidi - Pretty print binary saved MIDI to stdout.\n\
        \  CommitInfo - Dump info on the current commit in JSON. This doesn't\n\
        \    belong here, but there's no other great place."
    , GetOpt.Option [] ["out"] (GetOpt.ReqArg Output default_out_dir) $
        "Write output to this directory. This is diffs, and timing .json."
    ]

-- | This is intentionally not the same as what the shakefile uses, so I don't
-- mingle the results.
default_out_dir :: FilePath
default_out_dir = "build/verify-cmdline"

read_mode :: String -> Flag
read_mode s =
    Mode $ fromMaybe (error ("unknown mode: " <> show s)) $ Map.lookup s modes
    where modes = Map.fromList [(show m, m) | m <- [minBound .. maxBound]]

main :: IO ()
main = Git.initialize $ do
    args <- System.Environment.getArgs
    Log.configure $ \state -> state
        { Log.state_priority = Log.Warn
        , Log.state_write_msg = Log.write_formatted IO.stderr
        }
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    unless (null [Help | Help <- flags]) $ usage ""
    let out_dir = fromMaybe default_out_dir $ Seq.last [d | Output d <- flags]
    cmd_config <- DeriveSaved.load_cmd_config
    failures <- case fromMaybe Verify $ Seq.last [m | Mode m <- flags] of
        Verify -> do
            when (null args) $ usage "no inputs"
            fnames <- concatMapM expand_verify_me args
            results <- forM fnames $ \fname -> do
                putStrLn $ "------------------------- verify " <> fname
                fails <- run_error $ verify_performance out_dir cmd_config fname
                putStrLn $ if fails == 0
                    then "+++++++++++++++++++++++++ OK!"
                    else "_________________________ FAILED!"
                return (fname, fails)
            when (length results > 1) $ do
                let (failed, ok) = List.partition ((>0) . snd) results
                unless (null ok) $
                    putStr $ "    passed:\n" <> unlines (map fst ok)
                unless (null failed) $
                    putStr $ "    failed:\n" <> unlines (map fst failed)
            return $ Num.sum $ map snd results
        Save -> do
            when (null args) $ usage "no inputs"
            run_error $ concat <$> mapM (save out_dir) args
        Profile -> do
            when (null args) $ usage "no inputs"
            run_error $ mapM_ (perform Nothing cmd_config) args >> return []
        ProfileDerive -> do
            when (null args) $ usage "no inputs"
            run_error $ mapM_ (derive cmd_config) args >> return []
        Perform -> do
            when (null args) $ usage "no inputs"
            run_error $ mapM_ (perform (Just out_dir) cmd_config) args
                >> return []
        DumpMidi -> do
            when (null args) $ usage "no inputs"
            run_error $ mapM_ dump_midi args >> return []
        CommitInfo -> do
            patch <- either (errorIO . txt) return
                =<< SourceControl.current "."
            ByteString.Lazy.Char8.putStrLn $ Aeson.encode $ Map.fromList
                [ ("date" :: Text, Aeson.toJSON $ SourceControl._date patch)
                , ("hash", Aeson.toJSON $ SourceControl._hash patch)
                , ("name", Aeson.toJSON $ SourceControl._summary patch)
                ]
            return 0
    Processes.exit failures
    where
    usage msg = do
        putStrLn $ "error: " ++ msg
        putStrLn "usage: verify_performance [ flags ] dirs or filenames"
        putStr (GetOpt.usageInfo "" options)
        Processes.exit 1

verify_me_txt :: FilePath
verify_me_txt = "verify-me.txt"

-- | If this is a directory with a 'verify_me_txt', expand to include its
-- contents.
expand_verify_me :: FilePath -> IO [FilePath]
expand_verify_me fname = do
    m_contents <- Exceptions.ignoreIOError $
        Text.IO.readFile (fname </> verify_me_txt)
    return $ case m_contents of
        Nothing -> [fname]
        Just contents -> map ((fname</>) . untxt) $ Text.lines contents

type ErrorM a = Except.ExceptT Text IO a

run_error :: ErrorM [Text] -> IO Int
run_error m = do
    errors <- either (\err -> return [err]) return =<< Except.runExceptT m
    mapM_ Text.IO.putStrLn errors
    return (length errors)

require_right :: IO (Either Text a) -> Except.ExceptT Text IO a
require_right io = tryRight =<< liftIO io

-- * implementation

-- | Extract saved performances and write them to disk.
save :: FilePath -> FilePath -> ErrorM [Text]
save out_dir fname = do
    (state, _defs_lib, _aliases, block_id) <- load fname
    let meta = Ui.config#UiConfig.meta #$ state
        look = Map.lookup block_id
    midi <- case look (UiConfig.meta_midi_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = out_dir </> basename fname <> ".midi"
            liftIO $ putStrLn $ "write " <> out
            liftIO $ DiffPerformance.save_midi out (UiConfig.perf_events perf)
            return True
    ly <- case look (UiConfig.meta_lilypond_performances meta) of
        Nothing -> return False
        Just perf -> do
            let out = out_dir </> basename fname <> ".ly"
            liftIO $ putStrLn $ "write " <> out
            liftIO $ Text.IO.writeFile out (UiConfig.perf_events perf)
            return True
    return $ if midi || ly then []
        else [txt fname <> ": no midi or ly performance"]

-- | Perform to MIDI and possibly write to disk.
perform :: Maybe FilePath -> Cmd.Config -> FilePath -> ErrorM ()
perform maybe_out_dir cmd_config fname = do
    (state, library, aliases, block_id) <- load fname
    (msgs, _, _) <- perform_block fname
        (make_cmd_state library aliases cmd_config) state block_id
    whenJust maybe_out_dir $ \out_dir -> do
        let out = out_dir </> basename fname <> ".midi"
        liftIO $ putStrLn $ "write " <> out
        liftIO $ DiffPerformance.save_midi out (Vector.fromList msgs)

-- | Like 'perform', but don't perform to MIDI.
derive :: Cmd.Config -> FilePath
    -> ErrorM (Vector.Vector Score.Event, DeriveSaved.CPU)
derive cmd_config fname = do
    (state, library, aliases, block_id) <- load fname
    let cmd_state = make_cmd_state library aliases cmd_config
    ((!events, logs), derive_cpu) <- liftIO $
        DeriveSaved.timed_derive fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    liftIO $ Text.IO.putStrLn $ "derived " <> showt (Vector.length events)
        <> " in " <> Num.showFloat 2 (toSecs derive_cpu)
    return (events, derive_cpu)

dump_midi :: FilePath -> ErrorM ()
dump_midi fname = do
    msgs <- require_right $ DiffPerformance.load_midi fname
    liftIO $ mapM_ Pretty.pprint (Vector.toList msgs)

type Timings = [(Text, Thread.Seconds)]

verify_performance :: FilePath -> Cmd.Config -> FilePath -> ErrorM [Text]
verify_performance out_dir cmd_config fname = do
    (state, library, aliases, block_id) <- load fname
    let meta = Ui.config#UiConfig.meta #$ state
    let cmd_state = make_cmd_state library aliases cmd_config
    let verify1 verify field =
            maybe (return (Nothing, []))
                (verify out_dir fname cmd_state state block_id) $
            Map.lookup block_id (field meta)
    (midi_err, midi_timings) <-
        verify1 verify_midi UiConfig.meta_midi_performances
    (im_err, im_timings) <- verify1 verify_im UiConfig.meta_im_performances
    (ly_err, ly_timings) <- verify1 verify_lilypond
        UiConfig.meta_lilypond_performances
    let timings = midi_timings ++ im_timings ++ ly_timings
    if null timings
        then return ["no saved performances"]
        else do
            liftIO $ write_timing (out_dir </> basename fname <> ".json")
                timings
            return $ Maybe.catMaybes [midi_err, im_err, ly_err]


-- | Perform from the given state and compare it to the old MidiPerformance.
verify_midi :: FilePath -> FilePath -> Cmd.State -> Ui.State -> BlockId
    -> UiConfig.MidiPerformance -> ErrorM (Maybe Text, Timings)
verify_midi out_dir fname cmd_state ui_state block_id performance = do
    (msgs, derive_cpu, perform_cpu) <-
        perform_block fname cmd_state ui_state block_id
    (maybe_diff, wrote_files) <- liftIO $
        DiffPerformance.diff_midi (basename fname ++ ".midi")
            out_dir performance msgs
    return
        ( (<> ("\nwrote " <> txt (unwords wrote_files))) <$> maybe_diff
        , [("derive", derive_cpu), ("perform", perform_cpu)]
        )

verify_im :: FilePath -> FilePath -> Cmd.State -> Ui.State -> BlockId
    -> UiConfig.ImPerformance -> ErrorM (Maybe Text, Timings)
verify_im out_dir fname cmd_state ui_state block_id performance = do
    ((events, logs), derive_cpu) <- liftIO $
        DeriveSaved.timed_derive fname ui_state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    let (notes, convert_logs) = convert_im cmd_state ui_state block_id
            (Vector.toList events)
    liftIO $ mapM_ Log.write convert_logs
    (maybe_diff, wrote_files) <- liftIO $
        DiffPerformance.diff_im (basename fname ++ ".im")
            out_dir performance notes
    return
        ( (<> ("\nwrote " <> txt (unwords wrote_files))) <$> maybe_diff
        , [("derive", derive_cpu)]
        )

convert_im :: Cmd.State -> Ui.State -> BlockId -> [Score.Event]
    -> ([Shared.Note.Note], [Log.Msg])
convert_im cmd_state ui_state block_id events =
    extract $ DeriveSaved.run_cmd ui_state cmd_state $ do
        lookup_inst <- Cmd.get_lookup_instrument
        return $ Im.Convert.convert block_id lookup_inst events
    where
    extract (Left err) = ([], [Log.msg Log.Error Nothing err])
    extract (Right (levents, logs)) = (events, logs ++ perf_logs)
        where (events, perf_logs) = LEvent.partition levents

perform_block :: FilePath -> Cmd.State -> Ui.State -> BlockId
    -> ErrorM ([Midi.WriteMessage], DeriveSaved.CPU, DeriveSaved.CPU)
perform_block fname cmd_state state block_id = do
    ((events, logs), derive_cpu) <- liftIO $
        DeriveSaved.timed_derive fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    ((msgs, logs), perform_cpu) <- liftIO $
        DeriveSaved.timed_perform cmd_state fname state events
    liftIO $ mapM_ Log.write logs
    return (msgs, derive_cpu, perform_cpu)

verify_lilypond :: FilePath -> FilePath -> Cmd.State -> Ui.State
    -> BlockId -> UiConfig.LilypondPerformance -> ErrorM (Maybe Text, Timings)
verify_lilypond out_dir fname cmd_state state block_id performance = do
    ((result, logs), cpu) <- liftIO $
        DeriveSaved.timed_lilypond fname state cmd_state block_id
    liftIO $ mapM_ Log.write logs
    case result of
        Left err -> return (Just $ "error deriving: " <> Log.format_msg err, [])
        Right got -> do
            (maybe_diff, wrote_files) <- liftIO $
                DiffPerformance.diff_lilypond (basename fname ++ ".ly") out_dir
                    performance got
            return
                ( (<> ("\nwrote " <> txt (unwords wrote_files))) <$> maybe_diff
                , [("lilypond", cpu)]
                )

-- * util

-- | Load a score and get its root block id.
load :: FilePath
    -> ErrorM (Ui.State, Derive.Builtins, Derive.InstrumentAliases, BlockId)
load fname = do
     (state, builtins, aliases) <- require_right $ DeriveSaved.load_score fname
     block_id <- require_right $ return $ get_root state
     return (state, builtins, aliases, block_id)

make_cmd_state :: Derive.Builtins -> Derive.InstrumentAliases -> Cmd.Config
    -> Cmd.State
make_cmd_state builtins aliases cmd_config =
    DeriveSaved.add_library builtins aliases $ Cmd.initial_state cmd_config

get_root :: Ui.State -> Either Text BlockId
get_root state = justErr "no root block" $ Ui.config#UiConfig.root #$ state

basename :: FilePath -> FilePath
basename = FilePath.takeFileName . Seq.rdrop_while (=='/')

write_timing :: FilePath -> Timings -> IO ()
write_timing fname timings = ByteString.Lazy.writeFile fname $ (<>"\n") $
    Aeson.encode $ Map.fromList $ map (second toSecs) timings

toSecs :: Thread.Seconds -> Double
toSecs = realToFrac
