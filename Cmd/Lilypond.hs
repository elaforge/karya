-- | Cmd-level support for the lilypond backend.
module Cmd.Lilypond where
import qualified Control.Monad.Trans as Trans
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.Process as Process

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


-- | Wait this many seconds before kicking off a compile.  This is on top
-- of the usual derive delay.
compile_delay :: Thread.Seconds
compile_delay = 3

cmd_compile :: Msg.Msg -> Cmd.CmdIO
cmd_compile (Msg.DeriveStatus block_id (Msg.DeriveComplete perf)) = do
    compile block_id perf
    return Cmd.Continue
cmd_compile _ = return Cmd.Continue

compile :: BlockId -> Cmd.Performance -> Cmd.CmdT IO ()
compile block_id perf = do
    meta <- Block.block_meta <$> State.get_block block_id
    -- TODO pull this out of the environ, defaulting as Scale.Twelve would
    let maybe_key = Just (Pitch.Key "c-maj")
    case Lilypond.meta_to_score maybe_key meta of
        Nothing -> return ()
        Just (Left err) -> Log.warn $ "can't convert to lilypond: " ++ err
        Just (Right score) -> run score
    where
    run score = do
        old_compiles <- Cmd.gets
            (Cmd.state_lilypond_compiles . Cmd.state_play)
        -- Cancel the last one, if any.
        case Map.lookup block_id old_compiles of
            Just (Cmd.CancelLilypond var) ->
                Trans.liftIO $ IORef.writeIORef var True
            _ -> return ()
        var <- Trans.liftIO $ IORef.newIORef False
        Cmd.modify_play_state $ \st -> st { Cmd.state_lilypond_compiles =
            Map.insert block_id (Cmd.CancelLilypond var)
                (Cmd.state_lilypond_compiles st) }
        save_file <- SaveGit.save_file False <$> State.get
        Trans.liftIO $ void $ Thread.start $
            compile_score var save_file block_id score (Cmd.perf_events perf)

compile_score :: IORef.IORef Bool -> FilePath -> BlockId -> Lilypond.Score
    -> Derive.Events -> IO ()
compile_score var save_file block_id score events = do
    Thread.delay compile_delay
    cancelled <- IORef.readIORef var
    unless cancelled $ do
        let (score_doc, logs) = make_score score events
        mapM_ Log.write logs
        compile_ly save_file block_id score_doc

compile_ly :: FilePath -> BlockId -> Pretty.Doc -> IO ()
compile_ly save_file block_id score = do
    Directory.createDirectoryIfMissing True (save_file ++ "_ly")
    let fname = save_file ++ "_ly" </> Id.ident_name block_id
    writeFile (fname ++ ".ly") (Pretty.formatted score)
    void $ Process.rawSystem "lilypond" ["-o", fname, fname ++ ".ly"]

make_score :: Lilypond.Score -> Derive.Events -> (Pretty.Doc, [Log.Msg])
make_score score score_events = (Lilypond.make_score score events, logs)
    where
    (events, logs) = LEvent.partition $ Convert.convert
        (Lilypond.score_duration1 score) (filter LEvent.is_event score_events)
        -- Filter out existing logs because those will be reported by normal
        -- performance.
