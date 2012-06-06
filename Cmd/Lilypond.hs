-- | Cmd-level support for the lilypond backend.
module Cmd.Lilypond where
import qualified Control.Monad.Trans as Trans
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


-- TODO
-- Maybe I should wait a while before running lilypond.  But then I have to put
-- the ThreadId somewhere, or at least a var to check before setting off the
-- compile.

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
        Just (Right score) -> do
            save_file <- SaveGit.save_file False <$> State.get
            Trans.liftIO $ void $ Thread.start $
                compile_score save_file block_id score (Cmd.perf_events perf)

compile_score :: FilePath -> BlockId -> Lilypond.Score -> Derive.Events -> IO ()
compile_score save_file block_id score events = do
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
