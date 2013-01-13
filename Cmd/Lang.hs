{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{-# LANGUAGE CPP #-}
{- | Process a textual language, which may look familiar, to perform UI state
    changes.

    The incoming commands are received via Msg.Socket msgs.

    TODO currently this will reload any updated modules as interpreted.  While
    I want to do this for explicitly named modules (Cmd.Lang.Environ and
    Local/Lang/ *.hs), it's just annoying and brittle when applied to the main
    src files.  Is there a way to get ghc to load the objects even if the
    source files are newer?
-}
module Cmd.Lang (
    Session, make_session, interpreter, cmd_language
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception

import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Cmd.Lang.Fast as Fast
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

#include "hsconfig.h"
#if defined(INTERPRETER_GHC)
import qualified Cmd.LangGhc as LangImpl
#else
import qualified Cmd.LangStub as LangImpl
#endif


-- | This is the persistent interpreter session which is stored in the global
-- state.
type Session = LangImpl.Session

make_session :: IO Session
make_session = LangImpl.make_session

-- | This is the interpreter thread, which should be started when the app
-- starts.  It's in a separate thread so it can run in its own monad.
interpreter :: Session -> IO ()
interpreter = LangImpl.interpreter

cmd_language :: Session -> [FilePath] -> Msg.Msg -> Cmd.CmdIO
cmd_language session lang_dirs msg = do
    (response_hdl, text) <- case msg of
        Msg.Socket hdl s -> return (hdl, s)
        _ -> Cmd.abort
    Log.debug $ "repl input: " ++ show text
    ui_state <- State.get
    cmd_state <- Cmd.get
    local_modules <- fmap concat (mapM get_local_modules lang_dirs)

    cmd <- case Fast.fast_interpret text of
        Just cmd -> return cmd
        Nothing -> liftIO $
            LangImpl.interpret session local_modules ui_state cmd_state text
    (response, status, success) <- run_cmdio $ Cmd.name ("repl: " ++ text) cmd
    liftIO $ catch_io_errors $ do
        unless (null response) $
            IO.hPutStrLn response_hdl response
        IO.hClose response_hdl
    when success (write_cmd text)
    return status
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) ->
        Log.warn $ "caught exception from socket write: " ++ show exc

-- | Run the Cmd under an IO exception handler.
run_cmdio :: Cmd.CmdT IO String -> Cmd.CmdT IO (String, Cmd.Status, Bool)
run_cmdio cmd = do
    ui_state <- State.get
    cmd_state <- Cmd.get
    result <- liftIO $ Exception.try $ do
        (cmd_state, midi, logs, result) <- Cmd.run "<aborted>" ui_state
            (cmd_state { Cmd.state_repl_status = Cmd.Done }) cmd
        mapM_ Log.write logs
        case result of
            Left _ -> return ()
            -- Try to force out any async exceptions, e.g. 'Ui.Id.unsafe_id'.
            Right (val, _, _) -> val `DeepSeq.deepseq` return ()
        return (cmd_state, midi, result)
    case result of
        Left (exc :: Exception.SomeException) ->
            return ("IO exception: " ++ show exc, Cmd.Done, False)
        Right (cmd_state, midi, result) -> case result of
            Left err ->
                return ("State error: " ++ Pretty.pretty err, Cmd.Done, False)
            Right (val, ui_state, updates) -> do
                mapM_ Cmd.write_midi midi
                Cmd.put $ cmd_state { Cmd.state_repl_status = Cmd.Continue }
                -- Should be safe, because I'm writing the updates.
                State.unsafe_put ui_state
                mapM_ State.update updates
                return (val, Cmd.state_repl_status cmd_state, True)

get_local_modules :: FilePath -> Cmd.CmdT IO [String]
get_local_modules lang_dir = do
    fns <- liftIO $ Directory.getDirectoryContents lang_dir
        `Exception.catch` \(exc :: IOError) -> do
            Log.warn $ "error reading local lang dir: " ++ show exc
            return []
    let mod_fns = map (lang_dir </>) (filter is_hs fns)
    mod_fns <- liftIO $ filterM Directory.doesFileExist mod_fns
    -- Turn /s into dots to get a module name.  It's kind of a hack.
    return $ map (Seq.replace (FilePath.pathSeparator:"") "."
        . FilePath.dropExtension . FilePath.normalise) mod_fns

is_hs :: FilePath -> Bool
is_hs fn = take 1 fn /= "." && FilePath.takeExtension fn == ".hs"

write_cmd :: String -> Cmd.CmdT IO ()
write_cmd text = Cmd.gets Cmd.state_save_dir >>= \x -> case x of
    Nothing -> return ()
    Just dir ->
        liftIO $ void $ File.log_io_error "write_cmd" $
            IO.appendFile (FilePath.combine dir "repl") (text ++ "\n")
