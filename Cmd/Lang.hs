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
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans

import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State

import qualified Cmd.Lang.Fast as Fast
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

#ifdef INTERPRETER
import qualified Cmd.LangHint as LangImpl
#else
import qualified Cmd.LangStub as LangImpl
#endif


-- | This is the persistent interpreter session which is stored in the global
-- state.
type Session = LangImpl.Session

make_session :: IO Session
make_session = LangImpl.make_session

-- | This is the interpreter thread, which should be started when the app
-- starts.  It has to be in a separate thread because hint insists all
-- interpretation be run in a single monad and it's too much bother to try to
-- put CmdT into that monad.  So I run it in a separate thread and send
-- requests over a channel.
interpreter :: Session -> IO ()
interpreter = LangImpl.interpreter

cmd_language :: Session -> [FilePath] -> Msg.Msg -> Cmd.CmdIO
cmd_language session lang_dirs msg = do
    (response_hdl, text) <- case msg of
        Msg.Socket hdl s -> return (hdl, s)
        _ -> Cmd.abort
    Log.notice $ "got lang: " ++ show text
    ui_state <- State.get
    cmd_state <- Cmd.get_state
    local_modules <- fmap concat (mapM get_local_modules lang_dirs)

    cmd <- case Fast.fast_interpret text of
        Just cmd -> return $ cmd `Error.catchError` \err -> case err of
            State.StateError err -> do
                Log.warn $ "state error in lang cmd: " ++ err
                return $ "error: " ++ err
            State.Abort -> return "aborted"
        -- 'interpret' catches errors in 'merge_cmd_state'
        Nothing -> Trans.liftIO $ LangImpl.interpret session
            local_modules ui_state cmd_state text
    response <- cmd
    Trans.liftIO $ catch_io_errors $ do
        unless (null response) $
            IO.hPutStrLn response_hdl response
        IO.hClose response_hdl
    return $ if response == Fast.magic_quit_string then Cmd.Quit else Cmd.Done
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) -> do
        Log.warn $ "caught exception from socket write: " ++ show exc

get_local_modules :: FilePath -> Cmd.CmdT IO [String]
get_local_modules lang_dir = do
    fns <- Trans.liftIO $ Directory.getDirectoryContents lang_dir
        `Exception.catch` \(exc :: IOError) -> do
            Log.warn $ "error reading local lang dir: " ++ show exc
            return []
    let mod_fns = map (lang_dir </>) (filter is_hs fns)
    mod_fns <- Trans.liftIO $ filterM Directory.doesFileExist mod_fns
    -- Turn /s into dots to get a module name.  It's kind of a hack.
    return $ map (Seq.replace (FilePath.pathSeparator:"") "."
        . FilePath.dropExtension . FilePath.normalise) mod_fns

is_hs :: FilePath -> Bool
is_hs fn = take 1 fn /= "." && FilePath.takeExtension fn == ".hs"
