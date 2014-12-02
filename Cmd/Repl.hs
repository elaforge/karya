-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{-# LANGUAGE CPP #-}
{- | Process a textual language, which may look familiar, to perform UI state
    changes.

    The incoming commands are received via Msg.Socket msgs.

    TODO currently this will reload any updated modules as interpreted.  While
    I want to do this for explicitly named modules (Cmd.Repl.Environ and
    Local/Repl/ *.hs), it's just annoying and brittle when applied to the main
    src files.  Is there a way to get ghc to load the objects even if the
    source files are newer?
-}
module Cmd.Repl (
    Session, make_session, interpreter, repl
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString.Char8

import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Ui.Id as Id
import qualified Cmd.Repl.Fast as Fast
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

#include "hsconfig.h"
#if defined(INTERPRETER_GHC)
import qualified Cmd.ReplGhc as ReplImpl
#else
import qualified Cmd.ReplStub as ReplImpl
#endif

import qualified Derive.Parse as Parse
import qualified App.ReplUtil as ReplUtil
import Global


-- | This is the persistent interpreter session which is stored in the global
-- state.
type Session = ReplImpl.Session

make_session :: IO Session
make_session = ReplImpl.make_session

-- | This is the interpreter thread, which should be started when the app
-- starts.  It's in a separate thread so it can run in its own monad.
interpreter :: Session -> IO ()
interpreter = ReplImpl.interpreter

repl :: Session -> [FilePath] -> Msg.Msg -> Cmd.CmdIO
repl session repl_dirs msg = do
    (response_hdl, text) <- case msg of
        Msg.Socket hdl s -> return (hdl, s)
        _ -> Cmd.abort
    ns <- State.get_namespace
    text <- Cmd.require_right ("expand_macros: "<>) $ expand_macros ns text
    Log.debug $ "repl input: " <> showt text
    local_modules <- fmap concat (mapM get_local_modules repl_dirs)

    cmd <- case Fast.fast_interpret (untxt text) of
        Just cmd -> return cmd
        Nothing -> liftIO $ ReplImpl.interpret session local_modules text
    (response, status) <- run_cmdio $ Cmd.name ("repl: " <> text) cmd
    liftIO $ catch_io_errors $ do
        ByteString.Char8.hPutStrLn response_hdl $
            ReplUtil.encode_response response
        IO.hClose response_hdl
    return status
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) ->
        Log.warn $ "caught exception from socket write: " <> showt exc

-- | Replace \@some-id with @(make_id ns \"some-id\")@
expand_macros :: Id.Namespace -> Text -> Either String Text
expand_macros namespace expr = Parse.expand_macros replace expr
    where
    replace ident = "(make_id " <> showt (Id.un_namespace namespace) <> " "
        <> showt ident <> ")"

-- | Run the Cmd under an IO exception handler.
run_cmdio :: Cmd.CmdT IO ReplUtil.Response
    -> Cmd.CmdT IO (ReplUtil.Response, Cmd.Status)
run_cmdio cmd = do
    ui_state <- State.get
    cmd_state <- Cmd.get
    result <- liftIO $ Exception.try $ do
        let aborted = ReplUtil.raw "<aborted>"
        (cmd_state, midi, logs, result) <- Cmd.run aborted ui_state
            (cmd_state { Cmd.state_repl_status = Cmd.Done }) cmd
        mapM_ Log.write logs
        case result of
            Left _ -> return ()
            -- Try to force out any async exceptions.
            Right (val, _, _) -> val `DeepSeq.deepseq` return ()
        return (cmd_state, midi, result)
    case result of
        Left (exc :: Exception.SomeException) -> return
            (ReplUtil.raw $ "IO exception: " <> showt exc, Cmd.Done)
        Right (cmd_state, midi, result) -> case result of
            Left err -> return
                (ReplUtil.raw $ "State error: " <> prettyt err, Cmd.Done)
            Right (val, ui_state, updates) -> do
                mapM_ Cmd.write_midi midi
                Cmd.put $ cmd_state { Cmd.state_repl_status = Cmd.Continue }
                -- Should be safe, because I'm writing the updates.
                State.unsafe_put ui_state
                mapM_ State.update updates
                return (val, Cmd.state_repl_status cmd_state)

get_local_modules :: FilePath -> Cmd.CmdT IO [String]
get_local_modules repl_dir = do
    fns <- liftIO $ Directory.getDirectoryContents repl_dir
        `Exception.catch` \(exc :: IOError) -> do
            Log.warn $ "error reading local repl dir: " <> showt exc
            return []
    let mod_fns = map (repl_dir </>) (filter is_hs fns)
    mod_fns <- liftIO $ filterM Directory.doesFileExist mod_fns
    -- Turn /s into dots to get a module name.  It's kind of a hack.
    return $ map (Seq.replace (FilePath.pathSeparator:"") "."
        . FilePath.dropExtension . FilePath.normalise) mod_fns

is_hs :: FilePath -> Bool
is_hs fn = take 1 fn /= "." && FilePath.takeExtension fn == ".hs"
