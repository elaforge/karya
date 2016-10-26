-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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
    with_socket
    , Session, make_session, interpreter, repl
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Network

import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified Util.File as File
import qualified Util.Log as Log

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
import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import Global


-- | Acquire a new unix socket, and delete when done.
--
-- This tries to find an unused socket name.  It might seem like overkill, but
-- the previous strategy of unconditionally deleting and recreating the single
-- socket meant that accidentally starting the app twice in the same directory
-- would make the first one unreachable.
with_socket :: (Network.Socket -> IO a) -> IO a
with_socket app = do
    (fname, socket) <- try_socket
        (Config.repl_port : [Config.repl_port <> "." <> show n | n <- [1..4]])
    app socket `Exception.finally`
        File.ignoreEnoent (Directory.removeFile fname)
    where
    -- Let the exception through on the last try.
    try_socket [fname] = (fname,) <$> listen fname
    try_socket (fname : fnames) = File.ignoreIOError (listen fname) >>= \case
        Nothing -> try_socket fnames
        Just socket -> return (fname, socket)
    try_socket []= errorIO "no socket files?"
    listen = Network.listenOn . Network.UnixSocket

-- | This is the persistent interpreter session which is stored in the global
-- state.
type Session = ReplImpl.Session

make_session :: IO Session
make_session = ReplImpl.make_session

-- | This is the interpreter thread, which should be started when the app
-- starts.  It's in a separate thread so it can run in its own monad.
interpreter :: Session -> IO ()
interpreter = ReplImpl.interpreter

repl :: Session -> Msg.Msg -> Cmd.CmdT IO Cmd.Status
repl session msg = do
    (response_hdl, query) <- case msg of
        Msg.Socket hdl s -> return (hdl, s)
        _ -> Cmd.abort
    case query of
        ReplProtocol.QCommand cmd -> command session response_hdl cmd
        ReplProtocol.QSaveFile -> do
            save_file <- Cmd.gets Cmd.state_save_file
            liftIO $ ReplProtocol.server_send response_hdl $
                ReplProtocol.RSaveFile $ name_of . snd <$> save_file
            return Cmd.Done
    where
    name_of (Cmd.SaveState fname) = fname
    name_of (Cmd.SaveRepo fname) = fname

command :: ReplImpl.Session -> IO.Handle -> Text -> Cmd.CmdT IO Cmd.Status
command session response_hdl cmd_text = do
    ns <- State.get_namespace
    cmd_text <- Cmd.require_right ("expand_macros: "<>) $
        expand_macros ns cmd_text
    Log.debug $ "repl input: " <> showt cmd_text

    cmd <- case Fast.fast_interpret (untxt cmd_text) of
        Just cmd -> return cmd
        Nothing -> liftIO $ ReplImpl.interpret session cmd_text
    (response, status) <- run_cmdio $ Cmd.name ("repl: " <> cmd_text) cmd
    liftIO $ catch_io_errors $ do
        ReplProtocol.server_send response_hdl (ReplProtocol.RCommand response)
        IO.hClose response_hdl
    return status
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) ->
        Log.warn $ "caught exception from socket write: " <> showt exc

-- | Replace \@some-id with @(make_id ns \"some-id\")@
expand_macros :: Id.Namespace -> Text -> Either Text Text
expand_macros namespace expr = Parse.expand_macros replace expr
    where
    replace ident = "(make_id " <> showt (Id.un_namespace namespace) <> " "
        <> showt ident <> ")"

-- | Run the Cmd under an IO exception handler.
run_cmdio :: Cmd.CmdT IO ReplProtocol.CmdResult
    -> Cmd.CmdT IO (ReplProtocol.CmdResult, Cmd.Status)
run_cmdio cmd = do
    ui_state <- State.get
    cmd_state <- Cmd.get
    run_result <- liftIO $ Exception.try $ do
        (cmd_state, midi, logs, cmd_result) <-
            Cmd.run (ReplProtocol.raw "<aborted>") ui_state
                (cmd_state { Cmd.state_repl_status = Cmd.Done }) cmd
        case cmd_result of
            Left _ -> return ()
            -- Try to force out any async exceptions.  UI state may also have
            -- some, but I try to force those out in Ui.State functions.
            -- Otherwise, if an error gets out of this try block it can kill
            -- the responder when serializes the value for the socket.
            Right (val, _state, _updates) -> val `DeepSeq.deepseq` return ()
        return (cmd_state, midi, cmd_result, logs)
    case run_result of
        Left (exc :: Exception.SomeException) -> return
            (ReplProtocol.raw $ "IO exception: " <> showt exc, Cmd.Done)
        Right (cmd_state, midi, cmd_result, logs) -> case cmd_result of
            Left err -> return
                (ReplProtocol.raw $ "State error: " <> pretty err, Cmd.Done)
            Right (ReplProtocol.CmdResult response eval_logs, ui_state,
                    updates) -> do
                mapM_ Cmd.write_midi midi
                Cmd.put $ cmd_state { Cmd.state_repl_status = Cmd.Continue }
                -- Should be safe, because I'm writing the updates.
                State.unsafe_put ui_state
                mapM_ State.update updates
                return
                    ( ReplProtocol.CmdResult response (eval_logs ++ logs)
                    , Cmd.state_repl_status cmd_state
                    )
