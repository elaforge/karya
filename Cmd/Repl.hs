-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Process a textual language, which may look familiar, to perform UI state
    changes.

    The incoming commands are received via Msg.Socket msgs.

    TODO currently this will reload any updated modules as interpreted.  While
    I want to do this for explicitly named modules (Cmd.Repl.Environ and
    Local/Repl/ *.hs), it's just annoying and brittle when applied to the main
    src files.  Is there a way to get ghc to load the objects even if the
    source files are newer?  Or turn on -fobject-code?

    https://groupoid.moe/posts/ghc-api-tutorial.html
-}
module Cmd.Repl (
    withSocket
    , Session, makeSession, interpreter, respond
    , acceptMsg
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.Socket as Socket
import qualified System.Directory as Directory
import qualified System.IO as IO

import qualified App.Config as Config
import qualified App.Path as Path
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Repl.Fast as Fast
import qualified Derive.Parse as Parse
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Util.Exceptions as Exceptions
import qualified Util.Log as Log
import qualified Util.Network as Network

import Global

#include "hsconfig.h"
#if defined(INTERPRETER_GHC)
import qualified Cmd.ReplGhc as ReplImpl
#else
import qualified Cmd.ReplStub as ReplImpl
#endif


-- | Acquire a new unix socket, and delete when done.
--
-- This tries to find an unused socket name.  It might seem like overkill, but
-- the previous strategy of unconditionally deleting and recreating the single
-- socket meant that accidentally starting the app twice in the same directory
-- would make the first one unreachable.
withSocket :: (Socket.Socket -> IO a) -> IO a
withSocket app = do
    (fname, socket) <- trySocket $
        Config.replSocketName
        : [Config.replSocketName <> "." <> show n | n <- [1..4]]
    app socket `Exception.finally`
        Exceptions.ignoreEnoent (Directory.removeFile fname)
    where
    -- Let the exception through on the last try.
    trySocket [fname] = (fname,) <$> Network.listenUnix fname
    trySocket (fname : fnames) =
        Exceptions.ignoreIOError (Network.listenUnix fname) >>= \case
            Nothing -> trySocket fnames
            Just socket -> pure (fname, socket)
    trySocket [] = errorIO "no socket files?"

-- | This is the persistent interpreter session which is stored in the global
-- state.
type Session = ReplImpl.Session

makeSession :: IO Session
makeSession = ReplImpl.makeSession

-- | This is the interpreter thread, which should be started when the app
-- starts.  It's in a separate thread so it can run in its own monad.
interpreter :: Session -> IO ()
interpreter = ReplImpl.interpreter

respond :: Session -> Msg.Msg -> Cmd.CmdT IO Cmd.Status
respond session msg = do
    (responseHdl, query) <- case msg of
        Msg.Socket hdl s -> pure (hdl, s)
        _ -> Cmd.abort
    (mbResponse, status) <- case query of
        ReplProtocol.QSaveFile -> do
            saveFile <- fmap (nameOf . snd) <$> Cmd.gets Cmd.stateSaveFile
            pure (Just $ ReplProtocol.RSaveFile saveFile, Cmd.Done)
        ReplProtocol.QCommand expr -> command session (Text.strip expr)
        ReplProtocol.QCompletion prefix -> do
            words <- liftIO $ ReplImpl.complete session prefix
            pure (Just $ ReplProtocol.RCompletion words, Cmd.Done)
        ReplProtocol.QNotify notify -> do
            case notify of
                ReplProtocol.NEditorOpened -> Cmd.modify $ \st -> st
                    { Cmd.stateSaved = (Cmd.stateSaved st)
                        { Cmd._editorOpen = True }
                    }
                ReplProtocol.NEditorClosed -> Cmd.modify $ \st -> st
                    { Cmd.stateSaved = (Cmd.stateSaved st)
                        { Cmd._editorOpen = False }
                    }
            pure (Nothing, Cmd.Done)
    liftIO $ Exception.handle warnIoErrors $ do
        whenJust mbResponse $ ReplProtocol.seqSend responseHdl
        IO.hClose responseHdl
    pure status
    where
    nameOf (Cmd.SaveState fname) = Path.toPath fname
    nameOf (Cmd.SaveRepo fname) = Path.toPath fname
    warnIoErrors (exc :: IOError) =
        Log.warn $ "caught exception from socket write: " <> showt exc

command :: ReplImpl.Session -> Text
    -> Cmd.CmdT IO (Maybe ReplProtocol.Response, Cmd.Status)
command session expr = do
    ns <- Ui.getNamespace
    expr <- Cmd.requireRight ("expand_macros: "<>) $ expandMacros ns expr
    Log.debug $ "repl input: " <> showt expr
    cmd <- case Fast.fastInterpret (untxt expr) of
        Just cmd -> pure cmd
        Nothing -> liftIO $ ReplImpl.interpret session expr
    (response, status) <- runCmdio $ Cmd.name ("repl: " <> expr) cmd
    pure (Just $ ReplProtocol.RCommand response, status)

-- | Replace \@some-id with @(makeId ns \"some-id\")@
expandMacros :: Id.Namespace -> Text -> Either Text Text
expandMacros namespace expr = Parse.expandMacros replace expr
    where
    replace ident = "(makeId " <> showt (Id.unNamespace namespace) <> " "
        <> showt ident <> ")"

-- | Run the Cmd under an IO exception handler.
runCmdio :: Cmd.CmdT IO ReplProtocol.CmdResult
    -> Cmd.CmdT IO (ReplProtocol.CmdResult, Cmd.Status)
runCmdio cmd = do
    uiState <- Ui.get
    cmdState <- Cmd.get
    runResult <- liftIO $ Exception.try $ do
        (cmdState, midi, logs, cmdResult) <-
            Cmd.run (ReplProtocol.raw "<aborted>") uiState
                (cmdState { Cmd.stateReplStatus = Cmd.Done }) cmd
        case cmdResult of
            Left _ -> pure ()
            -- Try to force out any async exceptions.  UI state may also have
            -- some, but I try to force those out in Ui.State functions.
            -- Otherwise, if an error gets out of this try block it can kill
            -- the responder when serializes the value for the socket.
            Right (val, _state, _updates) -> val `DeepSeq.deepseq` pure ()
        mapM_ Log.write logs
        pure (cmdState, midi, cmdResult, logs)
    case runResult of
        Left (exc :: Exception.SomeException) -> pure
            (ReplProtocol.raw $ "IO exception: " <> showt exc, Cmd.Done)
        Right (cmdState, thru, cmdResult, logs) -> case cmdResult of
            Left err -> pure
                (ReplProtocol.raw $ "State error: " <> pretty err, Cmd.Done)
            Right (ReplProtocol.CmdResult response evalLogs, uiState,
                    damage) -> do
                mapM_ Cmd.writeThru thru
                Cmd.put $ cmdState { Cmd.stateReplStatus = Cmd.Continue }
                -- Should be safe, because I'm writing the damage.
                Ui.unsafePut uiState
                Ui.damage damage
                mapM_ Log.write evalLogs
                pure
                    ( ReplProtocol.CmdResult response (evalLogs ++ logs)
                    , Cmd.stateReplStatus cmdState
                    )

-- | Block and read a single message from the REPL client, and return it
-- along with a response handle.  The Query should eventually make it's way to
-- 'respond'.
acceptMsg :: Socket.Socket -> IO (Maybe (IO.Handle, ReplProtocol.Query))
acceptMsg socket = Exception.handle handle $ do
    (socket, _peer) <- Socket.accept socket
    hdl <- Socket.socketToHandle socket IO.ReadWriteMode
    msg <- ReplProtocol.seqReceive hdl
    pure $ Just (hdl, msg)
    where
    handle (exc :: IOError) = do
        Log.warn $ "caught exception from socket read: " <> showt exc
        pure Nothing
