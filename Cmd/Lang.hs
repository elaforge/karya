{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{- | Process a textual language, which may look familiar, to perform UI state
    changes.

    The incoming commands are received via Msg.Socket msgs.

    TODO currently this will reload any updated modules as interpreted.  While
    I want to do this for explicitly named modules (Cmd.Lang.Environ and
    Local/Lang/*.hs), it's just annoying and brittle when applied to the main
    src files.  Is there a way to get ghc to load the objects even if the
    source files are newer?
-}
module Cmd.Lang where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans

import qualified Language.Haskell.Interpreter as Interpreter
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State

import qualified Cmd.Lang.Fast as Fast
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg


type LangCmd = Cmd.CmdT IO String
type InterpreterChan =
    Chan.Chan (MVar.MVar LangCmd, Interpreter.Interpreter LangCmd)

cmd_language :: InterpreterChan -> [FilePath] -> Msg.Msg -> Cmd.CmdIO
cmd_language interpreter_chan lang_dirs msg = do
    (response_hdl, text) <- case msg of
        Msg.Socket hdl s -> return (hdl, s)
        _ -> Cmd.abort
    Log.notice $ "got lang: " ++ show text
    ui_state <- State.get
    cmd_state <- Cmd.get_state
    local_modules <- fmap concat (mapM get_local_modules lang_dirs)

    cmd <- case Fast.fast_interpret text of
        Just cmd -> return cmd
        Nothing -> Trans.liftIO $ send interpreter_chan
            (interpret local_modules ui_state cmd_state text)
    response <- cmd -- TODO what if cmd aborts?  won't I skip the response?
    Trans.liftIO $ catch_io_errors $ do
        unless (null response) $
            IO.hPutStrLn response_hdl response
        IO.hClose response_hdl
    return $ if response == Fast.magic_quit_string then Cmd.Quit else Cmd.Done
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) -> do
        Log.warn $ "caught exception from socket write: " ++ show exc

-- * implementation

send :: InterpreterChan -> Interpreter.Interpreter LangCmd -> IO LangCmd
send chan cmd = do
    return_mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (return_mvar, cmd)
    MVar.takeMVar return_mvar

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

-- ** interpreter

interpreter :: InterpreterChan -> IO ()
interpreter chan = (>> return ()) $ Interpreter.runInterpreter $ do
    Interpreter.set [Interpreter.installedModulesInScope Interpreter.:= False]
    forever $ do
        (return_mvar, cmd) <- Trans.liftIO $ Chan.readChan chan
        result <- cmd `Error.catchError` catch
        Trans.liftIO $ MVar.putMVar return_mvar result
    where
    catch :: Interpreter.InterpreterError -> Interpreter.Interpreter LangCmd
    catch exc = return $ do
        Log.warn ("interpreter error: " ++ show_exc exc)
        return $ "error: " ++ show_exc exc
    show_exc (Interpreter.WontCompile errs) = "won't compile: "
        ++ Seq.join "\n\n" (map Interpreter.errMsg errs)
    show_exc exc = show exc

-- | Interpreted code should be of this type.  However, due to
-- 'mangle_code', it really runs in CmdT IO String
type LangType = State.State -> Cmd.State -> IO (Cmd.CmdVal String)

-- | Interpret the given string inside a CmdT IO monad, and return
-- the resulting CmdT.
--
-- Since I got errors trying to have the type of the code be CmdT directly
-- ("error loading interface for Cmd"), I do a workaround where I have it
-- return a function of type 'LangType' instead.  Cmd.Lang.Environ contains
-- a 'run' function to run that in CmdT, and then this function packages it
-- back up in a CmdT again and returns it.  It's a little roundabout but it
-- seems to work.
--
-- TODO figure out what the original error means, and if I can get around it
interpret :: [Interpreter.ModuleName] -> State.State -> Cmd.State -> String
    -> Interpreter.Interpreter (Cmd.CmdT IO String)
interpret local_mods ui_state cmd_state text = do
    Interpreter.loadModules $ ["Cmd.Lang.Environ"] ++ local_mods
    Interpreter.setTopLevelModules ["Cmd.Lang.Environ"]
    Interpreter.setImports $ ["Prelude"] ++ local_mods

    cmd_func <- Interpreter.interpret (mangle_code text)
        (Interpreter.as :: LangType)
    (cmd_state2, midi, logs, ui_res) <- Trans.liftIO $
        cmd_func ui_state cmd_state
    return (merge_cmd_state cmd_state2 midi logs ui_res)

-- | Create a CmdT that merges the given state into itself.
merge_cmd_state cmd_state midi logs ui_res = do
    Cmd.modify_state (const cmd_state)
    mapM_ Log.write logs
    mapM_ (uncurry Cmd.midi) midi
    case ui_res of
        Left (State.StateError err) -> return $ "error: " ++ err
        Left State.Abort -> Cmd.abort
        Right (response, ui_state2, updates) -> do
            -- I trust that they modified the state through the State
            -- ops, which means the updates should reflect any track
            -- changes.
            State.put ui_state2
            mapM_ State.update updates
            return response

-- | Automatically put the input code into CmdT by putting it in
-- Cmd.Lang.Environ.run.
mangle_code :: String -> String
mangle_code text = Seq.strip $ "run $ do\n" ++ indent text
    where indent = unlines . map ("    "++) . lines
