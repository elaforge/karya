{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
module Cmd.LangHint (
    Session, make_session
    , interpreter, interpret
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans

import qualified Language.Haskell.Interpreter as Interpreter

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd


type LangCmd = Cmd.CmdT IO String
type Session = Chan.Chan (MVar.MVar LangCmd, Interpreter.Interpreter LangCmd)

make_session :: IO Session
make_session = Chan.newChan

interpret :: Session -> [Interpreter.ModuleName] -> State.State
    -> Cmd.State -> String -> IO LangCmd
interpret session_chan local_modules ui_state cmd_state text =
    send session_chan (run local_modules ui_state cmd_state text)

-- * implementation

send :: Session -> Interpreter.Interpreter LangCmd -> IO LangCmd
send chan cmd = do
    return_mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (return_mvar, cmd)
    MVar.takeMVar return_mvar

-- ** interpreter

interpreter :: Session -> IO ()
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
run :: [Interpreter.ModuleName] -> State.State -> Cmd.State -> String
    -> Interpreter.Interpreter (Cmd.CmdT IO String)
run local_mods ui_state cmd_state text = do
    Interpreter.loadModules $ "Cmd.Lang.Environ" : local_mods
    Interpreter.setTopLevelModules ["Cmd.Lang.Environ"]
    Interpreter.setImports $ "Prelude" : local_mods

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
        Left (State.StateError err) -> do
            Log.warn $ "state error in lang cmd: " ++ err
            return $ "error: " ++ err
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
