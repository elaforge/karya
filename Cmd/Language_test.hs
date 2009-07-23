module Cmd.Language_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Language.Haskell.Interpreter as Interpreter

import qualified Util.Log as Log
import Util.Test

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Language as Language


test_interpreter = do
    (chan, th_id) <- Language.start_interpreter_thread
    flip Exception.finally (Concurrent.killThread th_id) $ do
    let state = State.empty
        cmd_state = Cmd.initial_state undefined Map.empty
        f s = do
            cmd <- Language.send chan (Language.interpret [] state cmd_state s)
            (_cstate, _midi, logs, ui_res) <- Cmd.run "abort"
                state cmd_state cmd
            return (extract_val ui_res, map Log.msg_text logs)
    io_equal (f "return $ 4+3") (Right "7\n", [])
    io_equal (f "aoeu") (Right "error: won't compile: Not in scope: `aoeu'",
        ["interpreter error: won't compile: Not in scope: `aoeu'"])
    io_equal (f "Log.warn \"bazfaz\"") (Right "", ["bazfaz"])

extract_val (Right (val, _state, _updates)) = Right val
extract_val (Left err) = Left (show err)

test_hint = do
    val <- Interpreter.runInterpreter $ do
        Interpreter.loadModules $ ["Cmd.LanguageEnviron"]
        Interpreter.setTopLevelModules ["Cmd.LanguageEnviron"]
        Interpreter.setImports $ ["Prelude"]
        val <- Interpreter.interpret "show (State.empty)"
            (Interpreter.as :: String)
        return val
    pprint val
