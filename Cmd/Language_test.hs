module Cmd.Language_test where
import qualified Control.Exception as Exception

import qualified Util.Log as Log
import qualified Language.Haskell.Interpreter.GHC as GHC

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Language as Language


test_interpret :: IO ()
test_interpret = flip Exception.catchDyn catch_interpreter_error $ do
    session <- GHC.newSession
    -- let text = "Log.error \"hi\""
    -- let text = "\\st -> show (f 10)"
    text <- readFile "test_code"
    let ui_state = State.empty
        cmd_state = Cmd.empty_state
    cmd <- GHC.withSession session
        (Language.interpret ui_state cmd_state text)
    (cstate, midi, logs, ui_res) <- Cmd.run "" ui_state cmd_state cmd
    putStrLn (concat (map Log.msg_text logs))
    return ()

{-
\st -> show $ State.run_state st $ do
    ruler <- State.create_ruler "r1"
        (TestSetup.ruler [TestSetup.marklist 64 16, cues_marklist])
    return ()
-}

catch_interpreter_error :: GHC.InterpreterError -> IO ()
catch_interpreter_error exc = putStrLn $ "ow: " ++ show exc
