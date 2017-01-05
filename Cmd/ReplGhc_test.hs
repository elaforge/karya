-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ReplGhc_test where
import qualified Control.Concurrent as Concurrent

import Util.Test
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ReplGhc as ReplGhc

import qualified App.ReplProtocol as ReplProtocol
import Global


-- Fiddle around with the REPL by hand.
test_repl_ghc :: IO ()
test_repl_ghc = do
    -- Get generate_run_tests.py to recognize this as interactive.
    -- TODO need a better way
    io_human "Ready?" (return ())
    session <- ReplGhc.make_session
    Concurrent.forkIO $ ReplGhc.interpreter session
    go session
    where
    go session = do
        expr <- getLine
        if expr == "quit" then return () else do
            cmd <- ReplGhc.interpret session (txt expr)
            result <- run_io cmd
            case result of
                Left err -> putStrLn $ "---> err: " ++ err
                Right val -> putStrLn $ "---> val: " ++ untxt val
            go session

run_io :: Cmd.CmdT IO ReplProtocol.CmdResult -> IO (Either String Text)
run_io cmd = do
    (_cstate, _midi, _logs, result) <-
        Cmd.run (ReplProtocol.raw "") ui_state cmd_state cmd
    return $ case result of
        Left err -> Left (prettys err)
        Right (cmd_result, _ustate, _updates) ->
            Right $ ReplProtocol.format_result cmd_result
    where
    ui_state = Ui.empty
    cmd_state = CmdTest.default_cmd_state
