-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ReplGhc_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import Util.Test
import qualified Ui.State as State
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
    sess@(ReplGhc.Session chan) <- ReplGhc.make_session
    Concurrent.forkIO $ ReplGhc.interpreter sess
    go chan
    where
    go chan = do
        line <- getLine
        if line == "quit" then return () else do
        mvar <- MVar.newEmptyMVar
        Chan.writeChan chan (txt line, mvar)
        cmd <- MVar.takeMVar mvar
        result <- run_io cmd
        case result of
            Left err -> putStrLn $ "---> err: " ++ err
            Right val -> putStrLn $ "---> val: " ++ untxt val
        go chan

run_io :: Cmd.CmdT IO ReplProtocol.CmdResult -> IO (Either String Text)
run_io cmd = do
    (_cstate, _midi, _logs, result) <-
        Cmd.run (ReplProtocol.raw "") ui_state cmd_state cmd
    return $ case result of
        Left err -> Left (prettys err)
        Right (cmd_result, _ustate, _updates) ->
            Right $ ReplProtocol.format_result cmd_result
    where
    ui_state = State.empty
    cmd_state = CmdTest.default_cmd_state
