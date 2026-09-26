-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ReplGhc_test where
import qualified Control.Concurrent as Concurrent

import qualified Util.Test.Testing as Testing
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ReplGhc as ReplGhc

import qualified App.ReplProtocol as ReplProtocol
import Global


meta = Testing.moduleMeta { Testing.tags = [Testing.Interactive] }

-- Fiddle around with the REPL by hand.
test_repl_ghc :: IO ()
test_repl_ghc = do
    session <- ReplGhc.makeSession
    Concurrent.forkIO $ ReplGhc.interpreter session
    go session
    where
    go session = do
        expr <- getLine
        if expr == "quit" then pure () else do
            cmd <- ReplGhc.interpret session (txt expr)
            result <- runIo cmd
            case result of
                Left err -> putStrLn $ "---> err: " ++ err
                Right val -> putStrLn $ "---> val: " ++ untxt val
            go session

runIo :: Cmd.CmdT IO ReplProtocol.CmdResult -> IO (Either String Text)
runIo cmd = do
    (_cstate, _midi, _logs, result) <-
        Cmd.run (ReplProtocol.raw "") uiState cmdState cmd
    pure $ case result of
        Left err -> Left (prettys err)
        Right (cmdResult, _ustate, _updates) ->
            Right $ ReplProtocol.formatResult cmdResult
    where
    uiState = Ui.empty
    cmdState = CmdTest.defaultCmdState
