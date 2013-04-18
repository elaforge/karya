module Cmd.ReplGhc_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import qualified Util.Pretty as Pretty
import Util.Test
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ReplGhc as ReplGhc


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
        Chan.writeChan chan (line, Id.unsafe_namespace "ns", mvar)
        cmd <- MVar.takeMVar mvar
        result <- run_io "" cmd
        case result of
            Left err -> putStrLn $ "---> err: " ++ err
            Right val -> putStrLn $ "---> val: " ++ val
        go chan

run_io :: a -> Cmd.CmdT IO a -> IO (Either String a)
run_io deflt cmd = do
    (_cstate, _midi, _logs, result) <- Cmd.run deflt ui_state cmd_state cmd
    return $ case result of
        Left err -> Left (Pretty.pretty err)
        Right (val, _ustate, _updates) -> Right val
    where
    ui_state = State.empty
    cmd_state = CmdTest.default_cmd_state
