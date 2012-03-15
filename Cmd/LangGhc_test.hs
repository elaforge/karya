module Cmd.LangGhc_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import qualified Util.Pretty as Pretty
import Util.Test
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.LangGhc as LangGhc


-- Fiddle around with the REPL by hand.
test_lang_ghc :: IO ()
test_lang_ghc = do
    -- Get generate_run_tests.py to recognize this as interactive.
    -- TODO need a better way
    io_human "Ready?" (return ())
    sess@(LangGhc.Session chan) <- LangGhc.make_session
    Concurrent.forkIO $ LangGhc.interpreter sess
    go chan
    where
    go chan = do
        line <- getLine
        if line == "quit" then return () else do
        mvar <- MVar.newEmptyMVar
        Chan.writeChan chan (line, Id.namespace "ns", mvar)
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
