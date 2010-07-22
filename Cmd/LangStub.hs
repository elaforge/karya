-- | A stub module that doesn't do a good job at interpreting haskell but does
-- a great job at linking quickly.
module Cmd.LangStub (
    Session, make_session
    , interpreter, interpret
) where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd

type Session = ()

make_session :: IO Session
make_session = return ()

interpreter :: Session -> IO ()
interpreter () = forever $ Concurrent.threadDelay (10 * 1000000)

interpret :: Session -> [String] -> State.State
    -> Cmd.State -> String -> IO (Cmd.CmdT IO String)
interpret () _ _ _ text = return (return $ "stub not running: " ++ show text)
